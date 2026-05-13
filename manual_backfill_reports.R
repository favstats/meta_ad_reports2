options(timeout = 600)

# Ensure gh::gh() picks up an auth token regardless of which env var the runner
# populates. Without this we fall back to unauthenticated requests and trip the
# 60 req/hr IP-based GitHub API rate limit partway through the 204-country
# release pre-flight (see issue: rate-limit at country ~61).
gh_token_value <- Sys.getenv("GH_PAT", unset =
  Sys.getenv("GITHUB_PAT", unset =
    Sys.getenv("GH_TOKEN", unset =
      Sys.getenv("GITHUB_TOKEN", unset = ""))))
if (nzchar(gh_token_value)) {
  Sys.setenv(
    GITHUB_PAT = gh_token_value,
    GITHUB_TOKEN = gh_token_value,
    GH_TOKEN = gh_token_value
  )
} else {
  warning("No GitHub token found in env (GH_PAT/GITHUB_PAT/GH_TOKEN/GITHUB_TOKEN); release API calls will be unauthenticated and rate-limited.")
}

pacman::p_load(
  dplyr,
  fs,
  glue,
  httr,
  janitor,
  jsonlite,
  lubridate,
  piggyback,
  purrr,
  readr,
  stringr,
  tibble,
  tidyr,
  vroom
)

source("utils.R")

parse_env_list <- function(name, default) {
  value <- Sys.getenv(name, unset = default)
  value %>%
    str_split("[,\\s]+") %>%
    unlist() %>%
    str_trim() %>%
    discard(~ .x == "")
}

env_or_default <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

default_countries <- function() {
  readRDS("cntry_list.rds") %>%
    pull(iso2) %>%
    str_to_upper() %>%
    unique()
}

requested_countries <- Sys.getenv("REPORT_COUNTRIES", unset = "")
countries <- if (!nzchar(requested_countries) || str_to_upper(requested_countries) %in% c("ALL", "WORLD")) {
  default_countries()
} else {
  parse_env_list("REPORT_COUNTRIES", requested_countries) %>% str_to_upper()
}

time_presets <- parse_env_list("REPORT_TIME_PRESETS", "last_7_days")
start_date <- ymd(env_or_default("REPORT_START_DATE", as.character(today() - days(14))))
end_date <- ymd(env_or_default("REPORT_END_DATE", as.character(today() - days(2))))
max_jobs_env <- Sys.getenv("REPORT_MAX_DOWNLOADS", unset = Sys.getenv("REPORT_MAX_DATES", unset = ""))
max_jobs <- suppressWarnings(as.integer(max_jobs_env))
report_repo <- Sys.getenv("REPORT_REPO", unset = "favstats/meta_ad_reports2")

if (!nzchar(max_jobs_env)) {
  max_jobs <- Inf
}

if (is.na(start_date) || is.na(end_date)) {
  stop("REPORT_START_DATE and REPORT_END_DATE must be parseable YYYY-MM-DD dates.")
}

base_request_file <- Sys.getenv("REPORT_GRAPHQL_BODY", unset = "f74570bcb55e676a1b7b72626d0049f8.txt")
if (!file.exists(base_request_file)) {
  stop("Missing base GraphQL request body: ", base_request_file)
}

base_body <- read_file(base_request_file)
base_query <- httr::parse_url(paste0("https://www.facebook.com/?", base_body))$query
base_query$variables <- NULL

target_tags <- tidyr::expand_grid(country = countries, time_preset = time_presets) %>%
  transmute(tag = paste0(country, "-", time_preset)) %>%
  pull(tag)

ensure_report_releases <- function(repo, tags) {
  parsed_repo <- piggyback:::parse_repo(repo)

  walk(unique(tags), function(tag) {
    release_exists <- tryCatch({
      gh::gh(
        "/repos/:owner/:repo/releases/tags/:tag",
        owner = parsed_repo[[1]],
        repo = parsed_repo[[2]],
        tag = tag
      )
      TRUE
    }, error = function(e) FALSE)

    if (release_exists) {
      return(invisible(TRUE))
    }

    message(glue("Creating missing release {tag}"))
    gh::gh(
      "POST /repos/:owner/:repo/releases",
      owner = parsed_repo[[1]],
      repo = parsed_repo[[2]],
      tag_name = tag,
      name = tag,
      body = "Data release",
      draft = FALSE,
      prerelease = FALSE
    )
  })
}

ensure_report_releases(report_repo, target_tags)
full_repos <- get_full_release(report_repo, tags = target_tags)
latest_dir <- tempfile("meta-report-latest-")
dir_create(latest_dir)
on.exit(unlink(latest_dir, recursive = TRUE, force = TRUE), add = TRUE)

existing_assets <- full_repos %>%
  filter(!is.na(file_name), file_name != "", file_name != "latest.rds") %>%
  mutate(
    report_date = ymd(str_extract(file_name, "\\d{4}-\\d{2}-\\d{2}")),
    ext = str_extract(file_name, "[^.]+$")
  ) %>%
  filter(!is.na(report_date), ext %in% c("rds", "zip")) %>%
  distinct(tag, report_date, ext) %>%
  count(tag, report_date, name = "asset_count")

fetch_report_download_uri <- function(country, report_date, time_preset) {
  body <- base_query
  body$variables <- toJSON(
    list(country = country, reportDS = as.character(report_date), timePreset = time_preset),
    auto_unbox = TRUE
  )
  
  response <- RETRY(
    "POST",
    "https://www.facebook.com/api/graphql/",
    body = body,
    encode = "form",
    user_agent("Mozilla/5.0"),
    times = 4,
    pause_base = 2
  )
  
  if (http_error(response)) {
    warning("GraphQL request failed for ", country, " ", report_date, " ", time_preset)
    return(NA_character_)
  }
  
  parsed <- fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  uri <- parsed$data$ad_library_report$download_uri
  if (is.null(uri) || !nzchar(uri)) {
    return(NA_character_)
  }
  
  uri
}

process_report <- function(country, report_date, time_preset) {
  tag <- paste0(country, "-", time_preset)
  message(glue("Backfilling {tag} {report_date}"))
  
  download_uri <- fetch_report_download_uri(country, report_date, time_preset)
  if (is.na(download_uri)) {
    message(glue("No download URI for {tag} {report_date}"))
    return(invisible(FALSE))
  }
  
  work_dir <- tempfile("meta-report-")
  dir_create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
  
  zip_file <- path(work_dir, paste0(report_date, ".zip"))
  rds_file <- path(work_dir, paste0(report_date, ".rds"))
  
  download.file(download_uri, zip_file, mode = "wb", quiet = TRUE)
  
  extract_dir <- path(work_dir, "extracted")
  dir_create(extract_dir)
  unzip(zip_file, exdir = extract_dir)
  
  extracted_path <- dir(extract_dir, full.names = TRUE, recursive = FALSE) %>%
    keep(~ str_detect(.x, "advert")) %>%
    keep(~ str_detect(.x, country) && str_detect(.x, time_preset))
  
  if (length(extracted_path) == 0) {
    warning("No extracted advertiser file for ", tag, " ", report_date)
    return(invisible(FALSE))
  }
  
  report_data <- vroom(extracted_path[1], show_col_types = FALSE) %>%
    clean_names() %>%
    mutate(date = str_extract(path_file(extracted_path[1]), "\\d{4}-\\d{2}-\\d{2}")) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(path = extracted_path[1], tf = time_preset, cntry = country)
  
  if ("name_disclaimer_amount" %in% names(report_data)) {
    report_data <- report_data %>%
      filter(is.na(name_disclaimer_amount)) %>%
      remove_empty()
  }
  
  if (nrow(report_data) == 0) {
    warning("No rows in extracted report for ", tag, " ", report_date)
    return(invisible(FALSE))
  }
  
  write_rds(report_data, rds_file, compress = "xz")
  
  latest_tag_dir <- path(latest_dir, tag)
  dir_create(latest_tag_dir)
  file.copy(rds_file, path(latest_tag_dir, "latest.rds"), overwrite = TRUE)
  
  pb_upload_file_fr(rds_file, repo = report_repo, tag = tag, releases = full_repos)
  pb_upload_file_fr(zip_file, repo = report_repo, tag = tag, releases = full_repos)
  
  invisible(TRUE)
}

dates <- seq.Date(start_date, end_date, by = "day")
jobs <- tidyr::expand_grid(country = countries, time_preset = time_presets, report_date = dates) %>%
  mutate(tag = paste0(country, "-", time_preset)) %>%
  left_join(existing_assets, by = c("tag", "report_date")) %>%
  filter(is.na(asset_count) | asset_count < 2) %>%
  select(country, time_preset, report_date) %>%
  arrange(time_preset, country, report_date)

if (is.finite(max_jobs)) {
  jobs <- slice_head(jobs, n = max_jobs)
}

message(glue("Queued {nrow(jobs)} missing report downloads across {length(target_tags)} releases."))

if (nrow(jobs) == 0) {
  quit(save = "no", status = 0)
}

walk2(
  seq_len(nrow(jobs)),
  split(jobs, seq_len(nrow(jobs))),
  ~ {
    message(glue("[{.x}/{nrow(jobs)}]"))
    try(process_report(.y$country, .y$report_date, .y$time_preset), silent = FALSE)
    Sys.sleep(runif(1, 0.2, 1.0))
  }
)

latest_repos <- get_full_release(report_repo, tags = target_tags)
dir(latest_dir, full.names = TRUE, recursive = FALSE) %>%
  walk(~ {
    latest_file <- path(.x, "latest.rds")
    if (!file_exists(latest_file)) {
      return(invisible(NULL))
    }
    
    tag <- path_file(.x)
    message(glue("Uploading {tag} latest.rds"))
    pb_upload_file_fr("latest.rds", repo = report_repo, tag = tag, releases = latest_repos, dir = .x)
  })
