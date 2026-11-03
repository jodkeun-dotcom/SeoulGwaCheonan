# Run source("R/read_knhanes_2024.R") from the repository root.
# Original and derived individual-level data stay outside Git tracking.

read_knhanes_2024 <- function(zip_path = file.path("data", "raw", "HN24_ALL(SAS).zip")) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Install haven first: install.packages('haven')", call. = FALSE)
  }
  if (!file.exists(zip_path)) {
    stop("ZIP not found. Place your downloaded file in data/raw/ or pass zip_path.",
         call. = FALSE)
  }
  members <- utils::unzip(zip_path, list = TRUE)$Name
  target <- members[tolower(basename(members)) == "hn24_all.sas7bdat"]
  if (length(target) != 1L || grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))", target)) {
    stop("ZIP must contain exactly one safe hn24_all.sas7bdat path.", call. = FALSE)
  }

  extract_dir <- tempfile("knhanes_2024_")
  dir.create(extract_dir)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
  utils::unzip(zip_path, files = target, exdir = extract_dir)
  data <- haven::read_sas(file.path(extract_dir, target))

  # Match names without changing the original variable names or value labels.
  required <- c("ID", "year", "HE_BMI", "HE_wc", "age", "sex",
                "HE_DMfh1", "HE_DMfh2", "HE_DMfh3", "pa_aerobic",
                "BS1_1", "BS3_1", "dr_month", "ho_incm", "edu",
                "wt_itvex", "kstrata", "psu")
  missing <- required[!tolower(required) %in% tolower(names(data))]
  if (length(missing)) {
    stop(paste("Required columns missing:", paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  # The guide's variable table and SAS examples use different spellings.
  stage_candidates <- c("HE_DM_HbA1c", "HE_DM_hb")
  stage <- names(data)[tolower(names(data)) %in% tolower(stage_candidates)]
  if (length(stage) != 1L) {
    stop("Confirm the diabetes-stage variable against the data labels and guide.",
         call. = FALSE)
  }
  year_col <- names(data)[tolower(names(data)) == "year"]
  if (anyNA(data[[year_col]]) || !all(data[[year_col]] == 2024)) {
    stop("Expected only 2024 records.", call. = FALSE)
  }
  message("Loaded 2024 data. Diabetes-stage column: ", stage,
          ". Review labels and missing values before analysis.")
  data
}
