# RStudio에서 SeoulGwaCheonan.Rproj를 연 뒤 처음 한 번 실행합니다.
packages <- c("haven", "survey")
missing <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
if (!all(vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)))) {
  stop("패키지 설치를 완료한 뒤 다시 실행해 주세요.")
}
for (path in c("data/raw", "data/processed", "reports/survey_design/local")) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
versions <- data.frame(package = packages,
  version = vapply(packages, function(p) as.character(utils::packageVersion(p)), character(1)))
write.csv(versions, "reports/survey_design/local/package_versions.csv", row.names = FALSE)
writeLines(capture.output(utils::sessionInfo()), "reports/survey_design/local/session_info.txt")
cat("준비 완료. R:", as.character(getRversion()), "\n")
print(versions)
