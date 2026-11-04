# 실제 로컬 원시자료 필요: 스냅숏 덮어쓰기 회귀 검증.
snapshots <- file.path("reports/survey_design",
  c("README.md", "design_audit.csv", "package_versions.csv", "session_info.txt"))
stopifnot(all(file.exists(snapshots)))
before <- tools::md5sum(snapshots)
run_env <- new.env(parent = globalenv())
sys.source("setup.R", envir = run_env)
old <- options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)
tryCatch({
  sys.source("R/check_survey_design.R", envir = run_env)
  stopifnot(identical(getOption("survey.lonely.psu"), "adjust"),
    identical(getOption("survey.adjust.domain.lonely"), TRUE))
}, finally = options(old))
stopifnot(identical(before, tools::md5sum(snapshots)))
stopifnot(all(file.exists(file.path("reports/survey_design/local",
  c("README.md", "design_audit.csv", "domain_psu_audit.csv",
    "package_versions.csv", "session_info.txt")))))
stopifnot(exists("age19_reference_example_design", envir = run_env, inherits = FALSE),
  !exists("age19_reference_design", envir = run_env, inherits = FALSE))
cat("통과: 커밋 스냅숏 보존·로컬 산출물 생성·옵션 복구·예시 객체 이름\n")
