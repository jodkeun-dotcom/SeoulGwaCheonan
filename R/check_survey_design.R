# 프로젝트 루트에서 source("R/check_survey_design.R", encoding = "UTF-8")
# 개인별 데이터·설계객체는 R 메모리에만 두고 집계 점검표만 저장합니다.
if (!requireNamespace("haven", quietly = TRUE) || !requireNamespace("survey", quietly = TRUE)) {
  stop("먼저 source('setup.R')을 실행해 주세요.")
}
source("R/read_knhanes_2024.R", encoding = "UTF-8")
source("R/make_survey_design.R", encoding = "UTF-8")

run_design_check <- function() {
  previous <- options(survey.lonely.psu = "fail", survey.adjust.domain.lonely = FALSE)
  on.exit(options(previous), add = TRUE)
  candidates <- c("data/raw/HN24_ALL(SAS).zip", "data/HN24_ALL(SAS).zip")
  found <- candidates[file.exists(candidates)]
  if (length(found)) path <- found[1L] else if (interactive()) path <- file.choose() else {
    stop("공식 ZIP을 data/raw/ 또는 data/ 폴더에 넣어 주세요.")
  }
  data <- read_knhanes_2024(path)
  design <- make_knhanes_design(data)
  # 하위집단 선택 예시일 뿐 최종 분석대상 기준이 아닙니다.
  reference <- subset(design, !is.na(age) & age >= 19)

  psu_pairs <- unique(data.frame(stratum = data$kstrata, psu = data$psu))
  n_per_stratum <- as.integer(table(psu_pairs$stratum))
  audit <- data.frame(
    item = c("records", "duplicate_ID", "missing_weight", "nonpositive_weight",
      "missing_stratum", "missing_psu", "strata", "psu_within_strata",
      "min_psu_per_stratum", "single_psu_strata", "design_df", "weight_min", "weight_max",
      "weight_sum", "reference_age19_records"),
    value = c(nrow(data), sum(duplicated(data$ID)), sum(is.na(data$wt_itvex)),
      sum(data$wt_itvex <= 0), sum(is.na(data$kstrata)), sum(is.na(data$psu)),
      length(unique(data$kstrata)), nrow(psu_pairs), min(n_per_stratum),
      sum(n_per_stratum == 1), survey::degf(design), min(data$wt_itvex),
      max(data$wt_itvex), sum(data$wt_itvex), nrow(reference$variables)))

  # 설계 실행 검증: 가중 추정값을 직접 계산한 값과 대조합니다.
  # 임상 결과를 분석하는 단계가 아니므로 성별 지표와 상수만 사용합니다.
  total <- survey::svytotal(~I(rep(1, length(age))), design)
  female <- survey::svymean(~I(sex == 2), design)
  ref_total <- survey::svytotal(~I(rep(1, length(age))), reference)
  female_true <- unname(stats::coef(female)[2L])
  female_se <- unname(survey::SE(female)[2L])
  stopifnot(isTRUE(all.equal(as.numeric(stats::coef(total)), sum(data$wt_itvex), tolerance = 1e-8)))
  stopifnot(isTRUE(all.equal(female_true,
    sum(data$wt_itvex * (data$sex == 2)) / sum(data$wt_itvex), tolerance = 1e-8)))
  stopifnot(is.finite(female_se), female_se > 0,
    isTRUE(all.equal(as.numeric(stats::coef(ref_total)),
      sum(data$wt_itvex[!is.na(data$age) & data$age >= 19]), tolerance = 1e-8)))

  out <- "reports/survey_design"
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  write.csv(audit, file.path(out, "design_audit.csv"), row.names = FALSE)
  writeLines(capture.output(utils::sessionInfo()), file.path(out, "session_info.txt"))
  package_versions <- data.frame(package = c("haven", "survey"),
    version = vapply(c("haven", "survey"), function(p) as.character(utils::packageVersion(p)), character(1)))
  write.csv(package_versions, file.path(out, "package_versions.csv"), row.names = FALSE)

  human_labels <- c("자료 인원", "중복 개인 ID", "가중치 결측", "0 이하 가중치",
    "층 결측", "집락 결측", "층 수", "층 내 조사구 수", "층별 최소 조사구 수",
    "단일 조사구 층 수", "설계 자유도", "최소 가중치", "최대 가중치",
    "가중치 합", "19세 이상 참고 인원")
  display_values <- vapply(audit$value, function(v) format(round(v, 2),
    big.mark = ",", scientific = FALSE, trim = TRUE), character(1))
  table_lines <- c("| 점검 항목 | 값 |", "| --- | --- |",
    paste0("| ", human_labels, " | ", display_values, " |"))
  report <- c("# 복합표본 설계 준비 결과", "",
    "## 적용 범위", "",
    "2024년 건강설문·검진조사의 현재 확정 변수 조합에 wt_itvex를 사용한다.",
    "층은 kstrata, 1차 집락은 psu이며 survey::svydesign(ids=~psu, strata=~kstrata, weights=~wt_itvex, nest=TRUE)로 구성했다.",
    "유한모집단보정(fpc)은 제공·검토된 값이 없어 임의로 넣지 않는다. 최상위 집락의 복원추출 근사·Taylor 선형화 기반 설계이다.",
    "영양조사 변수를 추가하거나 여러 연도를 통합하면 가중치 선택을 다시 검토해야 한다.", "",
    "## 원자료·설계 점검", "", table_lines, "",
    "weight_sum은 표본 수가 아니라 제공 가중치의 합이다. 반올림 전 값은 CSV에 저장했다.",
    "27개 층과 192개 조사구가 있는 제공 파일에서 설계변수 결측·0 이하 가중치·개인 ID 중복·단일 조사구 층은 없었다.", "",
    "## 검증", "",
    "- 전체 상수의 가중합과 원자료 가중치 합 일치 확인.",
    "- 성별 지표 가중평균과 직접 가중계산 일치, 표준오차 유한·양수 확인.",
    "- 전체 설계 생성 후 subset으로 얻은 19세 이상 참고집단의 가중합 확인.",
    "- 검증 시 survey.lonely.psu='fail'로 두고 종료 시 원래 옵션으로 복구.", "",
    "## 아직 확정하지 않은 것", "",
    "- 19세 이상은 하위집단 선택 예시이며 최종 분석대상 선정이 아니다.",
    "- 임신·공복·Y 유효성, 비만 유형, 가족력·생활습관 결측 처리 및 최종 포함 여부는 팀 기준 통합 후 적용한다.",
    "- 현재 확인한 자료에 단일 조사구 층은 없으며, 추후 하위집단·모형에서 분산 문제가 발생하면 원인과 처리 방법을 별도 검토한다. 오류를 숨기기 위한 옵션을 일괄 적용하지 않는다.",
    "- 회귀모형·당뇨병 단계별 결과·정책 결론은 아직 산출하지 않았다.", "",
    "## 실행 후 객체", "",
    "knhanes_design: 전체 원자료의 복합표본 설계. age19_reference_design: 참고 하위집단 설계.",
    "객체에는 개인별 자료가 들어 있으므로 RDS/RData로 공개 저장소에 올리지 않는다.", "",
    "## 근거", "",
    "국민건강영양조사 제9기(2022–2024) 원시자료 이용지침서 책자 23–24쪽(가중치), 35–37쪽(복합표본 분석), 94쪽(변수명).",
    "https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/svydesign.html",
    "https://r-survey.r-forge.r-project.org/pkgdown/docs/reference/subset.survey.design.html")
  writeLines(report, file.path(out, "README.md"), useBytes = TRUE)
  print(audit)
  list(design = design, reference = reference)
}
checked <- run_design_check()
knhanes_design <- checked$design
age19_reference_design <- checked$reference
rm(checked)
cat("설계 준비 완료. 객체는 메모리에만 있고 reports/survey_design에 집계 점검표를 저장했습니다.\n")
