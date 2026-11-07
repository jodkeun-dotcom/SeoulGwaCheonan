# 저장소 루트에서 source("R/check_basic_variables.R", encoding = "UTF-8") 실행
# 원자료를 수정하거나 개인별 자료를 저장하지 않고 집계 점검표만 생성합니다.

if (!file.exists("R/read_knhanes_2024.R")) {
  stop("작업 폴더를 범주형_팀플 저장소 루트로 설정해 주세요.")
}
source("R/read_knhanes_2024.R", encoding = "UTF-8")
paths <- c("data/raw/HN24_ALL(SAS).zip", "data/HN24_ALL(SAS).zip")
found <- paths[file.exists(paths)]
if (length(found)) {
  zip_path <- found[1L]
} else if (interactive()) {
  zip_path <- file.choose()
} else {
  stop("공식 ZIP을 data/raw/ 또는 data/ 폴더에 넣어 주세요.")
}
hn24 <- read_knhanes_2024(zip_path)
variables <- c("age", "sex", "ho_incm", "edu")
stopifnot(all(variables %in% names(hn24)))
raw <- lapply(hn24[variables], as.numeric)
valid <- list(age = 1:80, sex = 1:2, ho_incm = 1:4, edu = 1:4)
labels <- list(sex = c("남자", "여자"),
               ho_incm = c("하", "중하", "중상", "상"),
               edu = c("초졸 이하", "중졸", "고졸", "대졸 이상"))
scopes <- list(all = rep(TRUE, nrow(hn24)),
               age_19plus_reference = !is.na(raw$age) & raw$age >= 19)

# 19세 이상은 비교용 기준입니다. 최종 분석대상 확정·제외를 수행하지 않습니다.
audit <- do.call(rbind, lapply(names(scopes), function(scope) {
  idx <- scopes[[scope]]
  do.call(rbind, lapply(variables, function(v) {
    x <- raw[[v]][idx]
    data.frame(scope = scope, variable = v, n_total = length(x),
      n_valid = sum(!is.na(x) & x %in% valid[[v]]),
      n_missing = sum(is.na(x)),
      missing_pct = round(100 * mean(is.na(x)), 2),
      n_invalid = sum(!is.na(x) & !x %in% valid[[v]]))
  }))
}))
if (any(audit$n_invalid > 0)) stop("허용범위 밖 값이 있습니다. 원자료·지침서를 확인하세요.")

frequencies <- do.call(rbind, lapply(names(scopes), function(scope) {
  idx <- scopes[[scope]]
  do.call(rbind, lapply(names(labels), function(v) {
    x <- raw[[v]][idx]
    n <- c(vapply(seq_along(labels[[v]]), function(k) sum(x == k, na.rm = TRUE), integer(1)),
           sum(is.na(x)))
    stopifnot(sum(n) == length(x))
    data.frame(scope = scope, variable = v,
      code = c(as.character(seq_along(labels[[v]])), "NA"),
      category = c(labels[[v]], "결측"), n = n,
      pct_of_scope = round(100 * n / length(x), 2))
  }))
}))

ages <- do.call(rbind, lapply(names(scopes), function(scope) {
  x <- raw$age[scopes[[scope]]]
  data.frame(scope = scope, n = length(x), min = min(x),
    q1 = unname(quantile(x, .25)), median = median(x),
    recorded_mean = round(mean(x), 2), q3 = unname(quantile(x, .75)),
    max_recorded = max(x), n_80plus = sum(x == 80))
}))
dictionary <- data.frame(
  variable = variables,
  description = c("나이", "성별", "가구소득 사분위", "교육수준"),
  coding = c("1~79: 해당 나이; 80: 80세 이상", "1: 남자; 2: 여자",
    "1: 하; 2: 중하; 3: 중상; 4: 상",
    "1: 초졸 이하; 2: 중졸; 3: 고졸; 4: 대졸 이상"),
  planned_use = c("연속형 보정변수; 80 유지, 연령대 민감도 분석", "범주형 보정변수",
    "범주형 보정변수; incm과 구분", "범주형 보정변수"),
  note = c("80은 80세 이상; 제외·임의 대체 없이 유지; docs/age_topcoding.md 참조",
    "이 자료의 성별 코드이며 성정체성 변수로 해석하지 않음",
    "공식 생성 사분위를 사용하며 분석표본에서 다시 4등분하지 않음",
    "졸업은 현 학력, 수료·중퇴·재학·휴학은 이전 학력으로 분류"))

out <- file.path("reports", "basic_variables")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
write.csv(dictionary, file.path(out, "variable_dictionary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(audit, file.path(out, "missingness.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(frequencies, file.path(out, "frequencies.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(ages, file.path(out, "age_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")

md_table <- function(x) {
  x[] <- lapply(x, as.character)
  c(paste0("| ", paste(names(x), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |"),
    apply(x, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |")))
}
report <- c(
  "# 조은 담당: 나이·성별·가구소득 사분위·교육 점검", "",
  "## 범위", "",
  "2024 기본DB를 R로 읽어 네 변수의 정의·코드·범위·결측·비가중 분포를 확인했다.",
  "all은 전체 자료, age_19plus_reference는 19세 이상 참고 집단이다.",
  "19세 기준은 비교용이며 최종 분석대상 확정이 아니다. 임신·공복·Y·비만 유형 등의 제외 기준은 아직 적용하지 않았다.",
  "모든 인원과 비율은 비가중 기술점검 값이며 국민 전체의 비율·최종 분석결과가 아니다.", "",
  "## 변수정의서", "", md_table(dictionary), "",
  "## 결측·허용범위 점검", "", md_table(audit), "",
  "NA는 원자료를 읽었을 때의 결측이며 여기서 결측의 원인을 추정하거나 대체하지 않는다.",
  "값 라벨은 원자료에 모두 포함되어 있지 않아 지침서의 코드 설명과 대조했다.", "",
  "## 범주별 분포", "", md_table(frequencies), "",
  "pct_of_scope는 결측까지 포함한 해당 집단 전체를 분모로 한다. 반올림으로 합계가 100과 조금 다를 수 있다.", "",
  "## 나이 요약", "", md_table(ages), "",
  "recorded_mean은 80세 이상을 80으로 기록한 공개값의 평균이다. 실제 평균 연령으로 해석하지 않는다.", "",
  "## 후속 결정", "",
  "- age는 연속형으로 사용하고 80세 이상은 공개값 80을 유지한다. 탑코딩만으로 제외하거나 실제 연령을 추정하지 않는다.",
  "- 80세 이상을 별도 범주로 둔 연령대 보정 모형을 동일표본에서 비교한다. 세부 기준·해석 한계: [연령 탑코딩 처리 기준](../../docs/age_topcoding.md).",
  "- sex·ho_incm·edu는 주 모형에서 범주형으로 처리하는 방향을 제안한다. 1~4를 자동으로 연속 점수로 넣지 않는다.",
  "- 소득은 ho_incm(가구)이며 incm(개인)으로 바꾸지 않는다.",
  "- 교육 결측을 초졸 이하로, 소득 결측을 하 분위로 바꾸지 않는다.",
  "- 최종 결측 처리와 동일 분석표본 선정은 다른 담당 변수와 통합한 후 결정한다.", "",
  "## 재실행", "", "```r",
  "source(\"R/check_basic_variables.R\", encoding = \"UTF-8\")", "```", "",
  "## 근거", "",
  "제공된 국민건강영양조사 제9기(2022–2024) 원시자료 이용지침서 책자 93–94쪽(기본 변수), 95쪽(소득 사분위 참고) 및 원자료 변수 속성·실측 분포.",
  "로컬 지침서: references/KNHANES_2022_2024_guide.pdf",
  "공식 출처: https://knhanes.kdca.go.kr/knhanes/main.do")
writeLines(report, file.path(out, "README.md"), useBytes = TRUE)
cat("완료: ", out, "\n", sep = "")
print(audit)
