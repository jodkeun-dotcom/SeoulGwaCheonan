# 저장소 루트에서 source("R/check_lifestyle_variables.R", encoding = "UTF-8") 실행
# 원자료를 수정하거나 개인별 자료를 저장하지 않고 집계 점검표만 생성합니다.

if (!file.exists("R/read_knhanes_2024.R")) {
  stop("작업 폴더를 SeoulGwaCheonan 저장소 루트로 설정해 주세요.")
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

# 원자료 변수명 대소문자가 섞여 있어 소문자로 대조합니다.
col <- function(v) {
  hit <- names(hn24)[tolower(names(hn24)) %in% tolower(v)]
  if (length(hit) != 1L) stop("변수를 하나로 찾지 못했습니다: ", paste(v, collapse = ", "))
  as.numeric(hn24[[hit]])
}
variables <- c("HE_DMfh1", "HE_DMfh2", "HE_DMfh3", "HE_fh", "BS1_1", "BS3_1",
               "sm_presnt", "pa_aerobic", "dr_month")
raw <- setNames(lapply(variables, col), variables)
age <- col("age")
base_cols <- list(Y = col(c("HE_DM_HbA1c", "HE_DM_hb")), HE_BMI = col("HE_BMI"), HE_wc = col("HE_wc"))

labels <- list(
  HE_DMfh1 = c("0" = "아니오", "1" = "예", "9" = "모름/무응답"),
  HE_DMfh2 = c("0" = "아니오", "1" = "예", "9" = "모름/무응답"),
  HE_DMfh3 = c("0" = "아니오", "1" = "예", "8" = "비해당(외동)", "9" = "모름/무응답"),
  HE_fh = c("0" = "아니오", "1" = "예", "9" = "모름"),
  BS1_1 = c("1" = "5갑(100개비) 미만", "2" = "5갑(100개비) 이상", "3" = "피운 적 없음",
            "8" = "비해당(소아, 청소년)", "9" = "모름, 무응답"),
  BS3_1 = c("1" = "매일피움", "2" = "가끔피움", "3" = "과거엔 피웠으나 현재 피우지 않음",
            "8" = "비해당(문항1-③⑧)", "9" = "모름, 무응답"),
  sm_presnt = c("0" = "과거 흡연, 비흡연", "1" = "현재흡연"),
  pa_aerobic = c("0" = "미실천", "1" = "실천"),
  dr_month = c("0" = "평생비음주, 최근 1년간 월1회미만 음주", "1" = "최근 1년간 월1회이상 음주"))

# 허용값 검사: 지침서 코드 밖의 값이 있으면 중단합니다.
for (v in variables) {
  bad <- !is.na(raw[[v]]) & !raw[[v]] %in% as.numeric(names(labels[[v]]))
  if (any(bad)) stop(v, "에 허용범위 밖 값이 있습니다. 원자료·지침서를 확인하세요.")
}

# 당뇨병 가족력(부·모·형제자매) 합친 값. 모름은 없음으로 합치지 않고 별도 범주로 둡니다.
fh <- cbind(raw$HE_DMfh1, raw$HE_DMfh2, raw$HE_DMfh3)
dm_fh <- ifelse(rowSums(is.na(fh)) == 3, NA,
         ifelse(rowSums(fh == 1, na.rm = TRUE) > 0, "있음",
         ifelse(rowSums(fh == 9, na.rm = TRUE) > 0, "모름", "없음")))

# 현재흡연은 공식 생성변수 sm_presnt를 그대로 쓰고, 지침서 프로그램으로 다시 계산해 일치를 확인합니다.
bs1 <- raw$BS1_1; bs3 <- raw$BS3_1; sm <- raw$sm_presnt
sm_check <- ifelse((bs1 %in% 1:2 & bs3 %in% 1:3) | bs1 %in% 3,
                   as.numeric(bs1 %in% 2 & bs3 %in% 1:2), NA)
stopifnot(identical(is.na(sm_check), is.na(sm)), all(sm_check == sm, na.rm = TRUE))
# sm_presnt=0 중 과거흡연(5갑 이상 & 현재 안 피움)만 나누고 나머지는 비흡연입니다.
# 5갑 미만인데 현재 매일·가끔 피우는 사람도 지침서 정의에 따라 비흡연입니다.
smk3 <- ifelse(is.na(sm), NA,
        ifelse(sm == 1, "현재흡연",
        ifelse(bs1 %in% 2 & bs3 %in% 3, "과거흡연", "비흡연")))
stopifnot(all(smk3[!is.na(sm) & sm == 0] %in% c("과거흡연", "비흡연")),
          all(bs1[smk3 %in% "비흡연"] %in% c(1, 3)))
n_light_current <- sum(bs1 %in% 1 & bs3 %in% 1:2)

derived <- list(dm_fh = dm_fh, smk3 = smk3)
derived_levels <- list(dm_fh = c("없음", "있음", "모름"), smk3 = c("비흡연", "과거흡연", "현재흡연"))
scopes <- list(all = rep(TRUE, length(age)),
               age_19plus_reference = !is.na(age) & age >= 19)
# 흡연 원문항은 모름(9)과 결측(NA)이 모두 sm_presnt 결측이고, 19세 이상 NA가 5명 미만이라
# 다른 표에서 역산되지 않도록 두 값을 합쳐 표시합니다.
merge_9_na <- c("BS1_1", "BS3_1")

# 19세 이상은 비교용 기준입니다. 최종 분석대상 확정·제외를 수행하지 않습니다.
audit <- do.call(rbind, lapply(names(scopes), function(scope) {
  idx <- scopes[[scope]]
  rbind(
    do.call(rbind, lapply(variables, function(v) {
      x <- raw[[v]][idx]
      merged <- v %in% merge_9_na
      n_missing <- sum(is.na(x)) + if (merged) sum(x %in% 9) else 0L
      data.frame(scope = scope, variable = v, type = "원변수", n_total = length(x),
        n_valid = sum(!is.na(x) & !x %in% c(8, 9)),
        n_not_applicable_8 = sum(x %in% 8), n_unknown_9 = if (merged) NA else sum(x %in% 9),
        n_missing = n_missing, missing_pct = round(100 * n_missing / length(x), 2))
    })),
    do.call(rbind, lapply(names(derived), function(v) {
      x <- derived[[v]][idx]
      data.frame(scope = scope, variable = v, type = "생성변수", n_total = length(x),
        n_valid = sum(!is.na(x)), n_not_applicable_8 = NA, n_unknown_9 = NA,
        n_missing = sum(is.na(x)), missing_pct = round(100 * mean(is.na(x)), 2))
    })))
}))

frequencies <- do.call(rbind, lapply(names(scopes), function(scope) {
  idx <- scopes[[scope]]
  rbind(
    do.call(rbind, lapply(variables, function(v) {
      x <- raw[[v]][idx]
      codes <- c(names(labels[[v]]), "NA")
      cats <- c(unname(labels[[v]]), "결측")
      n <- c(vapply(as.numeric(names(labels[[v]])), function(k) sum(x %in% k), integer(1)),
             sum(is.na(x)))
      if (v %in% merge_9_na) {
        k <- codes %in% c("9", "NA")
        n <- c(n[!k], sum(n[k])); codes <- c(codes[!k], "9/NA")
        cats <- c(cats[!k], "모름, 무응답 또는 결측")
      }
      stopifnot(sum(n) == length(x))
      data.frame(scope = scope, variable = v, code = codes, category = cats, n = n,
        pct_of_scope = round(100 * n / length(x), 2))
    })),
    do.call(rbind, lapply(names(derived), function(v) {
      x <- derived[[v]][idx]
      n <- c(vapply(derived_levels[[v]], function(k) sum(x %in% k), integer(1)), sum(is.na(x)))
      stopifnot(sum(n) == length(x))
      data.frame(scope = scope, variable = v, code = c(derived_levels[[v]], "NA"),
        category = c(derived_levels[[v]], "결측"), n = unname(n),
        pct_of_scope = round(100 * unname(n) / length(x), 2))
    })))
}))

adult <- scopes$age_19plus_reference
na_code <- function(x) ifelse(is.na(x), "NA", as.character(x))
count_by <- function(df) {
  tab <- as.data.frame(table(df, useNA = "no"), stringsAsFactors = FALSE)
  names(tab)[ncol(tab)] <- "n"
  tab[tab$n > 0, ]
}
code_9_na <- function(x) ifelse(is.na(x) | x %in% 9, "9/NA", as.character(x))
smoking_crosstab <- count_by(data.frame(BS1_1 = code_9_na(bs1[adult]), BS3_1 = code_9_na(bs3[adult]),
  sm_presnt = na_code(sm[adult]), smk3 = na_code(smk3[adult])))
smoking_crosstab <- smoking_crosstab[order(smoking_crosstab$BS1_1, smoking_crosstab$BS3_1), ]
# 부·모·형제자매 3원표는 작은 칸이 많고 합계로 역산될 수 있어 2원표 두 개로 나눕니다.
fh_parents_crosstab <- count_by(data.frame(HE_DMfh1 = na_code(fh[adult, 1]), HE_DMfh2 = na_code(fh[adult, 2])))
fh_parents_crosstab <- fh_parents_crosstab[order(fh_parents_crosstab$HE_DMfh1, fh_parents_crosstab$HE_DMfh2), ]
fh_sibling_crosstab <- count_by(data.frame(HE_DMfh3 = na_code(fh[adult, 3]), dm_fh = na_code(dm_fh[adult])))
fh_sibling_crosstab <- fh_sibling_crosstab[order(fh_sibling_crosstab$HE_DMfh3, fh_sibling_crosstab$dm_fh), ]
# 공개 저장소라 교차표의 5명 미만 칸은 "<5"로 가립니다(팀 기준 확정 전 제안).
mask_small <- function(tab) { tab$n <- ifelse(tab$n < 5, "<5", as.character(tab$n)); tab }
smoking_crosstab <- mask_small(smoking_crosstab)
fh_parents_crosstab <- mask_small(fh_parents_crosstab)
fh_sibling_crosstab <- mask_small(fh_sibling_crosstab)

# 결측 겹침: 흡연 결측은 sm_presnt 결측(BS1_1 모름·무응답 포함)입니다.
miss <- data.frame(dm_fh = is.na(dm_fh), pa_aerobic = is.na(raw$pa_aerobic),
                   smk3 = is.na(smk3), dr_month = is.na(raw$dr_month))
overlap <- crossprod(as.matrix(miss[adult, ]))
missing_overlap <- data.frame(variable = rownames(overlap), overlap, row.names = NULL)

base_ok <- adult & Reduce(`&`, lapply(base_cols, function(x) !is.na(x)))
n_miss <- rowSums(miss)
missing_count <- do.call(rbind, lapply(list(age_19plus_reference = adult, y_bmi_wc_complete = base_ok),
  function(idx) data.frame(n_missing_lifestyle = 0:4,
    n = vapply(0:4, function(k) sum(n_miss[idx] == k), integer(1)))))
missing_count <- data.frame(scope = rep(c("age_19plus_reference", "y_bmi_wc_complete"), each = 5),
                            missing_count, row.names = NULL)
additional_exclusion <- data.frame(
  item = c("19세 이상 참고집단", "그중 Y·HE_BMI·HE_wc 모두 있음", "그중 생활습관 4변수 모두 있음",
           "생활습관 변수로 추가 제외", paste0("  ", names(miss), " 결측")),
  n = c(sum(adult), sum(base_ok), sum(base_ok & n_miss == 0), sum(base_ok & n_miss > 0),
        vapply(miss, function(m) sum(base_ok & m), integer(1))))
additional_exclusion$pct_of_base <- c(NA, NA, round(100 * additional_exclusion$n[-(1:2)] / sum(base_ok), 2))

guide_discrepancies <- data.frame(
  item = c("19세 미만 가족력 응답", "HE_fh=0인데 HE_DMfh1=1"),
  scope = c("19세 미만", "age_19plus_reference"),
  n = c(sum(!adult & !is.na(raw$HE_DMfh1)), sum(adult & raw$HE_fh %in% 0 & raw$HE_DMfh1 %in% 1)),
  guide = c("2024년 가족력조사는 19세 이상 대상(책자 6쪽, 15쪽)",
            "HE_fh는 만성질환 의사진단 가족력 여부(책자 175쪽)"),
  handling = c("분석대상이 19세 이상이면 영향 없음. 값은 수정하지 않음",
               "HE_fh를 상위 거름 문항으로 쓰지 않고 HE_DMfh1~3만으로 dm_fh 생성"))

dictionary <- data.frame(
  source_variable = c("HE_DMfh1, HE_DMfh2, HE_DMfh3", "sm_presnt, BS1_1, BS3_1",
                      "pa_aerobic", "dr_month"),
  new_variable = c("dm_fh", "smk3", "pa_aerobic", "dr_month"),
  definition = c("부·모·형제자매 중 당뇨병 의사진단 가족력",
    "현재흡연(sm_presnt=1), 과거흡연(평생 5갑 이상 & 현재 피우지 않음), 비흡연(나머지)",
    "주당 중강도 150분 이상 또는 고강도 75분 이상 또는 고강도 1분=중강도 2분 합산 150분 이상",
    "최근 1년간 월 1회 이상 음주"),
  coding = c("있음: 하나라도 1; 모름: 1 없이 9가 하나 이상; 없음: 0 또는 8(외동)만",
    "현재흡연: sm_presnt=1; 과거흡연: sm_presnt=0 & BS1_1=2 & BS3_1=3; 비흡연: sm_presnt=0 & BS1_1∈{1,3}",
    "0: 미실천; 1: 실천 (원변수 그대로)", "0: 평생비음주·월1회미만; 1: 월1회이상 (원변수 그대로)"),
  missing_rule = c("셋 다 NA이면 결측. 모름은 없음으로 합치지 않음. 8(외동)은 없음",
    "sm_presnt NA(BS1_1 모름·무응답 포함)이면 결측. 5갑 미만·현재 매일/가끔 흡연은 비흡연",
    "5개 영역 중 하나라도 모름·결측이면 공식 산출에서 NA. 0으로 바꾸지 않음",
    "BD1·BD1_11 모름·무응답이면 NA. 0으로 바꾸지 않음"),
  evidence = c("책자 175~176쪽(코드), 6쪽·15쪽(2024년 19세 이상 조사)",
    "책자 41~42쪽 표 18(현재흡연율 정의·프로그램), 156~157쪽(BS1_1·BS3_1 코드), 163쪽(sm_presnt, 분모 19세 이상)",
    "책자 43쪽 표 19(산출 프로그램), 165~169쪽(문항·정의, 분모 19세 이상)",
    "책자 42쪽 표 18(프로그램), 142~144쪽(문항·코드, 분모 12세 이상)"))

# 가족력은 2024년 19세 이상 조사 항목이라 전체 범위 표는 싣지 않습니다.
# 전체와 19세 이상의 차이로 19세 미만 응답의 작은 칸이 역산되기 때문입니다(19세 미만 응답 수는 guide_discrepancies에만 기록).
fh_vars <- c("HE_DMfh1", "HE_DMfh2", "HE_DMfh3", "HE_fh", "dm_fh")
audit <- audit[!(audit$scope == "all" & audit$variable %in% fh_vars), ]
frequencies <- frequencies[!(frequencies$scope == "all" & frequencies$variable %in% fh_vars), ]

# 공개하는 집계표에 1~4명 칸이 남아 있거나 범위 간 차이로 1~4명이 계산되면 중단합니다(가린 칸 역산 방지).
count_cols <- c("n_valid", "n_not_applicable_8", "n_unknown_9", "n_missing")
scope_diff <- function(tab, keys, cols) {
  m <- merge(tab[tab$scope == "all", ], tab[tab$scope == "age_19plus_reference", ], by = keys)
  unlist(lapply(cols, function(k) m[[paste0(k, ".x")]] - m[[paste0(k, ".y")]]))
}
published_counts <- list(audit[, count_cols], frequencies$n, missing_overlap[, -1], missing_count$n,
  additional_exclusion$n, guide_discrepancies$n,
  scope_diff(frequencies, c("variable", "code"), "n"), scope_diff(audit, "variable", count_cols))
small <- unlist(lapply(published_counts, function(x) unlist(x)))
if (any(small %in% 1:4)) stop("공개 집계표에 5명 미만 칸이 있습니다. 범주를 묶거나 가려 주세요.")

out <- file.path("reports", "lifestyle_variables")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
save_csv <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE, fileEncoding = "UTF-8")
save_csv(dictionary, "variable_dictionary.csv")
save_csv(audit, "missingness.csv")
save_csv(frequencies, "frequencies.csv")
save_csv(smoking_crosstab, "crosstab_smoking.csv")
save_csv(fh_parents_crosstab, "crosstab_family_history_parents.csv")
save_csv(fh_sibling_crosstab, "crosstab_family_history_sibling.csv")
unlink(file.path(out, "crosstab_family_history.csv"))
save_csv(missing_overlap, "missing_overlap.csv")
save_csv(missing_count, "missing_count.csv")
save_csv(additional_exclusion, "additional_exclusion.csv")
save_csv(guide_discrepancies, "guide_discrepancies.csv")

md_table <- function(x) {
  x[] <- lapply(x, function(v) ifelse(is.na(v), "", as.character(v)))
  c(paste0("| ", paste(names(x), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |"),
    apply(x, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |")))
}
adult_freq <- frequencies[frequencies$scope == "age_19plus_reference" &
                          frequencies$variable %in% c("dm_fh", "smk3", "pa_aerobic", "dr_month"), ]
report <- c(
  "# 민서 담당: 당뇨병 가족력·흡연·신체활동·음주 점검", "",
  "## 범위", "",
  "2024 기본DB를 R로 읽어 가족력·생활습관 변수의 정의·코드·결측·비해당·비가중 분포를 확인하고 재코딩 규칙을 정했다.",
  "all은 전체 자료, age_19plus_reference는 19세 이상 참고 집단이다.",
  "19세 기준은 비교용이며 최종 분석대상 확정이 아니다. 임신·공복 등 세은 담당 제외 기준은 아직 적용하지 않았다.",
  "모든 인원과 비율은 비가중 기술점검 값이며 국민 전체의 비율·최종 분석결과가 아니다.",
  "원자료는 수정하지 않았고 생성변수(dm_fh, smk3)는 스크립트 안에서만 만든다.", "",
  "## 재코딩표", "", md_table(dictionary), "",
  "## 결측·비해당 처리 규칙", "",
  "- 결측을 0이나 '없음'으로 바꾸지 않는다. 근거 없이 일괄 삭제하지 않는다.",
  "- 가족력 '모름'(9)은 '없음'으로 합치지 않고 별도 범주로 둔다. 형제자매 '비해당(외동)'(8)은 형제자매 가족력이 없다는 뜻이므로 '없음'으로 본다.",
  "- 흡연 BS1_1=9(모름, 무응답)는 sm_presnt가 산출되지 않으므로 결측으로 둔다.",
  "- BS1_1=8(비해당)은 19세 미만에만 있다. BS1_1=3이면 BS3_1=8로 건너뛰기가 정상 처리된다.",
  paste0("- 평생 5갑(100개비) 미만인데 현재 매일·가끔 피우는 19세 이상 ",
         sum(adult & bs1 %in% 1 & bs3 %in% 1:2), "명(전체 ", n_light_current,
         "명)은 지침서 현재흡연율 정의(책자 41~42쪽 표 18)에서 sm_presnt=0이므로 비흡연으로 처리한다."),
  "- pa_aerobic NA에는 5개 활동 영역 중 하나라도 모름·무응답인 사람이 포함된다(책자 43쪽 표 19).",
  "- dr_month는 12세 이상에서 산출된다(책자 144쪽). 19세 미만 값이 있는 것은 정상이다.", "",
  "## 검증", "",
  "- sm_presnt를 지침서 프로그램(책자 42쪽 표 18)으로 BS1_1·BS3_1에서 다시 계산해 전체 자료에서 값과 결측 위치가 모두 일치함을 stopifnot으로 확인했다.",
  "- 모든 원변수가 지침서 코드 범위 안에 있음을 확인했다(벗어나면 스크립트가 중단된다).", "",
  "## 19세 이상 분포(생성변수·사용 변수)", "", md_table(adult_freq), "",
  "## 결측·비해당 점검", "", md_table(audit), "",
  "n_valid는 결측·8·9를 뺀 응답 수다. 생성변수는 8·9가 이미 범주로 정리되어 있다.", "",
  "## 작은 칸 공개 기준", "",
  "공개 저장소라 교차표의 5명 미만 칸은 <5로 표시한다(팀 기준 확정 전 제안).",
  "다른 표의 합계로 가린 칸이 역산되지 않도록 다음과 같이 묶었고, 공개 집계표에 1~4명 칸이 남으면 스크립트가 중단된다.",
  "- BS1_1·BS3_1의 모름·무응답(9)과 결측(NA)은 모두 sm_presnt 결측이므로 9/NA 한 범주로 합쳐 표시한다(missingness.csv에서는 n_missing에 포함).",
  "- 당뇨병 가족력은 부·모·형제자매 3원표 대신 부×모, 형제자매×dm_fh 2원표 두 개로 제공한다.",
  "- 가족력 변수(HE_DMfh1~3, HE_fh, dm_fh)는 2024년 19세 이상 조사 항목이므로 결측표·빈도표에 19세 이상 범위만 싣는다. 전체와 19세 이상의 차이로 19세 미만의 작은 칸이 계산되기 때문이며, 19세 미만 응답 수는 '자료와 지침서가 다른 점'에만 기록한다.",
  "- 스크립트는 전체와 19세 이상 범위의 차이로 1~4명이 계산되는 경우도 검사한다.", "",
  "## 흡연 교차표(19세 이상)", "", md_table(smoking_crosstab), "",
  "## 당뇨병 가족력 교차표(19세 이상)", "",
  "부(HE_DMfh1) × 모(HE_DMfh2):", "", md_table(fh_parents_crosstab), "",
  "형제자매(HE_DMfh3) × 합친 값(dm_fh):", "", md_table(fh_sibling_crosstab), "",
  "## 결측 겹침(19세 이상)", "",
  "대각선은 변수별 결측 수, 나머지는 두 변수가 함께 결측인 인원이다.", "",
  md_table(missing_overlap), "",
  "흡연·음주 결측은 거의 같은 사람으로, 건강설문 미응답으로 보인다.", "",
  "## 결측 개수별 인원", "", md_table(missing_count), "",
  "## 생활습관 변수로 인한 추가 제외", "",
  "Y(당뇨병 단계)·HE_BMI·HE_wc가 모두 있는 19세 이상을 기준으로 계산했다. 세은 기준 확정 후 다시 계산한다.", "",
  md_table(additional_exclusion), "",
  "## 자료와 지침서가 다른 점", "", md_table(guide_discrepancies), "",
  "## 팀 결정 필요", "",
  "- 전자담배: 주 변수(smk3)에는 넣지 않았다. 관련 변수는 궐련형 BS12_37(평생)·BS12_47(현재), 액상형 BS12_1(평생)·BS12_2(최근 1달)이며(책자 158~159쪽), 담배제품 현재사용률 sm_t 산출 프로그램은 책자 42쪽 표 18에 있다. 포함 여부는 팀 결정 필요.",
  "- 결측 처리: 생활습관 4변수 complete case로 모형 1~3 동일표본을 쓸지, 신체활동 결측을 별도 범주로 둔 민감도 분석을 추가할지.",
  "- 가족력 '모름' 별도 범주 유지 확정.",
  "- 분석대상 연령과 세은 제외 기준 확정 후 추가 제외 인원 재계산.", "",
  "## 재실행", "", "```r",
  "source(\"R/check_lifestyle_variables.R\", encoding = \"UTF-8\")", "```", "",
  "## 근거", "",
  "국민건강영양조사 제9기(2022-2024) 원시자료 이용지침서. 쪽수는 모두 책자 쪽수(목차 기준)이며 PDF 쪽 번호는 책자 쪽수보다 8 크다.",
  "- 주요지표 변수 생성 프로그램: 41~43쪽(표 18 현재흡연율·담배제품 현재사용률·월간음주율, 표 19 유산소 신체활동 실천율)",
  "- 음주: 142~144쪽 / 흡연: 156~164쪽 / 신체활동: 165~170쪽 / 검진기본조사·가족력: 174~176쪽",
  "- 조사 대상 연령: 6쪽, 15쪽(4-12 가족력)",
  "지침서 원문은 저장소에 포함하지 않는다.",
  "공식 출처: https://knhanes.kdca.go.kr/knhanes/main.do")
con <- file(file.path(out, "README.md"), open = "w", encoding = "UTF-8")
writeLines(report, con)
close(con)
cat("완료: ", out, "\n", sep = "")
print(additional_exclusion)
