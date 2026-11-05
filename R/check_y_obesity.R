# 저장소 루트에서 source("R/check_y_obesity.R", encoding = "UTF-8") 실행
# 세은 담당(1주차): 당뇨병 단계(Y)·비만 유형 정의와 분석대상 포함·제외 기준 점검.
# 원자료를 수정하거나 개인별 자료를 저장하지 않고 집계표만 생성합니다.
# 모든 인원은 비가중 표본 수이며 국민 전체 비율·최종 분석결과가 아닙니다.

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

needed <- c("age", "sex", "HE_DM_HbA1c", "HE_glu", "HE_HbA1c", "HE_fst",
            "HE_prg", "HE_dprg", "DE1_dg", "DE1_31", "DE1_32",
            "HE_BMI", "HE_wc", "HE_obe")
stopifnot(all(needed %in% names(hn24)))
v <- lapply(hn24[needed], as.numeric)

# 1. Y 정의 재현 검증 ---------------------------------------------------------
# 지침서 44쪽(표 20) SAS 프로그램과 180쪽 정의를 그대로 옮겨, 공개 생성변수와 대조한다.
# 판정 조건: 비임신(HE_dprg 결측), 공복혈당·당화혈색소 있음, 8시간 이상 공복,
# DE1_dg·DE1_31·DE1_32가 0/1/8(9=모름이면 판정하지 않음).
# 공개 HE_DM_HbA1c는 19세 이상에게만 생성되어 있어(SAS 프로그램에는 연령 조건 없음) 같은 조건을 둔다.
eligible <- v$age >= 19 & is.na(v$HE_dprg) & !is.na(v$HE_glu) & !is.na(v$HE_HbA1c) &
  !is.na(v$HE_fst) & v$HE_fst >= 8 &
  v$DE1_dg %in% c(0, 1, 8) & v$DE1_31 %in% c(0, 1, 8) & v$DE1_32 %in% c(0, 1, 8)
dm <- v$HE_glu >= 126 | v$DE1_31 == 1 | v$DE1_32 == 1 | v$DE1_dg == 1 | v$HE_HbA1c >= 6.5
pre <- (v$HE_glu >= 100 & v$HE_glu < 126) | (v$HE_HbA1c >= 5.7 & v$HE_HbA1c <= 6.4)
y_rule <- ifelse(!eligible, NA, ifelse(dm, 3, ifelse(pre, 2, 1)))
y_match <- identical(is.na(y_rule), is.na(v$HE_DM_HbA1c)) &&
  all(y_rule == v$HE_DM_HbA1c, na.rm = TRUE)
if (!y_match) stop("지침서 규칙으로 재현한 Y가 HE_DM_HbA1c와 다릅니다. 정의를 다시 확인하세요.")

# 2. 비만 유형 -----------------------------------------------------------------
# BMI 비만: HE_BMI >= 25 (지침서 179쪽 HE_obe 4~6단계와 동일 경계).
# 복부비만: 남자 HE_wc >= 90cm, 여자 >= 85cm (대한비만학회 2022 비만 진료지침; 지침서에는 절단값 없음).
# 둘 중 하나라도 결측이면 비만 유형도 결측. '비만 아님'으로 바꾸지 않는다.
bmi_obese <- v$HE_BMI >= 25
abd_obese <- ifelse(v$sex == 1, v$HE_wc >= 90, v$HE_wc >= 85)
group_labels <- c("1" = "BMI 비만 아님·복부비만 아님",
                  "2" = "BMI 비만 아님·복부비만",
                  "3" = "BMI 비만·복부비만 아님",
                  "4" = "BMI 비만·복부비만")
obesity_type <- ifelse(is.na(bmi_obese) | is.na(abd_obese), NA,
  1 + abd_obese + 2 * bmi_obese)
# HE_obe는 19세 이상 비임신자에게만 생성되어 있으므로(지침서 179쪽) 그 범위에서 HE_BMI 경계와 대조한다.
a19 <- v$age >= 19
pregnant <- v$HE_prg %in% 1
stopifnot(all(is.na(v$HE_obe[a19]) == (is.na(v$HE_BMI) | pregnant)[a19]),
  all((v$HE_obe >= 4) == bmi_obese, na.rm = TRUE))

# 3. 분석대상 선정 흐름 ---------------------------------------------------------
# 순서: 19세 이상 → Y 결측 제외 → 비만 유형 결측 제외. 각 단계에서 사유별 인원을 기록한다.
adult <- v$age >= 19
y_reason <- ifelse(!is.na(v$HE_DM_HbA1c), NA,
  ifelse(v$HE_prg %in% 1 | !is.na(v$HE_dprg), "임신",
  ifelse(is.na(v$HE_fst) | is.na(v$HE_glu) | is.na(v$HE_HbA1c), "혈액검사·공복시간 결측",
  ifelse(v$HE_fst < 8, "공복 8시간 미만",
  ifelse(!(v$DE1_dg %in% c(0, 1, 8)) | !(v$DE1_31 %in% c(0, 1, 8)) |
         !(v$DE1_32 %in% c(0, 1, 8)), "당뇨병 설문 모름·무응답·결측", "기타")))))
ob_reason <- ifelse(!is.na(obesity_type), NA,
  ifelse(is.na(v$HE_BMI) & is.na(v$HE_wc), "BMI·허리둘레 모두 결측",
  ifelse(is.na(v$HE_BMI), "BMI 결측", "허리둘레 결측")))

step2 <- adult
step3 <- step2 & !is.na(v$HE_DM_HbA1c)
step4 <- step3 & !is.na(obesity_type)
row <- function(step, reason, excluded, remaining) {
  data.frame(step = step, reason = reason, n_excluded = excluded, n_remaining = remaining)
}
y_tab <- table(y_reason[step2], useNA = "no")
ob_tab <- table(ob_reason[step3], useNA = "no")
flow <- rbind(
  row(0, "2024 전체 자료", NA, length(adult)),
  row(1, "19세 미만", sum(!adult), sum(step2)),
  do.call(rbind, lapply(names(y_tab), function(r)
    row(2, paste0("Y 결측: ", r), as.integer(y_tab[[r]]), NA))),
  row(2, "Y 결측 소계", sum(step2 & is.na(v$HE_DM_HbA1c)), sum(step3)),
  do.call(rbind, lapply(names(ob_tab), function(r)
    row(3, paste0("비만 유형 결측: ", r), as.integer(ob_tab[[r]]), NA))),
  row(3, "비만 유형 결측 소계", sum(step3 & is.na(obesity_type)), sum(step4)))
stopifnot(sum(step4) == length(adult) - sum(flow$n_excluded[grepl("19세|소계", flow$reason)]))

# 4. 비만 유형 × 당뇨병 단계 4×3 인원표 (분석대상 후보, 비가중) --------------------
y_labels <- c("1" = "정상", "2" = "당뇨병 전단계", "3" = "당뇨병")
cross <- table(factor(obesity_type[step4], 1:4, group_labels),
               factor(v$HE_DM_HbA1c[step4], 1:3, y_labels))
cross_df <- data.frame(obesity_type = rownames(cross),
  normal = as.integer(cross[, 1]), prediabetes = as.integer(cross[, 2]),
  diabetes = as.integer(cross[, 3]), row.names = NULL)
cross_df$total <- rowSums(cross_df[, 2:4])
stopifnot(sum(cross_df$total) == sum(step4))
age_y <- table(cut(v$age[step3], c(18, 29, 39, 49, 59, 69, 80),
  labels = c("19-29", "30-39", "40-49", "50-59", "60-69", "70+")),
  factor(v$HE_DM_HbA1c[step3], 1:3, y_labels))
age_y_df <- data.frame(age_group = rownames(age_y), normal = as.integer(age_y[, 1]),
  prediabetes = as.integer(age_y[, 2]), diabetes = as.integer(age_y[, 3]))
underweight <- sum(v$HE_obe[step4] == 1, na.rm = TRUE)

# 5. 변수정의서 ------------------------------------------------------------------
dictionary <- data.frame(
  new_variable = c("dm_stage", "bmi_obese", "abd_obese", "obesity_type"),
  source_variable = c("HE_DM_HbA1c", "HE_BMI", "HE_wc, sex", "HE_BMI, HE_wc, sex"),
  definition = c(
    "당뇨병 유병여부(공개 생성변수 그대로 사용)",
    "BMI 비만 여부",
    "복부비만 여부",
    "BMI 비만 × 복부비만 4개 집단"),
  coding = c(
    "1: 정상; 2: 당뇨병 전단계; 3: 당뇨병",
    "1: HE_BMI >= 25; 0: HE_BMI < 25",
    "1: 남자 HE_wc >= 90 또는 여자 HE_wc >= 85; 0: 그 외",
    paste(paste0(names(group_labels), ": ", group_labels), collapse = "; ")),
  missing_rule = c(
    "임신·공복 8시간 미만·혈액검사 결측·DE1 모름(9)이면 원자료에서 결측. 대체하지 않음",
    "HE_BMI 결측이면 결측",
    "HE_wc 결측이면 결측",
    "BMI 또는 허리둘레 결측이면 결측. '비만 아님'으로 처리하지 않음"),
  source = c(
    "지침서 180쪽(정의), 44쪽 표 20(SAS 프로그램), 104-105쪽(DE1 코드)",
    "지침서 179쪽(HE_obe 정의), 44쪽 표 20",
    "대한비만학회 2022 비만 진료지침(Haam et al., J Obes Metab Syndr 2023;32:121-129). 지침서 179쪽은 측정값만 제공",
    "위 두 기준의 조합(팀 정의)"),
  note = c(
    "혈당·당화혈색소·의사진단·투약 변수는 Y 판정에 쓰였으므로 X에서 제외",
    "'BMI 비만 아님'에는 저체중(HE_obe=1)이 포함되므로 '정상체중'으로 부르지 않음",
    "성별 기준이 다름",
    "비만 유형을 넣은 모형에 BMI·허리둘레를 중복 투입하지 않음"))

# 6. 저장 ------------------------------------------------------------------------
out <- file.path("reports", "y_obesity")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
write.csv(dictionary, file.path(out, "variable_dictionary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(flow, file.path(out, "selection_flow.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(cross_df, file.path(out, "obesity_by_dm_stage.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(age_y_df, file.path(out, "age_by_dm_stage.csv"), row.names = FALSE, fileEncoding = "UTF-8")

md_table <- function(x) {
  x[] <- lapply(x, function(col) ifelse(is.na(col), "", as.character(col)))
  c(paste0("| ", paste(names(x), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |"),
    apply(x, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}
report <- c(
  "# 세은 담당: 당뇨병 단계(Y)·비만 유형·분석대상 기준", "",
  "## 범위", "",
  "2024 기본DB에서 Y와 비만 유형의 정의·코드·결측을 확인하고, 분석대상 포함·제외 기준 초안과 4×3 인원표를 만들었다.",
  "모든 인원은 비가중 표본 수이며 국민 전체 비율·최종 분석결과가 아니다. 기준은 팀 합의 전 제안안이다.", "",
  "## 확정 제안 기준", "",
  "- Y: 공개 생성변수 HE_DM_HbA1c를 그대로 사용한다(1 정상 / 2 당뇨병 전단계 / 3 당뇨병).",
  paste0("- Y 재현 검증: 지침서 44쪽 SAS 규칙으로 다시 계산한 값이 HE_DM_HbA1c와 결측 여부까지 ",
         if (y_match) "전원 일치했다." else "일치하지 않았다."),
  "- 공개 HE_DM_HbA1c는 19세 이상에게만 값이 있다(10-18세는 혈액검사 값이 있어도 결측). 지침서 SAS 프로그램에는 연령 조건이 없으므로 재현 시 19세 이상 조건을 추가했다.",
  "- BMI 비만: HE_BMI >= 25. 복부비만: 남자 HE_wc >= 90cm, 여자 >= 85cm.",
  "- 비만 유형: 두 기준을 조합한 4개 집단. 한쪽이라도 결측이면 비만 유형도 결측이다.",
  "- 분석대상(제안): 19세 이상 → Y 결측 제외 → 비만 유형 결측 제외.", "",
  "## 변수정의서", "", md_table(dictionary), "",
  "## 분석대상 선정 흐름", "", md_table(flow), "",
  "임신·공복 8시간 미만·DE1 모름은 원자료 정의에서 이미 Y 결측으로 처리되어 있으므로 별도 제외 단계를 두지 않고 Y 결측 사유로 기록했다.", "",
  "## 비만 유형 × 당뇨병 단계 (비가중)", "", md_table(cross_df), "",
  paste0("BMI 비만 아님 집단에는 저체중 ", underweight, "명이 포함된다."), "",
  "## 연령대 × 당뇨병 단계 (Y 있는 19세 이상, 비가중)", "", md_table(age_y_df), "",
  "## 결측·모름·비해당 처리", "",
  "- DE1_dg·DE1_31·DE1_32: 8은 비해당(소아·청소년, 의사진단 없음 등), 9는 모름·무응답(지침서 104-105쪽).",
  "- 지침서 판정 규칙은 DE1 변수가 0/1/8일 때만 Y를 생성하므로 9(모름)는 Y 결측이 된다. 모름을 '없음'으로 바꾸지 않는다.",
  "- HE_prg: 0 아니오, 1 예, 8 비해당(남자)(지침서 174쪽). 임신부는 원자료에서 Y·HE_obe가 결측이다.",
  "- BMI·허리둘레 결측은 대체하지 않고 결측으로 둔다.", "",
  "## 같이 결정할 사항", "",
  "- 분석대상 연령: 19세 이상 유지 또는 30세 이상(19-29세는 당뇨병 인원이 적음).",
  "- 희소 칸(BMI 비만·복부비만 아님 × 당뇨병) 처리와 2주차 가중 비율 점검.",
  "- 비만 유형 집단 이름 최종안과 저체중 별도 구분 여부.",
  "",
  "## 재실행", "", "```r",
  "source(\"R/check_y_obesity.R\", encoding = \"UTF-8\")", "```", "",
  "## 근거", "",
  "국민건강영양조사 제9기(2022–2024) 원시자료 이용지침서 책자 44쪽(표 20 주요지표 SAS 프로그램), 104–105쪽(당뇨병 설문 DE1), 174쪽(공복시간·임신), 179쪽(신체계측·HE_obe), 180쪽(HE_DM_HbA1c).",
  "복부비만 절단값(남자 90cm, 여자 85cm): 대한비만학회 2022 비만 진료지침. Haam JH, et al. Diagnosis of Obesity: 2022 Update of Clinical Practice Guidelines for Obesity by the Korean Society for the Study of Obesity. J Obes Metab Syndr. 2023;32(2):121-129. doi:10.7570/jomes23031",
  "공식 출처: https://knhanes.kdca.go.kr/knhanes/main.do")
writeLines(report, file.path(out, "README.md"), useBytes = TRUE)
cat("완료: ", out, "\n", sep = "")
print(flow)
print(cross_df)
