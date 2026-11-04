# KNHANES 2024 건강설문·검진 변수 조합용 설계.
# 원자료 전체에서 설계를 만든 후 subset(design, ...)으로 분석영역을 선택합니다.
make_knhanes_design <- function(data) {
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("먼저 setup.R을 실행해 survey 패키지를 설치해 주세요.", call. = FALSE)
  }
  required <- c("ID", "year", "wt_itvex", "kstrata", "psu")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop(paste("필수 변수 없음:", paste(missing, collapse = ", ")), call. = FALSE)
  if (!nrow(data)) stop("자료가 비어 있습니다.", call. = FALSE)
  if (anyNA(data$ID) || anyDuplicated(data$ID)) stop("개인 ID 결측·중복을 확인하세요.", call. = FALSE)
  if (anyNA(data$year) || any(data$year != 2024)) stop("2024 단일연도 자료만 지원합니다.", call. = FALSE)
  if (!is.numeric(data$wt_itvex) || anyNA(data$wt_itvex) ||
      any(!is.finite(data$wt_itvex)) || any(data$wt_itvex <= 0)) {
    stop("가중치의 결측·비유한·0 이하 값을 확인하세요. 자동 제외하지 않습니다.", call. = FALSE)
  }
  if (anyNA(data$kstrata) || anyNA(data$psu) ||
      any(trimws(as.character(data$kstrata)) == "") ||
      any(trimws(as.character(data$psu)) == "")) {
    stop("층·집락 식별자의 결측을 확인하세요. 자동 제외하지 않습니다.", call. = FALSE)
  }
  survey::svydesign(ids = ~psu, strata = ~kstrata,
    weights = ~wt_itvex, data = as.data.frame(data), nest = TRUE)
}

# 원설계의 조사구 수와 하위집단에서 실제 관측된 조사구 수를 구분합니다.
# 이 점검표만으로 단일 조사구 보정 옵션을 자동으로 변경하지 않습니다.
audit_survey_domain <- function(full_design, domain_design) {
  full <- full_design$variables
  domain <- domain_design$variables
  required <- c("kstrata", "psu")
  if (!all(required %in% names(full)) || !all(required %in% names(domain))) {
    stop("설계객체의 kstrata·psu를 확인하세요.", call. = FALSE)
  }
  levels <- unique(as.character(full$kstrata))
  count_psu <- function(x) {
    pairs <- unique(data.frame(stratum = as.character(x$kstrata),
                               psu = as.character(x$psu)))
    if (any(!pairs$stratum %in% levels)) stop("원설계에 없는 층이 있습니다.")
    as.integer(table(factor(pairs$stratum, levels = levels)))
  }
  data.frame(stratum = levels, n_psu_full = count_psu(full),
             n_psu_domain_observed = count_psu(domain))
}
