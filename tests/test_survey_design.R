# 실행: Rscript tests/test_survey_design.R
source("R/make_survey_design.R", encoding = "UTF-8")
example <- data.frame(ID = paste0("test", 1:8), year = 2024,
  wt_itvex = c(1,2,1,2,2,3,2,3), kstrata = rep(1:2, each = 4),
  psu = rep(c("A", "B", "C", "D"), each = 2),
  age = c(12,22,32,42,15,25,35,45))
expect_error <- function(expr, pattern) {
  result <- tryCatch(force(expr), error = function(e) e)
  stopifnot(inherits(result, "error"), grepl(pattern, conditionMessage(result)))
}
original <- example
previous <- options(survey.lonely.psu = "fail", survey.adjust.domain.lonely = FALSE)
tryCatch({
  d <- make_knhanes_design(example)
  stopifnot(identical(example, original), survey::degf(d) == 2)
  sub <- subset(d, age >= 19)
  total <- survey::svytotal(~I(rep(1, length(age))), sub)
  stopifnot(isTRUE(all.equal(as.numeric(stats::coef(total)),
    sum(example$wt_itvex[example$age >= 19]))))
  bad <- example; bad$wt_itvex[1] <- 0
  expect_error(make_knhanes_design(bad), "가중치")
  bad <- example; bad$wt_itvex[1] <- NA
  expect_error(make_knhanes_design(bad), "가중치")
  bad <- example; bad$kstrata[1] <- NA
  expect_error(make_knhanes_design(bad), "층·집락")
  bad <- example; bad$ID[2] <- bad$ID[1]
  expect_error(make_knhanes_design(bad), "ID")
  bad <- example; bad$year[1] <- 2023
  expect_error(make_knhanes_design(bad), "2024")
  expect_error(make_knhanes_design(example[, names(example) != "psu"]), "필수 변수")
  cat("통과: 하위집단 가중합·원자료 보존·잘못된 설계 입력 차단\n")
}, finally = options(previous))
