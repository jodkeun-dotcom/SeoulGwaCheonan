# RStudio: open this repository as a project, then click Source.
# If data/raw/HN24_ALL(SAS).zip is absent, choose the downloaded ZIP.
# No SAS installation is required.

if (!requireNamespace("haven", quietly = TRUE)) {
  stop("먼저 콘솔에서 install.packages('haven')을 실행해 주세요.", call. = FALSE)
}

reader_path <- file.path("R", "read_knhanes_2024.R")
if (!file.exists(reader_path)) {
  stop("작업 폴더를 SeoulGwaCheonan 저장소 루트로 설정한 뒤 실행해 주세요.",
       call. = FALSE)
}
source(reader_path, encoding = "UTF-8")

default_zip <- file.path("data", "raw", "HN24_ALL(SAS).zip")
if (file.exists(default_zip)) {
  data_path <- default_zip
} else if (interactive()) {
  message("다운로드한 HN24_ALL(SAS).zip 파일을 선택해 주세요.")
  data_path <- file.choose()
} else {
  stop("data/raw/HN24_ALL(SAS).zip에 다운로드한 ZIP을 넣어 주세요.",
       call. = FALSE)
}

hn24 <- read_knhanes_2024(data_path)
cat("불러오기 완료:", nrow(hn24), "명 /", ncol(hn24), "개 변수\n")
cat("Environment 창의 hn24를 클릭하면 표로 볼 수 있습니다.\n")

# Keep individual-level records in the local R session.
if (interactive()) {
  View(hn24)
}
