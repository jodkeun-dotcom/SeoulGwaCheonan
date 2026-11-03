# RStudio에서 이 파일 전체를 실행한 뒤 HN24_ALL(SAS).zip을 선택하세요.
# 작업 폴더 설정이나 SAS 설치는 필요하지 않습니다.

if (!requireNamespace("haven", quietly = TRUE)) {
  install.packages("haven", repos = "https://cloud.r-project.org")
}

# 다운로드한 ZIP 파일 선택
zip_path <- file.choose()

# 임시 폴더에 SAS 데이터만 압축 해제
unzip_dir <- tempfile("hn24_")
dir.create(unzip_dir)
utils::unzip(zip_path, files = "hn24_all.sas7bdat", exdir = unzip_dir)

# R 데이터로 불러오기
hn24 <- haven::read_sas(file.path(unzip_dir, "hn24_all.sas7bdat"))
unlink(unzip_dir, recursive = TRUE)

cat("불러오기 완료:", nrow(hn24), "명 /", ncol(hn24), "개 변수\n")

# RStudio에서 데이터 표 열기
if (interactive()) {
  View(hn24)
}
