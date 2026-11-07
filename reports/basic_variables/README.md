# 조은 담당: 나이·성별·가구소득 사분위·교육 점검

## 범위

2024 기본DB를 R로 읽어 네 변수의 정의·코드·범위·결측·비가중 분포를 확인했다.
all은 전체 자료, age_19plus_reference는 19세 이상 참고 집단이다.
19세 기준은 비교용이며 최종 분석대상 확정이 아니다. 임신·공복·Y·비만 유형 등의 제외 기준은 아직 적용하지 않았다.
모든 인원과 비율은 비가중 기술점검 값이며 국민 전체의 비율·최종 분석결과가 아니다.

## 변수정의서

| variable | description | coding | planned_use | note |
| --- | --- | --- | --- | --- |
| age | 나이 | 1~79: 해당 나이; 80: 80세 이상 | 연속형 보정변수; 80 유지, 연령대 민감도 분석 | 80은 80세 이상; 제외·임의 대체 없이 유지; docs/age_topcoding.md 참조 |
| sex | 성별 | 1: 남자; 2: 여자 | 범주형 보정변수 | 이 자료의 성별 코드이며 성정체성 변수로 해석하지 않음 |
| ho_incm | 가구소득 사분위 | 1: 하; 2: 중하; 3: 중상; 4: 상 | 범주형 보정변수; incm과 구분 | 공식 생성 사분위를 사용하며 분석표본에서 다시 4등분하지 않음 |
| edu | 교육수준 | 1: 초졸 이하; 2: 중졸; 3: 고졸; 4: 대졸 이상 | 범주형 보정변수 | 졸업은 현 학력, 수료·중퇴·재학·휴학은 이전 학력으로 분류 |

## 결측·허용범위 점검

| scope | variable | n_total | n_valid | n_missing | missing_pct | n_invalid |
| --- | --- | --- | --- | --- | --- | --- |
| all | age | 6997 | 6997 | 0 | 0 | 0 |
| all | sex | 6997 | 6997 | 0 | 0 | 0 |
| all | ho_incm | 6997 | 6952 | 45 | 0.64 | 0 |
| all | edu | 6997 | 6764 | 233 | 3.33 | 0 |
| age_19plus_reference | age | 6033 | 6033 | 0 | 0 | 0 |
| age_19plus_reference | sex | 6033 | 6033 | 0 | 0 | 0 |
| age_19plus_reference | ho_incm | 6033 | 5992 | 41 | 0.68 | 0 |
| age_19plus_reference | edu | 6033 | 5808 | 225 | 3.73 | 0 |

NA는 원자료를 읽었을 때의 결측이며 여기서 결측의 원인을 추정하거나 대체하지 않는다.
값 라벨은 원자료에 모두 포함되어 있지 않아 지침서의 코드 설명과 대조했다.

## 범주별 분포

| scope | variable | code | category | n | pct_of_scope |
| --- | --- | --- | --- | --- | --- |
| all | sex | 1 | 남자 | 3070 | 43.88 |
| all | sex | 2 | 여자 | 3927 | 56.12 |
| all | sex | NA | 결측 | 0 | 0 |
| all | ho_incm | 1 | 하 | 1225 | 17.51 |
| all | ho_incm | 2 | 중하 | 1677 | 23.97 |
| all | ho_incm | 3 | 중상 | 1972 | 28.18 |
| all | ho_incm | 4 | 상 | 2078 | 29.7 |
| all | ho_incm | NA | 결측 | 45 | 0.64 |
| all | edu | 1 | 초졸 이하 | 1759 | 25.14 |
| all | edu | 2 | 중졸 | 742 | 10.6 |
| all | edu | 3 | 고졸 | 1930 | 27.58 |
| all | edu | 4 | 대졸 이상 | 2333 | 33.34 |
| all | edu | NA | 결측 | 233 | 3.33 |
| age_19plus_reference | sex | 1 | 남자 | 2582 | 42.8 |
| age_19plus_reference | sex | 2 | 여자 | 3451 | 57.2 |
| age_19plus_reference | sex | NA | 결측 | 0 | 0 |
| age_19plus_reference | ho_incm | 1 | 하 | 1152 | 19.09 |
| age_19plus_reference | ho_incm | 2 | 중하 | 1410 | 23.37 |
| age_19plus_reference | ho_incm | 3 | 중상 | 1642 | 27.22 |
| age_19plus_reference | ho_incm | 4 | 상 | 1788 | 29.64 |
| age_19plus_reference | ho_incm | NA | 결측 | 41 | 0.68 |
| age_19plus_reference | edu | 1 | 초졸 이하 | 966 | 16.01 |
| age_19plus_reference | edu | 2 | 중졸 | 597 | 9.9 |
| age_19plus_reference | edu | 3 | 고졸 | 1912 | 31.69 |
| age_19plus_reference | edu | 4 | 대졸 이상 | 2333 | 38.67 |
| age_19plus_reference | edu | NA | 결측 | 225 | 3.73 |

pct_of_scope는 결측까지 포함한 해당 집단 전체를 분모로 한다. 반올림으로 합계가 100과 조금 다를 수 있다.

## 나이 요약

| scope | n | min | q1 | median | recorded_mean | q3 | max_recorded | n_80plus |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| all | 6997 | 1 | 31 | 52 | 48.1 | 66 | 80 | 426 |
| age_19plus_reference | 6033 | 19 | 41 | 56 | 54.16 | 68 | 80 | 426 |

recorded_mean은 80세 이상을 80으로 기록한 공개값의 평균이다. 실제 평균 연령으로 해석하지 않는다.

## 후속 결정

- age는 연속형으로 사용하고 80세 이상은 공개값 80을 유지한다. 탑코딩만으로 제외하거나 실제 연령을 추정하지 않는다.
- 80세 이상을 별도 범주로 둔 연령대 보정 모형을 동일표본에서 비교한다. 세부 기준·해석 한계: [연령 탑코딩 처리 기준](../../docs/age_topcoding.md).
- sex·ho_incm·edu는 주 모형에서 범주형으로 처리하는 방향을 제안한다. 1~4를 자동으로 연속 점수로 넣지 않는다.
- 소득은 ho_incm(가구)이며 incm(개인)으로 바꾸지 않는다.
- 교육 결측을 초졸 이하로, 소득 결측을 하 분위로 바꾸지 않는다.
- 최종 결측 처리와 동일 분석표본 선정은 다른 담당 변수와 통합한 후 결정한다.

## 재실행

```r
source("R/check_basic_variables.R", encoding = "UTF-8")
```

## 근거

제공된 국민건강영양조사 제9기(2022–2024) 원시자료 이용지침서 책자 93–94쪽(기본 변수), 95쪽(소득 사분위 참고) 및 원자료 변수 속성·실측 분포.
로컬 지침서: references/KNHANES_2022_2024_guide.pdf
공식 출처: https://knhanes.kdca.go.kr/knhanes/main.do
