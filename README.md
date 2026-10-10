# covid19-humanRights

108개국의 COVID-19 비약물적 개입(NPI)이 확진자 증가 억제에 미친 효과를 분석한 데이터와 R 코드입니다.

Data and R code for the cross-country regression analysis of the effect of COVID-19 non-pharmaceutical interventions (NPIs) in 108 countries.

## 논문

> Hong, S.-H., Hwang, H.\*, & Park, M.-H. (2021). Effect of COVID-19 Non-Pharmaceutical Interventions and the Implications for Human Rights. *International Journal of Environmental Research and Public Health*, 18(1), 217. https://doi.org/10.3390/ijerph18010217 (\*교신저자)

## 연구 개요

- **분석 대상**: 108개국, 2020년 1월 1일~6월 15일
- **자료**: Oxford COVID-19 Government Response Tracker(OxCGRT)의 국가별 확진자 수와 정책 대응 기록
- **방법**: DRICs를 종속변수, NPI를 독립변수로 한 다중선형회귀
- **NPI 분류**: 각 NPI가 무엇을 제한하는지에 따라 세 가지로 나눔

| 분류 | 변수 | NPI |
|---|---|---|
| 집합 제한 (Assembly Restrictions) | A1 | 학교 폐쇄 |
| | A2 | 직장 폐쇄 |
| | A3 | 공공행사 취소 |
| | A4 | 모임 규모 제한 |
| 이동 제한 (Movement Restrictions) | M1 | 대중교통 폐쇄 |
| | M2 | 자택 대기 |
| | M3 | 국내 이동 제한 |
| | M4 | 국제 여행 제한 |
| 사생활 제한 (Privacy Restriction) | P1 | 접촉자 추적 |

NPI 점수는 OxCGRT 방식으로 정책 강도와 적용 범위(일부 대상/전체)를 반영해 0~1로 표준화했습니다.

### 종속변수 DRICs

DRICs(Decrease Rate of Increase in the Cumulated number of confirmed cases)는 확진자 증가가 얼마나 빨리 꺾였는지를 나타냅니다.

1. 누적 확진자 수를 3일 이동평균으로 평활화합니다.
2. 주간 평균 증가율 a<sub>t</sub> = (y<sub>t+3</sub> − y<sub>t−3</sub>) / 6을 계산합니다.
3. 증가율이 가장 높았던 날 t′의 증가율을 잠복기(6일) 뒤의 증가율로 나눕니다: Φ = a<sub>t′</sub> / a<sub>t′+6</sub>

값이 클수록 최고점 이후 증가세가 빨리 줄었다는 뜻입니다.

## 주요 결과

| 모형 | 결과 |
|---|---|
| Model 1 (NPI 주효과) | 학교 폐쇄(A1)만 유의한 양(+)의 효과를 보였습니다(1.328, p < 0.001). 공공행사 취소(A3)와 국내 이동 제한(M3)은 음(−)의 계수를 보였습니다. |
| Model 2 (집합 제한 × 접촉자 추적) | 학교 폐쇄와 접촉자 추적의 상호작용(A1:P1)이 강한 양(+)의 효과를 보였습니다(4.162, p < 0.001). |
| Model 3 (모든 NPI × 접촉자 추적) | A1:P1 효과가 유지되었습니다(3.930, p < 0.01). |

결론적으로 학교 폐쇄는 **전면적 접촉자 추적과 함께 시행될 때만** 효과가 있었습니다. 10개국 사례 비교에서도 초기부터 전면적 접촉자 추적을 시행한 국가(한국, 중국, 베트남, 슬로베니아, 뉴질랜드)는 누적 확진자 곡선이 빠르게 포화되었습니다. 반면 접촉자 추적이 부분적이었거나 늦었던 국가(영국, 미국, 스웨덴)는 확진자가 계속 늘었습니다.

논문은 이를 근거로 신중하게 설계된 전면적 접촉자 추적과 학교 폐쇄를 함께 고려하되, 이 경우에도 인권 침해 위험을 최소화해야 한다고 제안합니다.

## 파일 구성

| 파일 | 내용 |
|---|---|
| `dt_covid19_humanRights.csv` | 108개국 분석 데이터 |
| `covid_19_humanRights.R` | 상관분석과 회귀모형 1~3 추정, 회귀 진단 그래프 |
| `Figure_10_countries.xlsx` | 논문 Figure 2(10개국 대응 전략과 확진자 추이) 데이터와 그래프. 시트는 `data`, `graphs` |

10개국은 영국, 미국, 스웨덴, 프랑스, 슬로베니아, 중국, 한국, 베트남, 일본, 뉴질랜드입니다.

### 변수 (`dt_covid19_humanRights.csv`)

| 변수 | 설명 |
|---|---|
| `CountryName`, `CountryCode` | 국가명, ISO 3자리 코드 |
| `Date` | 누적 확진자 수의 평균 증가율이 가장 높았던 날 (YYYYMMDD) |
| `DRICs` | 누적 확진자 증가율의 감소율 |
| `A1`~`A4`, `M1`~`M4`, `P1` | `Date` 시점의 표준화된 NPI 점수 (0~1) |

## 실행

```r
dt <- read.csv("dt_covid19_humanRights.csv")
model_2 <- lm(DRICs ~ A1*P1 + A2*P1 + A3*P1 + A4*P1 + M1 + M2 + M3 + M4, data = dt)
summary(model_2)
```

전체 분석은 `covid_19_humanRights.R`을 실행하면 됩니다. 기본 R 외에 추가 패키지는 필요하지 않습니다.

## 자료 출처

Oxford COVID-19 Government Response Tracker (OxCGRT), https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker

## 연구비

이 연구는 교육부 재원으로 한국연구재단 기초연구사업의 지원을 받았습니다(2018R1D1A1B07047033).

## 작성자

황하 (Ha Hwang) · 한국행정연구원
