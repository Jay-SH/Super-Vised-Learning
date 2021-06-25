#사례연구
library(readr)
library(stats)
library(reshape2)
library(dplyr)
library(stringr)
library(forecast)
library(memisc)
library(lmtest)
library(quantmod)
library(TTR)
setwd("C:/workspace/portfolio")

#1 
#데이터 전처리 작업 
rawbirthdata <- read.csv("birthdata.csv")
rawdeathdata <- read.csv("deathdata.csv")
rawpopulation <- read.csv("koreapopulation.csv")

# 전처리 개시
Birth <- rawbirthdata[1,c(2:51)] #신생아 수 항목만 분리
Death <- rawdeathdata[1,c(2:51)] #사망자 수 항목만 분리
Population <- rawpopulation[,-c(2:11)] #총 인구수 집계의 경우 집계 시점이 신생아 사망자 집계보다 빠르기때문에 불용부분 삭제
Population <- Population[,c(2:51)] #총 인구수 항목 분리
연도 <- 1970:2019
BindDF <- as.data.frame(cbind((as.numeric(Birth)),(as.numeric(Death)),(as.numeric(Population)))) 
names(BindDF) <- c("신생아수","사망자수","총인구수")
PopulationDF <- data.frame(연도, BindDF)
PopulationDF$순인구증감 <- c(PopulationDF$신생아수 - PopulationDF$사망자수)

# 1-1 총 인구수 기준으로 회귀 분석

#상관분석 실시 = 신생아수, 사망자 수 및 순인구증감은 총 인구수와는 눈에 띄는 상관관계 존재 x. 연도와 높은 상관관계 발견
#독립변수로 연도를, 종속변수로 총 인구수를 가정하고 회귀분석 실시
cor(PopulationDF) 


# 홀드아웃 검정 데이터셋 생성
idx <- sample(1:nrow(PopulationDF), 0.7*nrow(PopulationDF)) 
train <- PopulationDF[idx,] 
test <- PopulationDF[-idx,] 

# 회귀모델 생성
model.lm <- lm(formula = 총인구수 ~ 연도, data = train)
plot(formula = 총인구수 ~ 연도, data = train)
abline(model.lm, col = 'red') #회귀선 확인 가능.
summary(model.lm)

# 모델의 설명력은 .93, p밸류값은 <2.2e-16에 t값이 20이상으로 산출되기 때문에 적합하다. (코드 재실행시 값 상이할수 있음)

# 회귀방정식 도출하기

head(test,1) # 1974년 총인구수 16159311 확인
Y = (-1.257e+09) + (6.463e+05)*1974
Residuals = 16159311 - Y #잔차

# 예측하기

pred <-predict(model.lm, test) #검정데이터셋을 예측에 활용.
cor(pred, test$총인구수) #0.97로 매우 높은 상관관계 발견 가능.
dwtest(model.lm)
res <- model.lm$residuals

# 더빈왓슨 독립성 테스트 = p값이 0.05이상(DW값 1~3범위) 산출됨. 잔차에 유의미한 자기상관 없음.

#2020, 2021년 인구 예측하기
Y = (-1.257e+09) + (6.463e+05)*2020
Y #2020년 인구예측
Y = (-1.257e+09) + (6.463e+05)*2021
Y #2021년 인구예측

#1-2 순인구증감으로 시계열분석
#연간자료가 비계절성에 비순환성을 가지기때문에 해당 자료를 freq = 1로 시계열 변환시 ARIMA p q값이 0으로 산출되며 기존 함수 사용 불가.
#때문에 loess() 함수를 사용해 순인구증감 추세를 산출해 시계열 자료로 활용할 것.(stats패키지)

totalPopulation <- data.frame(Year = PopulationDF$연도, Growth = PopulationDF$순인구증감, Total = PopulationDF$총인구수)
totalPopulation$Trend <- loess(formula = Growth ~ Year, data = totalPopulation)$fitted #추세 관측값 산출.
plot(totalPopulation$Year, totalPopulation$Growth, type = "l", col = 'red',
     main="순인구 증감 추세", xlab = "연도", ylab = "순인구 증감") #실제 순인구 증감 추세 확인
lines(totalPopulation$Year, totalPopulation$Trend, col = "blue", type = "l") # 추세선과 총 인구 증가 비교
tsPop <- ts(totalPopulation$Trend, start = 1970, freq = 1) #추세 시계열화
arimaPop <- auto.arima(tsPop) #ARIMA 1,2,1로 차분 2회 수행하는 ARMA모델 확인
model <- arima(totalPopulation$Trend, c(1,2,1))

tsdiag(model) #잔차의 ACF에서 자기상관이 1건 검출. Ljung Test 수행
Box.test(arimaPop$residuals,lag = 1,"Ljung") #p값 0.8로 모형의 적절성 확인 가능
fore <- forecast(model, h = 2) #향후 2년 예측
plot(fore) #시각화로 확인.

#인구예측
fore #2020년, 2021년 순인구증감 예측값 확인 가능

#1-3 AAGR 계산하기
#AAGR 계산함수 작성
AAGR <- function(x,y){
  values = (y/x)-1
  return(values)
}

calResult <- 0 #데이터 집계 첫 해의 AAGR은 계산할 수 없음 
calResult <- c(calResult, AAGR(totalPopulation$Total[1:49], totalPopulation$Total[2:50]))
totalPopulation$AAGR <- round(calResult*100,2) #소수점 2자리까지 표시

#1-4 CAGR 계산하기
#CAGR 계산함수 작성
CAGR <- function(x,y,yrs){
  values = ((y/x)^(1/yrs)-1)
  return(values)
}
calResult <- 0 #데이터 집계 첫 해의 CAGR은 계산할 수 없음.
calResult <- c(calResult,CAGR(totalPopulation$Total[1],totalPopulation$Total[2:50],1:49))
totalPopulation$CAGR <- round(calResult*100,2) #소수점 2자리까지 표시

#AAGR과 CAGR 확인 가능.
AAGRandCAGR <- data.frame(Year= totalPopulation$Year,
                          AAGR=totalPopulation$AAGR,
                          CAGR=totalPopulation$CAGR)

#1-5 지수평활법 사용하기 - 총인구수 추세를 바탕으로 지수평활:HoltWinters() 함수 사용(stats 패키지)
tstotal <- tsPop <- ts(totalPopulation$Total, start = 1970, freq = 1) #추세 시계열화
result <- HoltWinters(tstotal, alpha = 0.1, beta = F, gamma = F) #기본
result$fitted
plot(result) #기존의 총인구수 선과 지수평활법이 적용된 총인구수 선(red) 확인.


#2

#2-1 데이터 가져오기
data.spss <- as.data.set(spss.system.file('drinking_water_example.sav'))
drinkingWaterExam <- data.spss[1:7]
drinkingWaterExamDF <- as.data.frame(drinkingWaterExam)
str(drinkingWaterExamDF) 


#2-2 베리맥스 회전법 & 요인 점수 회귀분석방법 적용
drinkFA <- factanal(drinkingWaterExamDF, factors = 2, rotation = "varimax", scores = "regression")
drinkFA #p밸류값 낮음 = 실제 모델로 사용하기엔 요인수 적음. 
print(drinkFA, digit = 2, cutoff = .5) #변수가 포함되는 요인 확인 가능

#2-3 요인적재량 행렬의 칼럼명 변경
colnames(drinkFA$loadings) <- c("제품만족도","제품친밀도")

#2-4 요인 점수를 이용한 적재량 시각화
head(drinkFA$scores)

plot(drinkFA$scores[,c(1,2)], #회귀 분석방법으로 산출한 요인 점수 산점도 생성
     main = "Factor 1 & Factor 2 요인 적재량 시각화")
text(drinkFA$scores[,1],drinkFA$scores[,2],
     labels = rownames(drinkFA$scores), cex = 0.7, pos = 3, col = "blue")

points(drinkFA$loadings[,c(1:2)], pch = 19, col = "red") #요인 적재량 산점도에 표시
text(drinkFA$loadings[,1],drinkFA$loadings[,2], 
     labels = rownames(drinkFA$loadings), cex = 0.8, pos = 3, col = "red") 

#2-5 요인별 변수 묶기
print(drinkFA$loadings, digits = 2, cutoff = .5) #팩터 1의 요인 Q4 Q5 Q6 Q7, 팩터 2의 요인 Q1 Q2 Q3 확인 가능
"제품만족도" <- data.frame(drinkingWaterExamDF$Q4,drinkingWaterExamDF$Q5,drinkingWaterExamDF$Q6,drinkingWaterExamDF$Q7)
"제품친밀도" <- data.frame(drinkingWaterExamDF$Q1,drinkingWaterExamDF$Q2,drinkingWaterExamDF$Q3)

#2-6 프로맥스 회전법 사용
drinkFA2 <- factanal(drinkingWaterExamDF, factors = 2, rotation = "promax", scores = "regression")

#2-7 회전법 비교
drinkFA;drinkFA2
#변수의 유효성은 동일하게 나타남. 요인의 적재값, 설명력, 분산비율과 누적 분산비율에서 조금씩 상이함.

#3

#데이터 가공
rawdataKospi <- read.csv("Kospidata.csv") 
rawdataKospi <- rawdataKospi[order(rawdataKospi[,1]),] #일자 기준으로 다시 정렬

#시계열 자료 형식으로 객체 생성
ClosingPriceData <- ts(rawdataKospi$종가, start = c(2011,4,8),end = c(2021,4,9),frequency = 365)
class(ClosingPriceData)

#3-1 추세선 시각화 확인
X11()
plot.ts(ClosingPriceData, type = "l", col = 'red', main = "10년간 일별 KOSPI 종가")

#3-2시계열 분해 작업
plot(stl(ClosingPriceData, "periodic")) #stl()함수 - 시계열 변동요인(계절,추세,잔차)들을 모두 제공
m <- decompose(ClosingPriceData) #decompose() : 시계열을 분해하는 함수

#3-3시각화
plot(m)



