
# 무선통신_기술방식별_트래픽_현황 자료 로드
Network <- read.csv("./6._무선통신_기술방식별_트래픽_현황_2018.8월말_.csv",header=TRUE)
nw1 <- Network[,c(1:6)]
nw1$Date <- gsub("년", "", nw1$구.분)
nw1$Date <- gsub("월", "", nw1$Date)
nw1$Date <- gsub(" ", "", nw1$Date)
nw1$Year <- as.integer(substr(nw1$Date, 0,4))
nw1$Month <- as.integer(substr(nw1$Date, 5,7))
nw1$X2G.TB. <- as.integer(gsub(",","",nw1$X2G.TB.))
nw1$X3G.TB. <- as.integer(gsub(",","",nw1$X3G.TB.))
nw1$X4G.TB. <- as.integer(gsub(",","",nw1$X4G.TB.))
nw1$WiBro.TB. <- as.integer(gsub(",","",nw1$WiBro.TB.))
nw1$WiFi.TB. <- as.integer(gsub(",","",nw1$WiFi.TB.))
nw1 <- subset(nw1, select = -구.분)
nw1 <- subset(nw1, select = -Date)

# 산점도 작성
plot(X2G.TB.~X3G.TB.,data=nw1,xlab="X3G.TB.",ylab="X2G.TB.",type="p",pch=20,col="blue",cex=2)
plot(X3G.TB.~X4G.TB.,data=nw1,xlab="X4G.TB.",ylab="X3G.TB.",type="p",pch=20,col="blue",cex=2)
plot(X4G.TB.~WiBro.TB.,data=nw1,xlab="WiBro.TB.",ylab="X4G.TB.",type="p",pch=20,col="blue",cex=2)
plot(WiBro.TB.~WiFi.TB.,data=nw1,xlab="WiFi.TB.",ylab="WiBro.TB..",type="p",pch=20,col="blue",cex=2)

#install.packages("car")
library(car) 

scatterplot(X2G.TB.~X3G.TB.,data=nw1,pch=1,cex=2)
scatterplot(X3G.TB.~X4G.TB.,data=nw1,pch=1,cex=2)

# 산점도 행렬 작성
plot(nw1[c("X2G.TB.","X3G.TB.","X4G.TB.","WiBro.TB.","WiFi.TB.")],pch=9,cex=2)
library(car)
scatterplotMatrix(nw1[c("X2G.TB.","X3G.TB.","X4G.TB.","WiBro.TB.","WiFi.TB.")],pch=8,cex=2,spread=FALSE,ellipse=TRUE)

# 상관계수 계산
cor(nw1[,c(1:5)])

# 상관계수에 대한 검정
cor.test(nw1$X2G.TB., nw1$X3G.TB., method=c("pearson"))
cor.test(nw1$X3G.TB., nw1$X4G.TB., method=c("pearson"))

# 상관계수와 p-value
#install.packages("Hmisc")
library(Hmisc)
nw1.matrix = as.matrix(nw1)
rcorr(nw1.matrix,type="pearson")
# 상관계수의 그래프적 표현
nw1.cor = cor(nw1[,c(1:5)])
nw1.cor
#install.packages("corrplot")
library(corrplot)
corrplot(nw1.cor,method="number",diag=TRUE,type="full")
corrplot(nw1.cor,method="circle",diag=FALSE,type="full")
corrplot(nw1.cor,method="ellipse",diag=TRUE,type="lower")
corrplot(nw1.cor,method="pie",diag=TRUE,type="upper")

# 편상관계수의 계산 - pcor은 처음 두 변수의 상관관계를 계산. 나머지 변수는 통제변수.  
# install.packages("ggm")
library(ggm)
pcor(c("X2G.TB.","X3G.TB.","X4G.TB.","WiBro.TB.","WiFi.TB."),var(nw1))
pcor(c("X3G.TB.","X4G.TB.","X2G.TB.","WiBro.TB.","WiFi.TB."),var(nw1))
nw1.pcor = pcor(c("X3G.TB.","X4G.TB.","X2G.TB.","WiBro.TB.","WiFi.TB."),var(nw1))
pcor.test(nw1.pcor,1,20)


# 연도별로 구분된 산점도 작성
nw1$Yeargroup[nw1$Year<2013]=1
nw1$Yeargroup[nw1$Year<2015 & nw1$Year>=2013]=2
nw1$Yeargroup[nw1$Year<2017 & nw1$Year>=2015]=3
nw1$Yeargroup[nw1$Year<=2018 & nw1$Year>=2017]=4
nw1$Yeargroup = factor(nw1$Yeargroup,labels=c("12","13~14","15~16","17~18"))
library(car)
scatterplot(X3G.TB.~X4G.TB.,data=nw1,cex=2,lwd=2,boxplots="x",smoother=FALSE)
scatterplot(X3G.TB.~X4G.TB.|Yeargroup,data=nw1,cex=2,lwd=2,smoother=FALSE)


# 연도별 상관계수의 출럭
nw1.2012 = nw1[which(nw1$Year=="2012"),]
cor(nw1.2012[c("X3G.TB.","X4G.TB.")])
cor.test(nw1.2012$X3G.TB.,nw1.2012$X4G.TB.)

nw1.2017 = nw1[which(nw1$Year=="2017"),]
cor(nw1.2017[c("X3G.TB.","X4G.TB.")])
cor.test(nw1.2017$X3G.TB.,nw1.2017$X4G.TB.)


# 단순 회귀분석

library(car)
nw1.fit = lm(X4G.TB.~WiFi.TB.,data=nw1)
nw1.fit
anova(nw1.fit)
summary(nw1.fit)


# 산점도와 회귀직선의 출력
plot(X4G.TB.~WiFi.TB.,data=nw1,cex=3,lwd=1)
abline(lm(X4G.TB.~WiFi.TB.,data=nw1),lwd=2,col="green")

# 예측값 및 신뢰구간
confint(nw1.fit)
predict(nw1.fit,interval='confidence',level=0.95)
predict(nw1.fit,interval='prediction',level=0.95)


# 단순회귀모형 진단
par(mfrow=c(2,2))
plot(nw1.fit)
par(mfrow=c(1,1))


# 표준화잔차의 탐색
Fitted = as.vector(fitted(nw1.fit))
Residual = as.vector(residuals(nw1.fit))
Rstandard = as.vector(rstandard(nw1.fit)) 
nw1.res = data.frame(Fitted,Residual,Rstandard)
nw1.res
plot(nw1.res$Rstandard~nw1.res$Fitted,cex=2)
abline(h=c(-3,-2,2,3),lty=2)


# 표준화잔차의 히스토그램과 정규확률도표 작성
hist(nw1.res$Rstandard,probability=TRUE)
lines(density(nw1.res$Rstandard),col="green",lty=1,lwd=2)
library(car)
qqPlot(nw1.res$Rstandard)



# 더빈-왓슨(Durbin-Watson) 통계량의 출력
# install.packages("lmtest")
library(lmtest)
dwtest(nw1.fit)


# 다중 회귀분석  
nw2.fit = lm(X4G.TB.~X3G.TB.+WiFi.TB.,data=nw1)
summary(nw2.fit)
anova(nw2.fit)


# 표준화 회귀계수의 출력 
#install.packages("betas")
library(betas)
betas.lm(nw2.fit)


# 전진 선택법
M.full = lm(X4G.TB.~X3G.TB.+WiFi.TB.,data=nw1)
M.int = lm(X4G.TB.~1,data=nw1)
M.forward = step(M.int,scope=list(upper=M.full),direction="forward")
summary(M.forward)

# 후진 제거법
M.backward = step(M.full,scope=list(upper=M.int),direction="backward")
summary(M.backward)

# 단계적 방법
M.stepwise1 = step(M.int,scope=list(lower=M.int,upper=M.full),direction="both")
M.stepwise2 = step(M.full,scope=list(lower=M.int,upper=M.full),direction="both")
summary(Stepwise)

# 분산확대인자(VIF)의 출력
library(car)
vif(nw2.fit)

sqrt(vif(nw2.fit))>2
