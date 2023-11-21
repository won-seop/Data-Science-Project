library(ISLR)
library(MASS)
library(class)
library(leaps)

data_csv <- read.csv("C:\\Users\\tpmgt\\Desktop\\데사 팀플 최종\\final_data_bank.csv", header=TRUE)

data_csv <- data_csv[-1] #전처리과정에서 생긴 순서 열 제거
View(data_csv)



############################## test / train 나누기 #############
shuffled_data=data_csv[sample(1:nrow(data_csv)),] #데이터 섞기
shuffled_data[,"idx"]<-c(1:nrow(data_csv))  
attach(shuffled_data)
dim(data_csv) #행과 열의 개수

train=(idx<=2000)  #트레인데이터 2000개
shuffled_data.test=shuffled_data[!train,]
dim(shuffled_data.test)
Y.test=Y[!train]

Y.test   #테스트데이터 574개개



########### LOOCV 교차 검증


library(boot)
glm.fit=glm(Y~job+contact+month+duration+pdays+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx,data=data_csv)
cv.err=cv.glm(data_csv,glm.fit)
cv.err$delta



cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(Y~job+contact+month+duration+pdays+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx,data=data_csv)
  cv.error[i]=cv.glm(data_csv,glm.fit)$delta[1]
}

cv.error


################ k-fold 교차검증

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(Y~job+contact+month+duration+pdays+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx,data=data_csv)
  cv.error.10[i]=cv.glm(data_csv,glm.fit,K=10)$delta[1]
}
cv.error.10
mean(cv.error.10)



################# 부트스트랩
alpha.fn=function(data,index){
X=data$X[index]
Y=data$Y[index]
return((var(Y)-cov(X,Y))/var(X)+var(Y)-2*cov(X,Y))
}

alpha.fn(data_csv,1:100)









