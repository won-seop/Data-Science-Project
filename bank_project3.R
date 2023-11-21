library(ISLR)
library(MASS)
library(class)
library(leaps)

data_csv <- read.csv("C:\\Users\\tpmgt\\Desktop\\데사 팀플 최종\\final_data_bank.csv", header=TRUE)

data_csv <- data_csv[-1] #전처리과정에서 생긴 순서 열 제거
View(data_csv)

shuffled_data=data_csv[sample(1:nrow(data_csv)),] #데이터 섞기
shuffled_data[,"idx"]<-c(1:nrow(data_csv))
attach(shuffled_data)
train=(idx<=2000)  #트레인데이터 2000개
shuffled_data.test=shuffled_data[!train,]
dim(shuffled_data.test)
Y.test=Y[!train]
Y.test   #테스트데이터 574개



#########최적의 변수 찾기############################ 교재 285부터터
regfit.full=regsubsets(Y~.,data=data_csv,nvmax=20)# 변수수 20개 까지 보여주기
summary(regfit.full)
reg.summary=summary(regfit.full) # 관련변수 출력
names(reg.summary) # 조정된 R^2 Cp 그리고 BIC
reg.summary$rsq
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of variables", ylab="RSS", type="l")
plot(reg.summary$adjr2,xlab="Number of variables", ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)  # R^2통계량이 가장 큰 모델을 표시하기 위해
points(14,reg.summary$adjr2[14],col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of Variables", ylab="Cp",type="l")
which.min(reg.summary$cp)
points(11,reg.summary$cp[11],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab="Number of Variables",ylab="BIC",type="l")
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

#이 모델에 따른 계수 추정치
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

coef(regfit.full,6)

#전진 단계적 후진 단계적
regfit.fwd=regsubsets(Y~.,data=data_csv,nvmax=20,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Y~.,data=data_csv,nvmax=20,method="backward")
summary(regfit.bwd)






###검증셋 기법 최상의 서브셋
regfit.best=regsubsets(Y~.,data=shuffled_data[train,],nvmax=20)
test.mat=model.matrix(Y~.,data=shuffled_data[Y.test,])

val.errors=rep(NA,20)
for(i in 1:20){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((shuffled_data$Y[Y.test]-pred)^2)
}

val.errors
which.min(val.errors)
coef(regfit.best,9)


#교차검증 최상의 서브셋
predict.regsubsets=function (object , newdata ,id ,...){
  + form=as.formula (object$call [[2]])
  + mat=model.matrix(form ,newdata )
  + coefi=coef(object ,id=id)
  + xvars=names(coefi)
  + mat[,xvars]%*%coefi}

regfit.best=regsubsets(Y~.,data=data_csv,nvmax=20)
coef(regfit.best,2)

k=10
set.seed(1)
folds=sample (1:k,nrow(data_csv),replace=TRUE)
cv.errors =matrix (NA,k,19, dimnames =list(NULL , paste (1:19) ))

for(j in 1:k){
  best.fit=regsubsets(Y~.,data=data_csv[folds!=j,],nvmax=20)
for(i in 1:20){
    pred=predict(best.fit,data_csv[folds==j,],id=i)
    cv.errors[j,i]=mean((data_csv$Y[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mtrow=c(1,1))
plot(mean.cv.errors,type='b')




regfit.best=regsubsets (Y~.,data=data_csv ,nvmax=19)
coef(reg.best ,10)

#####능형회귀

x=model.matrix(Y~.,data_csv)[,-1]
y=data_csv$Y

library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lanbda=grid)

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod,s=50,type="coefficients")[1:20,]

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)

ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x,subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]

#Lasso
x=model.matrix(Y~.,data_csv)[,-1]
y=data_csv$Y

library(glmnet)
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
#왠그래프징 ?

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]




