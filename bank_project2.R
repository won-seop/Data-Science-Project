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
names(data_csv)
dim(data_csv) #행과 열의 개수

is.na(data_csv)

########################분활하지않고 glm활용#########################

glm.fit <- glm(Y~age+job+marital+education+default+housing+loan+contact+month+day_of_week
               +duration+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx
               +cons.conf.idx+euribor3m+nr.employed,data_csv,family=binomial())
summary(glm.fit)

coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]
glm.probs=predict(glm.fit, type="response")
glm.probs[1:10]
glm.pred=rep(0,2574)
glm.pred[glm.probs >.5]=1
table(glm.pred, Y)
(617+648)/2574
mean(glm.pred==Y)


###################분활하고 glm사용######################



train=(idx<=2000)
shuffled_data.test=shuffled_data[!train,]
dim(shuffled_data.test)
Y.test=Y[!train]

Y.test    #테스트 랜덤 데이터 574개

glm.fit <- glm(Y~age+job+marital+education+default+housing+loan+contact+month+day_of_week
               +duration+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx
               +cons.conf.idx+euribor3m+nr.employed,data_csv,family=binomial())
glm.probs=predict(glm.fit, shuffled_data.test,type="response")
glm.probs[1:10]
glm.pred=rep(0, 574)
glm.pred[glm.probs>.5]=1
table(glm.pred, Y.test)
(139+147)/574
mean(glm.pred==Y.test)
mean(glm.pred!=Y.test)

#상관성이 높은 변수만 glm적용#################################
summary(glm.fit)
glm.fit=glm(Y~duration+pdays+emp.var.rate+cons.conf.idx, data = shuffled_data, family = binomial, subset = train)
glm.probs=predict(glm.fit, shuffled_data.test, type = "response")
glm.pred=rep(0, 574)
glm.pred[glm.probs>.5]=1
table(glm.pred, Y.test)
(145+147)/574
mean(glm.pred==Y.test)


#############LDA#################

lda.fit=lda(Y~contact+month+duration+pdays+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed, data = shuffled_data, subset = train)
lda.fit
lda.pred=predict(lda.fit, shuffled_data.test)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class, Y.test)

mean(lda.class==Y.test)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)


######QDA####################################################

qda.fit=qda(Y~contact+month+duration+pdays+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed, data = shuffled_data, subset = train)
qda.fit
qda.class=predict(qda.fit, shuffled_data.test)$class
table(qda.class, Y.test)
mean(qda.class==Y.test)

##############KNN###############################################


train.X=cbind(month,duration,pdays,poutcome,emp.var.rate,cons.price.idx,cons.conf.idx,euribor3m,nr.employed)[train,]
test.X=cbind(month,duration,pdays,poutcome,emp.var.rate,cons.price.idx,cons.conf.idx,euribor3m,nr.employed)[!train,]
train.Y=Y[train]

set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
table(knn.pred, Y.test)
mean(knn.pred==Y.test)
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred, Y.test)
mean(knn.pred==Y.test)
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred, Y.test)
mean(knn.pred==Y.test)

