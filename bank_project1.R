#170p부터 로지스틱회귀 / LDA / QDA / KNN / k-fold / LOOCV

data_csv <- read.csv("C:\\Users\\tpmgt\\Desktop\\데이터사이언스 팀플 관련\\final_data_bank.csv", header=TRUE)
data_csv <- data_csv[-1] #전처리과정에서 생긴 순서 열 제거
View(data_csv)
names(data_csv)
dim(data_csv) #행과 열의 개수

table(data_csv$y)


glm.fit <- glm(Y~age+job+marital+education+default+housing+loan+contact+month+day_of_week
               +duration+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx
               +cons.conf.idx+euribor3m+nr.employed,data_csv,family=binomial())
#summary(model.glm)

summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]
glm.probs=predict(glm.fit, type="response")
glm.probs[1:10]
#View(data_csv)
glm.pred=rep(0,2574)
glm.pred[glm.probs >.5]=1
table(glm.pred, Y)
mean(glm.pred==Y)
###################################################################################

shuffled_data=data_csv[sample(1:nrow(data_csv)),]
shuffled_data[,"idx"]<-c(1:nrow(data_csv))
detach(data_csv)
attach(shuffled_data)

train=(idx<=2000)
shuffled_data.test=shuffled_data[!train,]
dim(shuffled_data.test)
Y.test=Y[!train]

glm.fit <- glm(Y~age+job+marital+education+default+housing+loan+contact+month+day_of_week
                 +duration+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx
                 +cons.conf.idx+euribor3m+nr.employed,data_csv,family=binomial())
glm.probs[1:10]
glm.pred=rep(0, 574)
glm.pred[glm.probs>.5]=1
table(glm.pred, Y.test)
mean(glm.pred==Y.test)
mean(glm.pred!=Y.test)

###################LDA###########################################################################

library(MASS)
lda.fit=lda(Y~age+job+marital+education+default+housing+loan+contact+month+day_of_week
            +duration+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx
            +cons.conf.idx+euribor3m+nr.employed, data = data_csv, subset = train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, shuffled_data.test)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class, Y.test)
mean(lda.class==Y.test)
sum(lda.pred$Y[,1]>=.5)
sum(lda.pred$Y[,1]<.5)
lda.pred$Y[1:20,1]
lda.class[1:20]
sum(lda.pred$Y[,1]>.9)


####################QDA###################################################################


qda.fit=qda(Y~age+job+marital+education+default+housing+loan+contact+month+day_of_week
            +duration+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx
            +cons.conf.idx+euribor3m+nr.employed, data = data_csv, subset = train)
qda.fit
qda.class=predict(qda.fit, shuffled_data.test)$class
table(qda.class, Y.test)
mean(qda.class==Y.test)



#################KNN#########################################################################






