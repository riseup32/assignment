titanic <- read.csv('C:/Users/User/Desktop/titanic_train.csv')
str(titanic)
head(titanic)
titanic <- titanic[,c(2,3,5,6,7,8,10,12)]

summary(titanic)
titanic$Embarked[!(titanic$Embarked=='C' | titanic$Embarked=='Q' | titanic$Embarked=='S')] <- 'S'

for(i in 1:nrow(titanic)){
 if(titanic$Sex[i]=='male') {titanic$Sex2[i] <- 1}
 else {titanic$Sex2[i] <- 2}
}

for(i in 1:nrow(titanic)){
 if(titanic$Embarked[i]=='C') {titanic$Embarked2[i] <- 1}
 else if(titanic$Embarked[i]=='Q') {titanic$Embarked2[i] <- 2}
 else {titanic$Embarked2[i] <- 3}
}

age.lm <- lm(Age~Pclass+SibSp,data=titanic)
agelist <- titanic[,c(2,5)]
for(i in 1:nrow(titanic)){
 if(is.na(titanic$Age[i])==1) {titanic$Age[i] <- predict(age.lm,agelist)}
}
summary(titanic$Age)


titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex2 <- as.factor(titanic$Sex2)
titanic$Embarked2 <- as.factor(titanic$Embarked2)
titanic$Survived <- as.factor(titanic$Survived)

titanic.logic <- glm(Survived~Age+SibSp+Parch+Fare+Pclass+Sex2+Embarked2,data=titanic,family='binomial')
summary(titanic.logic)

titanic.AIC <- step(titanic.logic,direction='backward')
summary(titanic.AIC)
par(mfrow=c(2,2))
plot(titanic.AIC)

boxplot(rstandard(titanic.AIC)~titanic$Sex2)
boxplot(rstandard(titanic.AIC)~titanic$Pclass)
boxplot(rstandard(titanic.AIC)~titanic$Embarked2)

sex.weights = 0 * titanic$Survived
for (sex in levels(titanic$Sex)) {
 subset.sex = (titanic$Sex == sex)
 sex.weights[subset.sex] <- 1.0 / (sum(resid(titanic.AIC)[subset.sex]^2) / sum(subset.sex))
}
unique(sex.weights) # ³² ¿©

pclass.weights = 0 * titanic$Survived
for (pclass in levels(titanic$Pclass)) {
 subset.pclass = (titanic$Pclass == pclass)
 pclass.weights[subset.pclass] <- 1.0 / (sum(resid(titanic.AIC)[subset.pclass]^2) / sum(subset.pclass))
}
unique(pclass.weights) # 3 1 2

final.weights <- 0 * titanic$Survived
for(i in 1:nrow(titanic)){
 if(titanic$Sex[i]=='male' & titanic$Pclass[i]=='1') {final.weights[i] <- unique(sex.weights)[1]*unique(pclass.weights)[2]}
 else if(titanic$Sex[i]=='male' & titanic$Pclass[i]=='2') {final.weights[i] <- unique(sex.weights)[1]*unique(pclass.weights)[3]}
 else if(titanic$Sex[i]=='male' & titanic$Pclass[i]=='3') {final.weights[i] <- unique(sex.weights)[1]*unique(pclass.weights)[1]}
 else if(titanic$Sex[i]=='female' & titanic$Pclass[i]=='1') {final.weights[i] <- unique(sex.weights)[2]*unique(pclass.weights)[2]}
 else if(titanic$Sex[i]=='female' & titanic$Pclass[i]=='2') {final.weights[i] <- unique(sex.weights)[2]*unique(pclass.weights)[3]}
 else {final.weights[i] <- unique(sex.weights)[2]*unique(pclass.weights)[1]}
}
unique(final.weights)

titanic.weight.lm <- glm(Survived~Age+SibSp+Pclass+Sex2+Embarked2,weights=final.weights,data=titanic,family='binomial')
summary(titanic.weight.lm)
par(mfrow=c(2,2))
plot(titanic.weight.lm)

boxplot(rstandard(titanic.weight.lm)~titanic$Sex2)
boxplot(rstandard(titanic.weight.lm)~titanic$Pclass)

#####################################################
test <- read.csv('C:/Users/User/Desktop/titanic_test.csv')
str(test)
summary(test)

for(i in 1:nrow(test)){
 if(test$Sex[i]=='male') {test$Sex2[i] <- 1}
 else {test$Sex2[i] <- 2}
}

for(i in 1:nrow(test)){
 if(test$Embarked[i]=='C') {test$Embarked2[i] <- 1}
 else if(test$Embarked[i]=='Q') {test$Embarked2[i] <- 2}
 else {test$Embarked2[i] <- 3}
}

agelist <- test[,c(1,4)]
for(i in 1:nrow(test)){
 if(is.na(test$Age[i])==1) {test$Age[i] <- predict(age.lm,test)}
}
summary(test$Age)

test$Pclass <- as.factor(test$Pclass)
test$Sex2 <- as.factor(test$Sex2)
test$Embarked2 <- as.factor(test$Embarked2)

k.fold.cv <- function(k,data){
n <- round((nrow(data)/k)-0.5)
sum.mse <- 0
for(i in 1:k){
 m <- (n*(i-1)+1):(n*i)
 cv.test <- data[m,]
 cv.train <- data[-m,]
 data.model <- glm(Survived~Age+SibSp+Pclass+Sex2+Embarked2,data=cv.train,family='binomial') # model
 predict.value <- predict(data.model,cv.test)
 predict.value <- round(1/(1+exp(-predict.value)))
 predict.value <- as.factor(predict.value)
 mse <- mean((as.numeric(predict.value)-as.numeric(cv.test$Survived))^2)
 sum.mse <- sum.mse+mse
 }
return(sum.mse)
}
k.fold.cv(10,titanic)


predict <- predict(titanic.weight.lm,test)
logic <- 1/(1+exp(-predict))
value <- 0*logic
for(i in 1:length(value)){
 if(logic[i]>=0.5) {value[i] <- 1}
 else {value[i] <- 0}
}

value <- as.factor(value)

survive <- read.csv('C:/Users/User/Desktop/gender_submission.csv')
survive[,2] <- as.factor(survive[,2])
sum(as.numeric(survive[,2])==as.numeric(value))/nrow(survive)

#####################################################
library(party)

k.fold.cv <- function(k,data){
n <- round((nrow(data)/k)-0.5)
sum.mse <- 0
for(i in 1:k){
 m <- (n*(i-1)+1):(n*i)
 cv.test <- data[m,]
 cv.train <- data[-m,]
 data.model <- ctree(Survived~Age+SibSp+Pclass+Sex2+Embarked2,data=cv.train) # model
 predict.value <- predict(data.model,cv.test)
 mse <- mean((as.numeric(predict.value)-as.numeric(cv.test$Survived))^2)
 sum.mse <- sum.mse+mse
 }
return(sum.mse)
}
k.fold.cv(10,titanic)

dt.fit <- ctree(Survived~Age+SibSp+Pclass+Sex2+Embarked2,data=titanic)
plot(dt.fit)
dt_pred <- predict(dt.fit,test)
sum(survive[,2]==dt_pred)/nrow(survive)

#####################################################
library(randomForest)

k.fold.cv <- function(k,data){
n <- round((nrow(data)/k)-0.5)
sum.mse <- 0
for(i in 1:k){
 m <- (n*(i-1)+1):(n*i)
 cv.test <- data[m,]
 cv.train <- data[-m,]
 data.model <- randomForest(Survived~Age+SibSp+Pclass+Sex2+Embarked2,data=cv.train,mtry=3,ntree=400,importance=T) # model
 predict.value <- predict(data.model,cv.test)
 mse <- mean((as.numeric(predict.value)-as.numeric(cv.test$Survived))^2)
 sum.mse <- sum.mse+mse
 }
return(sum.mse)
}
k.fold.cv(10,titanic)

rf.fit = randomForest(Survived~Age+SibSp+Pclass+Sex2+Embarked2,data=titanic,mtry=3,ntree=400,importance=T)
rf_pred <- predict(rf.fit,test)
importance(rf.fit)
sum(survive[,2]==rf_pred)/nrow(survive)

#####################################################
library(e1071)

k.fold.cv <- function(k,data){
n <- round((nrow(data)/k)-0.5)
sum.mse <- 0
for(i in 1:k){
 m <- (n*(i-1)+1):(n*i)
 cv.test <- data[m,]
 cv.train <- data[-m,]
 data.model <- naiveBayes(Survived~Age+SibSp+Pclass+Sex2+Embarked2,data=cv.train) # model
 predict.value <- predict(data.model,cv.test)
 mse <- mean((as.numeric(predict.value)-as.numeric(cv.test$Survived))^2)
 sum.mse <- sum.mse+mse
 }
return(sum.mse)
}
k.fold.cv(10,titanic)

nb.fit = naiveBayes(Survived~Age+SibSp+Pclass+Sex2+Embarked2,data=titanic)
nb_pred = predict(nb.fit,test)
sum(survive[,2]==nb_pred)/nrow(survive)