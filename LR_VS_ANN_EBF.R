################Comparison of LR and ANN with BDHS EBF Data#########################
#                            Mohammad Nayeem Hasan                                 #
####################################################################################

require(foreign)
require(MASS)
require(pROC)
require(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(maptools)
library(nnet)
library(FSA)
library(caret)
require(mapproj)
require(e1071)
## importing dataset

setwd('F:\\ResearchProject\\ARI_LR_ANN')

kr <- read.table("BF.csv",sep=',',header=T)

library(randomForest)
set.seed(222)
rf <- randomForest(kr$ï..ebf~., data=kr,
                   ntree = 500,
                   mtry = 5,
                   importance = TRUE,
                   proximity = TRUE)
print(rf)
attributes(rf)

print(importance(rf,type = 2)) 

# Prediction & Confusion Matrix - train data
library(caret)
detach(package:neuralnet)
pred <- predict(rf, kr, type = "response")

print(importance(rf,type = 2)) 
importance(n)
varImp()

plot(rf)

pred1 <- ifelse(pred>0.5, 1, 0)   ## if greatr than median value then 1 otherwise 0
pred1<- factor(pred1,levels=c(0,1),labels = c('No','Yes'))
kr$ebf <- factor(kr$ï..ebf,levels = c(0,1),labels = c('No','Yes'))

confusionMatrix(pred1, kr$ebf)

#auc value
library(ROCR)
detach(package:neuralnet)
#missing
sapply(kr,function(x) sum(is.na(x)))
pr <- prediction(as.numeric(pred1),as.numeric(kr$ebf))
perf <- performance(pr, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pr,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc

setwd('F:\\ResearchProject\\ARI_LR_ANN')

kr <- read.table("BF.csv",sep=',',header=T)


#LDA
model <- lda(factor(ï..ebf) ~ kr$Age_Group + kr$Division + kr$Residence +
               kr$Education + kr$Hus_edu + kr$Religion + kr$Wealth_index +
               kr$BMI_cat + kr$Child_sex + kr$C_sec + kr$Size_birth + kr$Age_Cat+
               kr$MassMedia,data=kr)

summary(model)
pred = predict(model, newdata=kr, type = "response")
tail(pred)
median(pred)

pred1 <- ifelse(pred$x>0.5, 1, 0)   ## if greatr than median value then 1 otherwise 0
pred1<- factor(pred1,levels=c(0,1),labels = c('No','Yes'))
kr$ebf <- factor(kr$ï..ebf,levels = c(0,1),labels = c('No','Yes'))
confusionMatrix(pred1,kr$ebf)   ## Original Yes=106 No=1926

#auc value
library(ROCR)
detach(package:neuralnet)
#missing
sapply(kr,function(x) sum(is.na(x)))
pr <- prediction(as.numeric(pred1),as.numeric(kr$ebf))
perf <- performance(pr, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pr,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc

#logis
setwd('F:\\ResearchProject\\ARI_LR_ANN')

kr <- read.table("BF.csv",sep=',',header=T)


model <- glm(ï..ebf ~ .,
             family=binomial(link='logit'),data=kr)

summary(model)
pred = predict(model, newdata=kr, type = "response")
tail(pred)
median(pred)

pred1 <- ifelse(pred>0.5, 1, 0)   ## if greatr than median value then 1 otherwise 0
pred1<- factor(pred1,levels=c(0,1),labels = c('No','Yes'))
kr$ebf <- factor(kr$ï..ebf,levels = c(0,1),labels = c('No','Yes'))
confusionMatrix(pred1,kr$ebf)   ## Original Yes=106 No=1926

#auc value
library(ROCR)
detach(package:neuralnet)
#missing
sapply(kr,function(x) sum(is.na(x)))
pr <- prediction(as.numeric(pred1),as.numeric(kr$ebf))
perf <- performance(pr, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pr,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc


############ ANN 5 #############

library(neuralnet)

kr <- read.table("BF.csv",sep=',',header=T)

summary(kr$ï..ebf)

set.seed(333)


#With 5 Hidden leyar
               
n <- neuralnet(ï..ebf ~ ., 
               data=kr,
               hidden = 5,
               threshold=0.1,
               
               act.fct = "logistic",
               linear.output = FALSE,
               lifesign='full',
               rep=5,
               algorithm="rprop+",
               learningrate.limit = NULL,
               learningrate.factor =
                 list(minus = 0.5, plus = 1.2),
               learningrate=NULL,
               stepmax=100000)


summary(n)


# Confusion Matrix & Misclassification Error - training data
output<-compute(n, kr)

pred <- output$net.result
actual<- kr$ï..ebf

MSE<- sum((pred-actual)^2)/nrow(kr)
MSE

pred1 <- ifelse(pred>0.5, 1, 0)   
pred1<- factor(pred1,levels=c(0,1),labels = c('No','Yes'))
kr$ebf <- factor(kr$ï..ebf,levels = c(0,1),labels = c('No','Yes'))  ## for confuson matrix we need to factor it

confusionMatrix(pred1,kr$ebf)   ## actual Yes=78 No=211

x <- as.numeric(actual)
pred1 <- as.numeric(pred1)

#auc value
library(ROCR)
detach(package:neuralnet)
pr <- prediction(as.numeric(pred1),as.numeric(kr$ebf))
perf <- performance(pr, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pr,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc


library(neuralnet)

kr <- read.table("BF.csv",sep=',',header=T)

summary(kr$ï..ebf)

set.seed(333)


#With 10 Hidden leyar

n <- neuralnet(ï..ebf ~ ., 
                data=kr,
                hidden = 10,
                threshold=0.1,
                
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign='full',
                rep=5,
                algorithm="rprop+",
                learningrate.limit = NULL,
                learningrate.factor =
                  list(minus = 0.5, plus = 1.2),
                learningrate=NULL,
                stepmax=100000)


summary(n)


# Confusion Matrix & Misclassification Error - training data
output<-compute(n, kr)

pred <- output$net.result
actual<- kr$ï..ebf

MSE<- sum((pred-actual)^2)/nrow(kr)
MSE

pred1 <- ifelse(pred>0.5, 1, 0)   
pred1<- factor(pred1,levels=c(0,1),labels = c('No','Yes'))
kr$ebf <- factor(kr$ï..ebf,levels = c(0,1),labels = c('No','Yes'))  ## for confuson matrix we need to factor it

confusionMatrix(pred1,kr$ebf)   ## actual Yes=78 No=211

x <- as.numeric(actual)
pred1 <- as.numeric(pred1)

#auc value
library(ROCR)
detach(package:neuralnet)
pr <- prediction(as.numeric(pred1),as.numeric(kr$ebf))
perf <- performance(pr, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pr,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc


library(neuralnet)

kr <- read.table("BF.csv",sep=',',header=T)

summary(kr$ï..ebf)

set.seed(333)


#With 15 Hidden leyar

n <- neuralnet(ï..ebf ~ ., 
                data=kr,
                hidden = 15,
                threshold=0.1,
                
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign='full',
                rep=5,
                algorithm="rprop+",
                learningrate.limit = NULL,
                learningrate.factor =
                  list(minus = 0.5, plus = 1.2),
                learningrate=NULL,
                stepmax=100000)


summary(n)


# Confusion Matrix & Misclassification Error - training data
output<-compute(n, kr)

pred <- output$net.result
actual<- kr$ï..ebf

MSE<- sum((pred-actual)^2)/nrow(kr)
MSE

pred1 <- ifelse(pred>0.5, 1, 0)   
pred1<- factor(pred1,levels=c(0,1),labels = c('No','Yes'))
kr$ebf <- factor(kr$ï..ebf,levels = c(0,1),labels = c('No','Yes'))  ## for confuson matrix we need to factor it

confusionMatrix(pred1,kr$ebf)   ## actual Yes=78 No=211

x <- as.numeric(actual)
pred1 <- as.numeric(pred1)

#auc value
library(ROCR)
detach(package:neuralnet)
pr <- prediction(as.numeric(pred1),as.numeric(kr$ebf))
perf <- performance(pr, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pr,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc


library(neuralnet)

kr <- read.table("BF.csv",sep=',',header=T)

summary(kr$ï..ebf)

set.seed(333)


#With 20 Hidden leyar

n <- neuralnet(ï..ebf ~ ., 
                data=kr,
                hidden = 20,
                threshold=0.1,
                
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign='full',
                rep=5,
                algorithm="rprop+",
                learningrate.limit = NULL,
                learningrate.factor =
                  list(minus = 0.5, plus = 1.2),
                learningrate=NULL,
                stepmax=100000)


summary(n)


# Confusion Matrix & Misclassification Error - training data
output<-compute(n, kr)

pred <- output$net.result
actual<- kr$ï..ebf

MSE<- sum((pred-actual)^2)/nrow(kr)
MSE

pred1 <- ifelse(pred>0.5, 1, 0)   
pred1<- factor(pred1,levels=c(0,1),labels = c('No','Yes'))
kr$ebf <- factor(kr$ï..ebf,levels = c(0,1),labels = c('No','Yes'))  ## for confuson matrix we need to factor it

confusionMatrix(pred1,kr$ebf)   ## actual Yes=78 No=211

x <- as.numeric(actual)
pred1 <- as.numeric(pred1)

#auc value
library(ROCR)
detach(package:neuralnet)
pr <- prediction(as.numeric(pred1),as.numeric(kr$ebf))
perf <- performance(pr, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pr,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc


library(neuralnet)

kr <- read.table("BF.csv",sep=',',header=T)

summary(kr$ï..ebf)

set.seed(333)


#With 25 Hidden leyar

n <- neuralnet(ï..ebf ~ ., 
                data=kr,
                hidden = 25,
                threshold=0.1,
                
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign='full',
                rep=5,
                algorithm="rprop+",
                learningrate.limit = NULL,
                learningrate.factor =
                  list(minus = 0.5, plus = 1.2),
                learningrate=NULL,
                stepmax=100000)


summary(n)


# Confusion Matrix & Misclassification Error - training data
output<-compute(n, kr)

pred <- output$net.result
actual<- kr$ï..ebf

MSE<- sum((pred-actual)^2)/nrow(kr)
MSE

pred1 <- ifelse(pred>0.5, 1, 0)   
pred1<- factor(pred1,levels=c(0,1),labels = c('No','Yes'))
kr$ebf <- factor(kr$ï..ebf,levels = c(0,1),labels = c('No','Yes'))  ## for confuson matrix we need to factor it

confusionMatrix(pred1,kr$ebf)   ## actual Yes=78 No=211

x <- as.numeric(actual)
pred1 <- as.numeric(pred1)

#auc value
library(ROCR)
detach(package:neuralnet)
pr <- prediction(as.numeric(pred1),as.numeric(kr$ebf))
perf <- performance(pr, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pr,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc


library(neuralnet)

kr <- read.table("BF.csv",sep=',',header=T)

summary(kr$ï..ebf)

set.seed(333)


#With 30 Hidden leyar

n <- neuralnet(ï..ebf ~ ., 
                data=kr,
                hidden = 30,
                threshold=0.1,
                
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign='full',
                rep=5,
                algorithm="rprop+",
                learningrate.limit = NULL,
                learningrate.factor =
                  list(minus = 0.5, plus = 1.2),
                learningrate=NULL,
                stepmax=100000)


summary(n)


# Confusion Matrix & Misclassification Error - training data
output<-compute(n, kr)

pred <- output$net.result
actual<- kr$ï..ebf

MSE<- sum((pred-actual)^2)/nrow(kr)
MSE

pred1 <- ifelse(pred>0.5, 1, 0)   
pred1<- factor(pred1,levels=c(0,1),labels = c('No','Yes'))
kr$ebf <- factor(kr$ï..ebf,levels = c(0,1),labels = c('No','Yes'))  ## for confuson matrix we need to factor it

confusionMatrix(pred1,kr$ebf)   ## actual Yes=78 No=211

x <- as.numeric(actual)
pred1 <- as.numeric(pred1)

#auc value
library(ROCR)
detach(package:neuralnet)
pr <- prediction(as.numeric(pred1),as.numeric(kr$ebf))
perf <- performance(pr, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pr,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc

library(caret)

print(importance(n,type = 2)) 
importance(n)
varImp()
require(devtools)
#import 'gar.fun' from Github
install_github("gar.fun")
source_gist('6206737')
kr
#create a pretty color vector for the bar plot
cols<-colorRampPalette(c('lightgreen','lightblue'))(14)
#use the function on the model created above
par(mar=c(3,4,1,1),family='serif')
gar.fun('y',n,col=cols,ylab='Rel. importance',ylim=c(-1,1))

library(neuralnet)

kr <- read.table("BF.csv",sep=',',header=T)

summary(kr$ï..ebf)

set.seed(333)


#With 35 Hidden leyar

n <- neuralnet(ï..ebf ~ ., 
                data=kr,
                hidden = 35,
                threshold=0.1,
                
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign='full',
                rep=5,
                algorithm="rprop+",
                learningrate.limit = NULL,
                learningrate.factor =
                  list(minus = 0.5, plus = 1.2),
                learningrate=NULL,
                stepmax=100000)


summary(n)


# Confusion Matrix & Misclassification Error - training data
output<-compute(n, kr)

pred <- output$net.result
actual<- kr$ï..ebf

MSE<- sum((pred-actual)^2)/nrow(kr)
MSE

pred1 <- ifelse(pred>0.5, 1, 0)   
pred1<- factor(pred1,levels=c(0,1),labels = c('No','Yes'))
kr$ebf <- factor(kr$ï..ebf,levels = c(0,1),labels = c('No','Yes'))  ## for confuson matrix we need to factor it

confusionMatrix(pred1,kr$ebf)   ## actual Yes=78 No=211

x <- as.numeric(actual)
pred1 <- as.numeric(pred1)

#auc value
library(ROCR)
detach(package:neuralnet)
pr <- prediction(as.numeric(pred1),as.numeric(kr$ebf))
perf <- performance(pr, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pr,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc


library(neuralnet)

kr <- read.table("BF.csv",sep=',',header=T)

summary(kr$ï..ebf)

set.seed(333)


#With 40 Hidden leyar

n <- neuralnet(ï..ebf ~ ., 
                data=kr,
                hidden = 40,
                threshold=0.1,
                
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign='full',
                rep=5,
                algorithm="rprop+",
                learningrate.limit = NULL,
                learningrate.factor =
                  list(minus = 0.5, plus = 1.2),
                learningrate=NULL,
                stepmax=100000)


summary(n)


# Confusion Matrix & Misclassification Error - training data
output<-compute(n, kr)

pred <- output$net.result
actual<- kr$ï..ebf

MSE<- sum((pred-actual)^2)/nrow(kr)
MSE

pred1 <- ifelse(pred>0.5, 1, 0)   
pred1<- factor(pred1,levels=c(0,1),labels = c('No','Yes'))
kr$ebf <- factor(kr$ï..ebf,levels = c(0,1),labels = c('No','Yes'))  ## for confuson matrix we need to factor it

confusionMatrix(pred1,kr$ebf)   ## actual Yes=78 No=211

x <- as.numeric(actual)
pred1 <- as.numeric(pred1)

#auc value
library(ROCR)
detach(package:neuralnet)
pr <- prediction(as.numeric(pred1),as.numeric(kr$ebf))
perf <- performance(pr, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pr,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc


library(neuralnet)

kr <- read.table("BF.csv",sep=',',header=T)

summary(kr$ï..ebf)

set.seed(333)


#With 45 Hidden leyar

n <- neuralnet(ï..ebf ~ ., 
                data=kr,
                hidden = 45,
                threshold=0.1,
                
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign='full',
                rep=5,
                algorithm="rprop+",
                learningrate.limit = NULL,
                learningrate.factor =
                  list(minus = 0.5, plus = 1.2),
                learningrate=NULL,
                stepmax=100000)


summary(n)


# Confusion Matrix & Misclassification Error - training data
output<-compute(n, kr)

pred <- output$net.result
actual<- kr$ï..ebf

MSE<- sum((pred-actual)^2)/nrow(kr)
MSE

pred1 <- ifelse(pred>0.5, 1, 0)   
pred1<- factor(pred1,levels=c(0,1),labels = c('No','Yes'))
kr$ebf <- factor(kr$ï..ebf,levels = c(0,1),labels = c('No','Yes'))  ## for confuson matrix we need to factor it

confusionMatrix(pred1,kr$ebf)   ## actual Yes=78 No=211

x <- as.numeric(actual)
pred1 <- as.numeric(pred1)

#auc value
library(ROCR)
detach(package:neuralnet)
pr <- prediction(as.numeric(pred1),as.numeric(kr$ebf))
perf <- performance(pr, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pr,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc


library(neuralnet)

kr <- read.table("BF.csv",sep=',',header=T)

summary(kr$ï..ebf)

set.seed(333)


#With 50 Hidden leyar

n <- neuralnet(ï..ebf ~ ., 
                data=kr,
                hidden = 50,
                threshold=0.1,
                
                act.fct = "logistic",
                linear.output = FALSE,
                lifesign='full',
                rep=5,
                algorithm="rprop+",
                learningrate.limit = NULL,
                learningrate.factor =
                  list(minus = 0.5, plus = 1.2),
                learningrate=NULL,
                stepmax=100000)


summary(n)


# Confusion Matrix & Misclassification Error - training data
output<-compute(n, kr)

pred <- output$net.result
actual<- kr$ï..ebf

MSE<- sum((pred-actual)^2)/nrow(kr)
MSE

pred1 <- ifelse(pred>0.5, 1, 0)   
pred1<- factor(pred1,levels=c(0,1),labels = c('No','Yes'))
kr$ebf <- factor(kr$ï..ebf,levels = c(0,1),labels = c('No','Yes'))  ## for confuson matrix we need to factor it

confusionMatrix(pred1,kr$ebf)   ## actual Yes=78 No=211

x <- as.numeric(actual)
pred1 <- as.numeric(pred1)

#auc value
library(ROCR)
detach(package:neuralnet)
pr <- prediction(as.numeric(pred1),as.numeric(kr$ebf))
perf <- performance(pr, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pr,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc

