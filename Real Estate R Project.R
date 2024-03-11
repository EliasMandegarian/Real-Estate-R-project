#This reads and creates the data set
library(car)
library(readxl)
setwd( dirname( rstudioapi::getActiveDocumentContext( )$path ) )
realEstateData = read.csv("Real Estate.csv")

#Shows the top rows and columns
head(realEstateData)

#catagorical variable set up
#AC
realEstateData$AC=factor(realEstateData$AC)
contrasts(realEstateData$AC)
#Pool
realEstateData$Pool=factor(realEstateData$Pool)
contrasts(realEstateData$Pool)
#Quality
realEstateData$Quality=factor(realEstateData$Quality)
realEstateData$Quality=relevel(realEstateData$Quality, ref='Low')
contrasts(realEstateData$Quality)

#Full model with all the predictors (double check categorical variables)
fullModel = lm(Price~Sqft + Bed + Bath + Age + Garage + Quality + AC + Pool, data = realEstateData)
summary(fullModel)

#VIF for the full model
vif(fullModel)




#Full Model with trainData
fullModelTrain = lm(Price~Sqft + Bed + Bath + Age + Garage + Quality + AC + Pool, data = traindata)
summary(fullModelTrain)


#Partial Model 1 w/ Sqft, Bed, Bath, and Age
reducedModel1 = lm(Price~Sqft+Bed+Bath+Age, data = traindata)
summary(reducedModel1)

#Partial Model 2 w/ Sqft, Bed, Bath, Age, Garage,Quality, AC, and Pool
reducedModel2 = lm(log(Price)~log(Sqft)+Bed+Bath+log(Age)+Garage+Quality, data = traindata)
summary(reducedModel2)
#vif for this model
vif(reducedModel2)

#Partial F test for model 2 seeing if AC and Pool adds to the model
anova(reducedModel2, transformFull1)
#p value .75, we can remove AC and Pool

#Partial Model 3 w/ log(Sqft), Bed, Bath, log(Age), Quality
reducedModel3 = lm(log(Price)~Sqft+Bed+Bath+Age+Quality, data = traindata)
summary(reducedModel3)

#Partial F test for model 3
anova(reducedModel3, transformFull1)
#P value =.14, garage is insignificant

#Partial Model 4 w/ Sqft, Bed, Age, and Quality
reducedModel4 = lm(log(Price)~log(Sqft)+Bed+log(Age)+Quality, data = traindata)
summary(reducedModel4)

#Partial Model 4 w/ Sqft, Bed, Age, and Quality on og data
reducedModel4OG = lm(log(Price)~log(Sqft)+Bed+log(Age)+Quality, data = realEstateData)
summary(reducedModel4)

reducedModel4notrans = lm(Price~Sqft+Bed+Age+Quality, data = traindata)
summary(reducedModel4notrans)

#boxcox 4
par(mfrow=c(1,1))
library(MASS)
boxcox(Price~Sqft+Bed+Age+Quality, data = traindata, lambda=seq(-1,1,by=0.01))


#Partial F test for model 4
anova(reducedModel4,fullModelTrain)
#P value =.14, garage is insignificant

#Partial model 5 w/ Sqft, Bath, Age, and quality
reducedModel5 = lm(log(Price)~log(Sqft)+Bath+log(Age)+Quality, data = traindata)
summary(reducedModel5)

#Partial F test for model 5
anova(reducedModel5,fullModelTrain)
#P value =.14, garage is insignificant

#Parital Model 6 w/ Sqft, Age, and Quality
reducedModel6 = lm(log(Price)~log(Sqft)+log(Age)+Quality, data = traindata)
summary(reducedModel6)

#Partial F test for model 6
anova(reducedModel6,reducedModel4)
#P value =.14, garage is insignificant


#Part 4 (80% of the data)
numtrain = ceiling(.8*522)
set.seed(123)
train_ind = sample(522,418)
traindata = realEstateData[train_ind, ]
testdata = realEstateData[-train_ind, ]
set.seed(NULL)




#Part 5


#cor graph

cor(realEstateData[,c('Price','Sqft','Bed','Bath','Age','Garage')])

#3d scatterplot
install.packages(rgl)
library(rgl)  
par(mfrow=c(1,1))
plot3d(realEstateData$Price, realEstateData$Sqft, realEstateData$Bed)


par(mfrow=c(2,2)) #plot in 2 by 2 grid
plot(Price~Sqft, data = traindata)
plot(Price~Bed, data = traindata)
plot(Price~Bath, data = traindata)
plot(Price~Age, data = traindata)

pairs(Price ~ Sqft + Bed + Bath + Age + Garage + Quality + AC + Pool, data = traindata)

#VIF Variable plots to determine colinearity
library(car)
avPlots(reducedModel1)

#resids
par(mfrow=c(2,2)) #plot in 2 by 2 grid
plot(resid(PaceFit) ~ Bank, data = Pace, ylab='Residuals', xlab='Bank')
abline(h=0,lty=2)


install.packages("leaps") 
library(leaps) 
all=lm(y~.,data=fullModelTrain)  
(best.sub=summary(regsubsets(formula(all), data=fullModel, method="exhaustive", nbest=2, nvmax=8)))



#Part 6


#response transformation graph
pairs(log(Price) ~ Sqft + Age, data = traindata)
testModel1 = lm(log(Price) ~ Sqft + Age, data = traindata)

par(mfrow=c(2,2)) #plot in 2 by 2 grid   
plot(testModel1)

#explanatory transformation graph
pairs(log(Price) ~ log(Sqft) + log(Age), data = traindata)
testModel2 = lm(log(Price) ~ log(Sqft) + log(Age), data = traindata)

par(mfrow=c(2,2)) #plot in 2 by 2 grid   
plot(testModel2)

#explanatory transformation graph
pairs(log(Price) ~ log(Sqft) + log(Age), data = traindata)


#Transformed Full Model w/ traindata
transformFull1 = lm(log(Price)~log(Sqft) + Bed + Bath + log(Age) + Garage + Quality + AC + Pool, data = traindata)
pairs(log(Price) ~ Age, data=traindata)
summary(transformFull1)
pairs(log(Price) ~ Sqft + logBed + Bath + Age + Garage + Quality, data=traindata)
par(mfrow=c(1,1))
plot(resid(transformFull1) ~ fitted(transformFull1), ylab='Residuals', xlab='Fitted')

par(mfrow=c(1,2))  #plot in 1 by 2 grid   
qqnorm(resid(transformFull1), ylab='Residuals'); qqline(resid(transformFull1))
hist(resid(transformFull1), main='', xlab='Residuals')

#transform reducedModel4
par(mfrow=c(1,2))  #plot in 1 by 2 grid   
qqnorm(resid(reducedModel4), ylab='Residuals'); qqline(resid(reducedModel4))
hist(resid(reducedModel4), main='', xlab='Residuals')

pairs(log(Price) ~ log(Sqft) + Bed + log(Age) + Quality, data = traindata)

#vif for this model
vif(reducedModel4)
#looks good
summary(reducedModel4)

par(mfrow=c(2,2))#plot in 2 by 2 grid   
plot(reducedModel4)
anova(reducedModel4)

#part 7

#Residual vs. predictors
plot(resid(reducedModel4)~log(Sqft),data=traindata,ylab="Residual")
abline(h=0,lty=2)

plot(resid(reducedModel4)~Bed,data=traindata,ylab="Residual")
abline(h=0,lty=2)

plot(resid(reducedModel4)~log(Age),data=traindata,ylab="Residual")
abline(h=0,lty=2)

plot(resid(reducedModel4)~Quality,data=traindata,ylab="Residual")
abline(h=0,lty=2)

#shapiro-wilk test
shapiro.test(resid(reducedModel4))

#Breusch-Pagan Test
library('lmtest')
bptest(reducedModel4)

#Frequency vs Residuals
hist(resid(reducedModel4),xlab="Residuals")

#part 10
predicted=predict(reducedModel4, realEstateData)
actual=realEstateData$Price
MSPE=mean((predicted-actual)^2)
