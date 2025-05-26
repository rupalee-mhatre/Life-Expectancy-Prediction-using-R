
#### Project Life expectancy analysis
#### Submitted by: Rupalee Mhatre


####### This portion includes using original dataset with missing values. Suggestion will be to start from around line 42. 
##That is the point where the analysis begins.

dataLE<-read.csv("Life Expectancy Data.csv")
summary(dataLE[,17:22])

sum(is.na(dataLE[,4]))
delRows1<-which(is.na(dataLE[,4]))
delRows<-dataLE[which(is.na(dataLE[,4])),]

###Omitted 10 records where LE was missing

delRows
dataLE<-dataLE[-c(delRows1),]
dataLE

summary(dataLE)

dataLE[which(is.na(dataLE[,18])),]
#dataLE<-dataLE[,-c(7)]
dataLE
######### Mice Imputation

library(mice)
md.pattern(dataLE[])

dataLE_final=mice(dataLE[],m=3,meth='cart',seed=123)


data_final<-complete(dataLE_final,3)

sum(is.na(data_final))

write.csv(data_final,file="LifeExpectancy_Final.csv")


######################################################################
######################################################################
      ###START Here
dataLE<-read.csv("LifeExpectancy_Final.csv")
summary(dataLE)
names(dataLE)


###Converting the status column into two binary columns
dataLE$Status1<-ifelse(dataLE$Status=='Developed',1,0)
#dataLE$Status2<-ifelse(dataLE$Status=='Developing',1,0)
dataLE
summary(dataLE[,24])

#install.packages("corrplot")

library(corrplot)
 corrln<-cor(dataLE[,c(5:24)])
 head(round(corrln,2))
 corrplot(corrln,method="color")
 
 library(car)
 
 Model1<-lm(Life.expectancy~Adult.Mortality+infant.deaths+Alcohol+percentage.expenditure+Hepatitis.B+Measles+BMI+under.five.deaths+Polio+Total.expenditure+Diphtheria+HIV.AIDS+GDP+Population+thinness..1.19.years+thinness.5.9.years+Income.composition.of.resources+Schooling+Status1,dataLE)
summary(Model1) 
vif(Model1)

###VIF infant deaths = 176 so removing that from the model

Model2<-lm(Life.expectancy~Adult.Mortality+Alcohol+percentage.expenditure+Hepatitis.B+Measles+BMI+under.five.deaths+Polio+Total.expenditure+Diphtheria+HIV.AIDS+GDP+Population+thinness..1.19.years+thinness.5.9.years+Income.composition.of.resources+Schooling+Status1,dataLE)
summary(Model2) 
vif(Model2)

###VIF thinness 5-9 is 8  so removing that from the model
Model3<-lm(Life.expectancy~Adult.Mortality+Alcohol+percentage.expenditure+Hepatitis.B+Measles+BMI+under.five.deaths+Polio+Total.expenditure+Diphtheria+HIV.AIDS+GDP+Population+thinness..1.19.years+Income.composition.of.resources+Schooling+Status1,dataLE)
summary(Model3) 
vif(Model3)

#### Consider the P values of each variables and remove the once with P value>0.05
###Remove Alcohol, Hep B,Measles,Tot Expenditure,Population,GDP

Model4<-lm(Life.expectancy~Adult.Mortality+percentage.expenditure+BMI+under.five.deaths+Polio+Diphtheria+HIV.AIDS+thinness..1.19.years+Income.composition.of.resources+Schooling+Status1,dataLE)
summary(Model4) 
vif(Model4)

Model4$residuals
sum(Model4$residuals)
qqnorm(Model4$residuals)
qqline(Model4$residuals)

plot(Model4$residuals~Model4$fitted.values)
anova(Model4)
library(MPV)
PRESS(Model4)
AIC(Model4)
BIC(Model4)

############## Get the outliers
library(MASS)
studres(Model4)
which((studres(Model4))>3)
dataLE[75,]
dataLE2[75,]
dataLE[c(75,76,77,78,79,80,336,1741,1742,1743,1744,2086,2087,2192,2494,2495),]
dataLE2<-dataLE[-c(75,76,77,78,79,80,336,1741,1742,1743,1744,2086,2087,2192,2494,2495),]

####Model after removing the outliers

Model5<-lm(Life.expectancy~Adult.Mortality+percentage.expenditure+BMI+under.five.deaths+Polio+Diphtheria+HIV.AIDS+thinness..1.19.years+Income.composition.of.resources+Schooling+Status1,dataLE2)
summary(Model5) 
confint(Model5)
formula(Model5)
vif(Model5)

#Model4$residuals
sum(Model5$residuals)
qqnorm(Model5$residuals)
qqline(Model5$residuals)



plot(Model5$residuals~Model5$fitted.values)
anova(Model5)
library(MPV)
PRESS(Model5)


####Model5 showed that Thinness 1-19 was not significant so removed it

####Remove India
#dataLE2<-dataLE2[-c(1185:1200),]

Model6<-lm(Life.expectancy~Adult.Mortality+percentage.expenditure+BMI+under.five.deaths+Polio+Diphtheria+HIV.AIDS+Income.composition.of.resources+Schooling+Status1,dataLE2)
summary(Model6) 
confint(Model6)
formula(Model6)
vif(Model6)

#Model4$residuals
sum(Model6$residuals)
qqnorm(Model6$residuals)
qqline(Model6$residuals)

plot(Model6$residuals~Model6$fitted.values)
anova(Model6)
library(MPV)
PRESS(Model6)
AIC(Model6)
AIC(Model5)
BIC(Model6)
BIC(Model5)

######## The residual vs normal plot has non-constant variance for lower values
###### So using Boxcox transformations

dataLE2[(which(Model5$fitted.values<50)),]

boxcox(Model6,lambda=seq(0,5.0,0.1))

#maximum likelihood value for lambda= 1.9

dataLE2$TLE<-(dataLE2$Life.expectancy)^(2)
dataLE2
Model6_Tx <-lm(TLE~Adult.Mortality+percentage.expenditure+BMI+under.five.deaths+Polio+Diphtheria+HIV.AIDS+Income.composition.of.resources+Schooling+Status1,dataLE2)
summary(Model6_Tx)
qqnorm(Model6_Tx$residuals)
qqline(Model6_Tx$residuals)

plot(Model6_Tx$residuals~Model6_Tx$fitted.values)

###################################################


#install.packages("caret")
library(caret)
set.seed(1313)
trainidx<-createDataPartition(dataLE2$Life.expectancy,p=0.7,list=FALSE,times=1)
dataLE_train<-dataLE2[trainidx,]
dataLE_test<-dataLE2[-trainidx,]



###Model 8> Removing status and running the model on entire dataset


Model8<-lm(Life.expectancy~Adult.Mortality+percentage.expenditure+BMI+under.five.deaths+Polio+Diphtheria+HIV.AIDS+Income.composition.of.resources+Schooling,dataLE2)
summary(Model8) 
confint(Model8)
formula(Model8)
vif(Model8)

#Model4$residuals
sum(Model8$residuals)
qqnorm(Model8$residuals)
qqline(Model8$residuals)

plot(Model8$residuals~Model8$fitted.values)
anova(Model8)
library(MPV)
PRESS(Model8)
vif(Model8)
AIC(Model8)

BIC(Model8)

#### Using regression subsets to see if we can reduce the number of variables
##head(dataLE2)
library(leaps)
r11<-regsubsets(dataLE2[,c(6:24)],dataLE2[,5],nbest=3,nvmax=9)
summary(r11)


###Step method 
Model9<-step(Model1,direction='backward')
summary(Model9)
formula(Model9)
PRESS(Model9)
anova(Model9)
AIC(Model9)

BIC(Model9)

#Model10 > ICR and schooling are highly correlated and the VIF is 3 + Removing schooling

Model10<-lm(Life.expectancy~Adult.Mortality+percentage.expenditure+BMI+under.five.deaths+Polio+Diphtheria+HIV.AIDS+Income.composition.of.resources+Status1,dataLE2)
summary(Model10) 
confint(Model10)
formula(Model10)
vif(Model10)
sum(Model10$residuals)
qqnorm(Model10$residuals)
qqline(Model10$residuals)

plot(Model10$residuals~Model10$fitted.values)
anova(Model10)
library(MPV)
PRESS(Model10)
AIC(Model10)
BIC(Model10)


#Model11 > ICR and schooling are highly correlated and the VIF is 3 + Removing ICR

Model11<-lm(Life.expectancy~Adult.Mortality+percentage.expenditure+BMI+under.five.deaths+Polio+Diphtheria+HIV.AIDS+Schooling+Status1,dataLE2)
summary(Model11) 
confint(Model11)
formula(Model11)
vif(Model11)
sum(Model11$residuals)
qqnorm(Model11$residuals)
qqline(Model11$residuals)

plot(Model11$residuals~Model10$fitted.values)
anova(Model11)
library(MPV)
PRESS(Model11)
AIC(Model11)
BIC(Model11)

####Model 12 Remove Diptheria

Model12<-lm(Life.expectancy~Adult.Mortality+percentage.expenditure+BMI+under.five.deaths+HIV.AIDS+Polio+Income.composition.of.resources+Schooling+Status1,dataLE2)
summary(Model12) 
confint(Model12)
formula(Model12)
vif(Model12)

#Model4$residuals
sum(Model12$residuals)
qqnorm(Model12$residuals)
qqline(Model12$residuals)

plot(Model12$residuals~Model12$fitted.values)
anova(Model12)
library(MPV)
PRESS(Model12)
AIC(Model12)

BIC(Model12)

####Model 13 Remove Diptheria +ICR

Model13<-lm(Life.expectancy~Adult.Mortality+percentage.expenditure+BMI+under.five.deaths+HIV.AIDS+Polio+Schooling+Status1,dataLE2)
summary(Model13) 
confint(Model13)
formula(Model13)
vif(Model13)

#Model4$residuals
sum(Model13$residuals)
qqnorm(Model13$residuals)
qqline(Model13$residuals)

plot(Model13$residuals~Model13$fitted.values)
anova(Model13)
library(MPV)
PRESS(Model13)
AIC(Model13)

BIC(Model13)


####Model 14 Remove Diptheria + Status

Model14<-lm(Life.expectancy~Adult.Mortality+percentage.expenditure+BMI+under.five.deaths+HIV.AIDS+Polio+Income.composition.of.resources+Schooling,dataLE2)
summary(Model14) 
confint(Model14)
formula(Model14)
vif(Model14)

#Model4$residuals
sum(Model14$residuals)
qqnorm(Model14$residuals)
qqline(Model14$residuals)

plot(Model14$residuals~Model14$fitted.values)
anova(Model14)
library(MPV)
PRESS(Model14)
AIC(Model14)

BIC(Model14)



################sPLITTING MODELS IN TRAINING AND TEST DATASETS############################################################################




###Training dataset for Model 6
Model6a<-lm(Life.expectancy~Adult.Mortality+percentage.expenditure+BMI+under.five.deaths+Polio+Diphtheria+HIV.AIDS+Income.composition.of.resources+Schooling+Status1,dataLE_train)
summary(Model6a) 
confint(Model6a)
formula(Model6a)
vif(Model6a)


sum(Model6a$residuals)
qqnorm(Model6a$residuals)
qqline(Model6a$residuals)

plot(Model6a$residuals~Model6a$fitted.values)
anova(Model6a)
library(MPV)
PRESS(Model6a)
AIC(Model6a)
BIC(Model6a)

dataLE_test[,5]
pM6_2<-predict(Model6a,dataLE_test)

sum((pM6_2-dataLE_test[,5])^2)



###Training dataset for Model 8

Model8a<-lm(Life.expectancy~Adult.Mortality+percentage.expenditure+BMI+under.five.deaths+Polio+Diphtheria+HIV.AIDS+Income.composition.of.resources+Schooling,dataLE_train)
summary(Model8a) 
confint(Model8a)
formula(Model8a)
vif(Model8a)


sum(Model8a$residuals)
qqnorm(Model8a$residuals)
qqline(Model8a$residuals)

plot(Model8a$residuals~Model8a$fitted.values)
anova(Model8a)
library(MPV)
PRESS(Model8a)
AIC(Model8a)
BIC(Model8a)

pM8_2<-predict(Model8a,dataLE_test)

sum((pM8_2-dataLE_test[,5])^2)


###Training dataset for Model 12

Model12a<-lm(Life.expectancy~Adult.Mortality+percentage.expenditure+BMI+under.five.deaths+HIV.AIDS+Polio+Income.composition.of.resources+Schooling+Status1,dataLE_train)
summary(Model12a) 
confint(Model12a)
formula(Model12a)
vif(Model12a)


sum(Model12a$residuals)
qqnorm(Model12a$residuals)
qqline(Model12a$residuals)

plot(Model12a$residuals~Model12a$fitted.values)
anova(Model12a)
library(MPV)
PRESS(Model12a)
AIC(Model12a)
BIC(Model12a)

pM12_2<-predict(Model12a,dataLE_test)

sum((pM12_2-dataLE_test[,5])^2)

###Training dataset for Model 14

Model14a<-lm(Life.expectancy~Adult.Mortality+percentage.expenditure+BMI+under.five.deaths+HIV.AIDS+Polio+Income.composition.of.resources+Schooling,dataLE_train)
summary(Model14a) 
confint(Model14a)
formula(Model14a)
vif(Model14a)


sum(Model14a$residuals)
qqnorm(Model14a$residuals)
qqline(Model14a$residuals)

plot(Model14a$residuals~Model14a$fitted.values)
anova(Model14a)
library(MPV)
PRESS(Model14a)
AIC(Model14a)
BIC(Model14a)

pM14_2<-predict(Model14a,dataLE_test)

sum((pM14_2-dataLE_test[,5])^2)


###### Partial F test

anova(Model6a,Model8a)
anova(Model6a,Model12a)
anova(Model6a,Model14a)
