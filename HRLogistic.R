HRDataOrig1<-read.csv(file.choose())
library(ISLR)
attach(HRDataOrig1)

#Understanding the Data Dictionary
hist(last_evaluation_tuned)
hist(average_montly_hours)
hist((average_montly_hours), freq=FALSE, ylim=c(0, .4))
qqnorm(average_montly_hours)
?scale


#Tuning dataset
HRDataTuned<-as.data.frame(scale(HRDataOrig1[1:8],center = TRUE))
HRDataTuned<-data.frame(HRDataTuned,HRDataOrig1[,c(9:13)])

attach(HRDataTuned)

hist(satisfaction_level)
summary(HRDataTuned)
#Converting Work Accident & Left & Promotion as factors
HRDataOrig1$Work_accident<-as.factor(Work_accident)
HRDataOrig1$left<-as.factor(left)
HRDataOrig1$promotion_last_5years<-as.factor(promotion_last_5years)

str(HRDataOrig1)

#Calculate correlation between variables excluding year and direction columns

cor(HRDataOrig1[,-c(6,7,8,9,10)])
pairs(HRDataOrig1[,-c(6,7,8,9,10)])

X<-as.matrix(HRDataOrig1[,-c(9,10)])
typeof(X)
cor(X)
corrplot(cor(X))
#Split data into train and test data
set.seed(555)
ind<-sample(2,nrow(HRDataOrig1),replace=TRUE,prob = c(0.7,0.3)) 
training1<-HRDataOrig1[ind==1,]
testing1<-HRDataOrig1[ind==2,]

str(training1)

#Next step - Fit a logistic regression model to training data

HR_model1 <- glm(left~+satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+IfIT+IfManagement+SalaryLow+SalaryMedium,
                 data=training1,
                 family=binomial)

summary(HR_model1)

HR_modelAllVar <- glm(left~.,data=training1,family=binomial)
summary(HR_modelAllVar)

#Calculating the p values of chisquare deviances
1 - pchisq(1383.3-1381.1,997-991)

#P value indicates that we do not reject the null hypothesis
#Null hypothesis - There is no significant difference when the variables
#are included in the model.

library(caret)
#Use the fitted model to predict for the test data
model_predict_prob <- predict(HR_modelAllVar,testing1,type="response")
model_predict_prob

model_pred_Direction <- rep(0, 4446)
model_pred_Direction[model_predict_prob > 0.5] <- 1

#Create confusion matrix and calculate misclassification rate
table(model_pred_Direction,testing$left)

