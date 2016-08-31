##Company wants to automate the loan eligibility process (real time) based on customer detail provided while filling online application form. 
##These details are Gender, Marital Status, Education, Number of Dependents, Income, Loan Amount, Credit History and others. 
##To automate this process, they have given a problem to identify the customers segments, 
##those are eligible for loan amount so that they can specifically target these customers. 
##Here they have provided a partial data set.

## Initial setup 
library(lattice)
library(ggplot2)
library(dplyr)
#install.packages("robustbase")
library(robustbase)
library(mice)
library(VIM)
library(mice)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(ineq)
library(lattice)
install.packages("ineq")

setwd ("C:/Users/Visveswar/Desktop/GL/AV/Prediction problem 2")
train = read.csv("train.csv",header = T,na.strings=c("","NA"))
test = read.csv("test.csv",header = T,na.strings=c("","NA"))
attach(train)
str(train)
str(test)
summary(train)
summary(test)

## Univariate Analysis
table(Loan_Status)/nrow(train)
## Our baseline model predicts "Yes" at 68.72% Accuracy 

##Since it is a categorical variable we are converting the Credit_History and Loan_Amount_Term to factors.
train$Credit_History = as.factor(train$Credit_History)
test$Credit_History = as.factor(test$Credit_History)

##  Bi-variate Analysis
table(Loan_Status,Self_Employed)
table(Loan_Status,Married)
table(Loan_Status,Gender)
table(Loan_Status,Education)
table(Loan_Status,Credit_History)

addmargins(prop.table(table(Loan_Status,Self_Employed),2))
addmargins(prop.table(table(Loan_Status,Gender),2))
ggplot(train,aes(Self_Employed,fill = factor(Loan_Status))) + geom_bar() + facet_grid(.~Gender)
## More or less the same propability of Employed type and gender got Loan.

ggplot(train, aes(x = Married,fill = factor(Loan_Status))) + geom_bar() + facet_grid(.~Education)
## probability for Married people who are graduate getting loan is higher when compared to Not single.

addmargins(prop.table(table(Loan_Status,Credit_History),2))
ggplot(train, aes(x = Loan_Status,fill = factor(Credit_History))) + geom_bar()
## From the graph we can see that the person who has credit history has high chances of getting loan.

prop.table(table(Loan_Status,Property_Area),2)
## The people from semiurban has got higher probability of loan status.

Coapplicanttrain = subset(train,CoapplicantIncome > 0)
table(Coapplicanttrain$Loan_Status)/nrow(Coapplicanttrain)
## The 71.8% Applicant got loan when the coapplicant earns 

train1 = train
range (ApplicantIncome)
cutRange = c(0,10000,20000,30000,40000,50000,60000,70000,80000,90000)
train1$Rangeincome = cut(ApplicantIncome,cutRange)
table(train1$Rangeincome)
plot(train1$Rangeincome)
# The applicant income is left skewed, there are more number of people who apply for loan is below 10000

mosaicplot(table(train$Dependents,train$Loan_Status),shade = TRUE)
#From the above table we can conclude that the dependent does not influence on loan sanction. 

### Married + Graduate + Credit History + Semiurban + earning from Coapplicant.

# Missing Value Treatment
trainMV = train[,-13]
str(train)

full = bind_rows(trainMV,test)
str(full)
#full$Credit_History = as.factor(full$Credit_History)
#full$Loan_Amount_Term = as.factor(full$Loan_Amount_Term)


##Filling Married data
NAs = which(is.na(full$Married))
full[NAs,]
table(full$Married)/nrow(full)
## Since only 3 data's are missing in marriage variable,  it would not affect much, 
## so we will use mode and fill all as 'Yes'
## In which one of the applicant has CoapplicantIncome and might be married.
full$Married[c(105,229,436)] = "Yes"

mice_plot =  aggr(full, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(full), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

## Filling Missing data using predixtion model
tempData <- mice(full,m=5,maxit=50,meth='pmm',seed=500)
summary(train)
tempData$imp$Gender

NAs = which(is.na(full$Gender))
full[NAs,]

completeData = complete(tempData,2)
str(completeData)

densityplot(tempData)

# Plot Loan amount distribution ## Comparing 
par(mfrow=c(1,2))
hist(full$LoanAmount, freq=F, main='Loan amount: Original Data', col='darkblue')
hist(completeData$LoanAmount, freq=F, main='Loan amount: MICE Output', col='lightblue')
## the graph looks same thus we are good. 

## comparing before and after imputation for catogorical variables
table(full$Gender)/nrow(full)
table(completeData$Gender)/nrow(completeData)
## Ratios we got are more or less same, lets have a final check on missing imputation

table(full$Dependents)/nrow(full)
table(completeData$Dependents)/nrow(completeData)
## the ratios are also looks great for Dependent.

## Feature engineering 
### computing Total income per month.
completeData$TotalincomePM = (completeData$ApplicantIncome + completeData$CoapplicantIncome)
summary(completeData$TotalincomePM)
### computing Loan amount per month.
completeData$LoanAmountPM = ((completeData$LoanAmount*1000)/completeData$Loan_Amount_Term)
summary(completeData$LoanAmountPM)

completeData$Ratio = (completeData$LoanAmountPM/completeData$TotalincomePM)
summary(completeData$Ratio)
str(completeData)
## 4 Prediction
trainfinal = completeData[1:614,]
testfinal = completeData[615:981,]
trainfinal$Loan_Status = train$Loan_Status
summary(trainfinal)

ggplot(trainfinal,aes(trainfinal$Ratio, fill = factor(trainfinal$Loan_Status))) + geom_histogram()  + facet_grid(.~ Credit_History) 
## The plot shows that the history credit with the lower Ratio has high probability of getting loan.

## RF model 
set.seed(754)
summary(trainfinal)
#corl = c(trainfinal$Ratio,trainfinal$LoanAmount,trainfinal$ApplicantIncome)
#cor(trainfinal,use="complete.obs")


## Model 1
rf_model1 = randomForest(factor(Loan_Status)~ Gender + Married +Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + 
                           + Loan_Amount_Term + Credit_History + Property_Area, data = trainfinal,ntree=500, mtry = 4, nodesize = 60,
                         importance=TRUE)

print(rf_model1)
plot(rf_model1)
rf_model1$err.rate

predict1 = predict(rf_model1,trainfinal)
table(trainfinal$Loan_Status,predict1)

## accuracy is  80.4%  Tuning random tree at node size 60 
(87+407)/(87+105+15+407)

## Tuning the model 1. 
tRF <- tuneRF(x = trainfinal[,-c(1,13,14,15,16)], 
              y=as.factor(trainfinal$Loan_Status),
              mtryStart = 4, 
              ntreeTry=34, 
              stepFactor = 1.5, 
              improve = 0.0001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 30, 
              importance=TRUE
)

rf_model1_class = predict(tRF,trainfinal,type = "class")
rf_model1_score = predict(tRF,trainfinal,type = "prob")
tunemodel1 = predict(tRF,trainfinal)
confusionMatrix(tunemodel1,trainfinal$Loan_Status)

pred = prediction(rf_model1_score[,2],trainfinal$Loan_Status)
perf = performance(pred,"tpr","fpr")
plot(perf)
Auc = as.numeric(performance(pred,"auc")@y.values)
Auc
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS
gini = ineq(rf_model1_score[,2], type="Gini")
gini
## final predicition using model 1
finalpred1 = predict(tRF,testfinal)
table(finalpred1)/nrow(testfinal)

str(trainfinal)
## Model 2 is built using our feature that we have extracted. 
rf_model2 = randomForest(factor(Loan_Status)~ ., data = trainfinal[,-c(1,13,14)],ntree=500, mtry = 4, nodesize = 60,
                         importance=TRUE)
print(rf_model2)
predict2 = predict(rf_model2,trainfinal)
table(trainfinal$Loan_Status,predict2)
(92+407)/(92+100+15+407)
## Model Accuracy 82.7% accuracy which is greater than the previous model
print(rf_model2)
plot(rf_model2)
rf_model2$err.rate

## Tuning the model 2. 
tRF2 <- tuneRF(x = trainfinal[,-c(1,13,14,16)], 
              y=as.factor(trainfinal$Loan_Status),
              mtryStart = 4, 
              ntreeTry=50, 
              stepFactor = 1.5, 
              improve = 0.0001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 30, 
              importance=TRUE
)

rf_model2_class = predict(tRF2,trainfinal,type = "class")
rf_model2_score = predict(tRF2,trainfinal,type = "prob")
tunemodel2 = predict(tRF2,trainfinal)

pred2 = prediction(rf_model2_score[,2],trainfinal$Loan_Status)
perf2 = performance(pred2,"tpr","fpr")
plot(perf2)
Auc2 = as.numeric(performance(pred2,"auc")@y.values)
Auc2
KS2 <- max(attr(perf2, 'y.values')[[1]]-attr(perf2, 'x.values')[[1]])
KS2
gini2 = ineq(rf_model2_score[,2], type="Gini")
gini2
install.packages("caret")
install.packages('caret', dependencies = TRUE)
library (caret)
confusionMatrix(tunemodel2,trainfinal$Loan_Status)
finalpred2 = predict(tRF2,testfinal)
table(finalpred2)/nrow(testfinal)
finalpred1 = predict(tRF,testfinal)
table(finalpred1,finalpred2)


summary(trainfinal)
## Model 3 is built using our feature that we have extracted. 
rf_model3 = randomForest(factor(Loan_Status)~ ., data = trainfinal[,-c(1)],ntree=500, mtry = 4, nodesize = 60,
                         importance=TRUE)
print(rf_model3)
predict3 = predict(rf_model3,trainfinal)
table(trainfinal$Loan_Status,predict3)
(93+407)/(93+99+15+407)
## Model Accuracy 82.7% accuracy which is greater than the previous model
print(rf_model3)
plot(rf_model3)
rf_model3$err.rate

## Tuning the model 3. 
tRF3 <- tuneRF(x = trainfinal[,-c(1,16)], 
               y=as.factor(trainfinal$Loan_Status),
               mtryStart = 4, 
               ntreeTry=175, 
               stepFactor = 1.5, 
               improve = 0.0001, 
               trace=TRUE, 
               plot = TRUE,
               doBest = TRUE,
               nodesize = 30, 
               importance=TRUE
)
#############################
rf_model3_class = predict(tRF3,trainfinal,type = "class")
rf_model3_score = predict(tRF3,trainfinal,type = "prob")
tunemodel3 = predict(tRF3,trainfinal)

pred3 = prediction(rf_model3_score[,2],trainfinal$Loan_Status)
perf3 = performance(pred3,"tpr","fpr")
plot(perf3)
Auc2 = as.numeric(performance(pred3,"auc")@y.values)
Auc2
finalpred3 = predict(tRF3,testfinal)
table(finalpred1,finalpred2)

confusionMatrix(tunemodel3,trainfinal$Loan_Status)

##Lets use the model 2 for our final prediction.
solution_frame <- data.frame(Loan_ID = testfinal$Loan_ID,Loan_Status = finalpred2)
write.csv(solution_frame,file = "C:/Users/Visveswar/Desktop/GL/AV/Prediction problem 2/final_Solution.csv")

##solution_frame1 <- data.frame(Loan_ID = testfinal$Loan_ID,Loan_Status = finalpred1)
##write.csv(solution_frame1,file = "C:/Users/Visveswar/Desktop/GL/AV/Prediction problem 2/final_Solution1.csv")

##solution_frame3 <- data.frame(Loan_ID = testfinal$Loan_ID,Loan_Status = finalpred3)
##write.csv(solution_frame3,file = "C:/Users/Visveswar/Desktop/GL/AV/Prediction problem 2/final_Solution3.csv")

### model 2 had performed better in the test compered to other model. it have performed 79.167% in the test data.