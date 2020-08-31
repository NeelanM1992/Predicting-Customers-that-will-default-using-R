install.packages("pacman")

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","partykit","ROCR","lift","glmnet","MASS","e1071", "readxl","rpart","randomForest","xgboost","dplyr" ) 

full <- read.csv("C:/Users/neela/Desktop/MMA867 A3 -- credit data.csv")

lapply(full[c(3:5,7:12,25)], table)

full_1<-full%>%
  select(13:24)

#full_1[full_1 == 0] <- 0.1

full_1$Zero_Values<-apply(full_1,1,function(x)sum(x == 0))

full_2<-full%>%
  select(1:12,25)

full_3<-full_2%>%
  select(7:12)

full_3$Positive_Values<-apply(full_3,1,function(x)sum(x>=0))

full_3<-full_3%>%
  select(7)

full<-cbind(full_2,full_3,full_1)%>%
  select(1:12,27,14:26,13)

full$bene <- ifelse(full$PAY_1 == -1 & full$PAY_2 == -1 & full$PAY_3 == -1 & full$PAY_4 == -1 & full$PAY_5 == -1 & full$PAY_6 == -1,1,0)
full$rev <- ifelse(full$PAY_1 == 0 & full$PAY_2 == 0 & full$PAY_3 == 0 & full$PAY_4 == 0 & full$PAY_5 == 0 & full$PAY_6 == 0,1,0)
full$Flag<-ifelse(full$Positive_Values>=3,1,0)
full$PAY_1 <- ifelse(full$PAY_1 >= 4 , 4 , full$PAY_1)
full$PAY_2 <- ifelse(full$PAY_2 >= 4 , 4 , full$PAY_2)
full$PAY_3 <- ifelse(full$PAY_3 >= 4 , 4 , full$PAY_3)
full$PAY_4 <- ifelse(full$PAY_4 >= 4 , 4 , full$PAY_4)
full$PAY_5 <- ifelse(full$PAY_5 >= 4 , 4 , full$PAY_5)
full$PAY_6 <- ifelse(full$PAY_6 >= 4 , 4 , full$PAY_6)
full$MARRIAGE <- ifelse(full$MARRIAGE == 0, 3 , full$MARRIAGE)
full$EDUCATION <- ifelse(full$EDUCATION == 0 | full$EDUCATION == 6 , 5 , full$EDUCATION)

full$SEX <- as.factor(full$SEX)
full$MARRIAGE <- as.factor(full$MARRIAGE)
full$EDUCATION <- as.factor(full$EDUCATION)
full$default_0 <- as.factor(full$default_0)


full$AGE_GROUP<-ifelse(full$AGE>=18 & full$AGE<=25,1,
                       ifelse(full$AGE>=26 & full$AGE <=35,2,
                              ifelse(full$AGE>=36 & full$AGE<=45,3,
                                     ifelse(full$AGE>=46 & full$AGE <=55,4,
                                            ifelse(full$AGE>=56 & full$AGE<=65,5,
                                                   ifelse(full$AGE>65,6,7))))))

full$Limit_Amount1<-round((full$BILL_AMT1/full$LIMIT_BAL)*100,3)
full$Limit_Amount2<-round((full$BILL_AMT2/full$LIMIT_BAL)*100,3)
full$Limit_Amount3<-round((full$BILL_AMT3/full$LIMIT_BAL)*100,3)
full$Limit_Amount4<-round((full$BILL_AMT4/full$LIMIT_BAL)*100,3)
full$Limit_Amount5<-round((full$BILL_AMT5/full$LIMIT_BAL)*100,3)
full$Limit_Amount6<-round((full$BILL_AMT6/full$LIMIT_BAL)*100,3)

full$TwoMonth_BillAmount<-rowSums(full[,15:16])
full$ThreeMonth_BillAmount<-rowSums(full[,15:17])
full$FourMonth_BillAmount<-rowSums(full[,15:18])
full$FiveMonth_BillAmount<-rowSums(full[,15:19])
full$SixMonth_BillAmount<-rowSums(full[,15:20])

full$OneMonth_BillAmount_Ratio<-round((full$BILL_AMT1/full$SixMonth_BillAmount)*100,3)
full$TwoMonth_BillAmount_Ratio<-round((full$TwoMonth_BillAmount/full$SixMonth_BillAmount)*100,3)
full$ThreeMonth_BillAmount_Ratio<-round((full$ThreeMonth_BillAmount/full$SixMonth_BillAmount)*100,3)
full$FourMonth_BillAmount_Ratio<-round((full$FourMonth_BillAmount/full$SixMonth_BillAmount)*100,3)
full$FiveMonth_BillAmount_Ratio<-round((full$FiveMonth_BillAmount/full$SixMonth_BillAmount)*100,3)

full$TwoMonth_PaymentAmount<-rowSums(full[,21:22])
full$ThreeMonth_PaymentAmount<-rowSums(full[,21:23])
full$FourMonth_PaymentAmount<-rowSums(full[,21:24])
full$FiveMonth_PaymentAmount<-rowSums(full[,21:25])
full$SixMonth_PaymentAmount<-rowSums(full[,21:26])

full$OneMonth_PaymentAmount_Ratio<-round((full$PAY_AMT1/full$SixMonth_PaymentAmount)*100,3)
full$TwoMonth_PaymentAmount_Ratio<-round((full$TwoMonth_PaymentAmount/full$SixMonth_PaymentAmount)*100,3)
full$ThreeMonth_PaymentAmount_Ratio<-round((full$ThreeMonth_PaymentAmount/full$SixMonth_PaymentAmount)*100,3)
full$FourMonth_PaymentAmount_Ratio<-round((full$FourMonth_PaymentAmount/full$SixMonth_PaymentAmount)*100,3)
full$FiveMonth_PaymentAmount_Ratio<-round((full$FiveMonth_PaymentAmount/full$SixMonth_PaymentAmount)*100,3)

#full$OneMonth_PaymentandBill_Ratio<-round((full$PAY_AMT1/full$BILL_AMT1)*100,3)
#full$TwoMonth_PaymentandBill_Ratio<-round((full$PAY_AMT2/full$BILL_AMT2)*100,3)
#full$ThreeMonth_PaymentandBill_Ratio<-round((full$PAY_AMT3/full$BILL_AMT3)*100,3)
#full$FourMonth_PaymentandBill_Ratio<-round((full$PAY_AMT4/full$BILL_AMT4)*100,3)
#full$FiveMonth_PaymentandBill_Ratio<-round((full$PAY_AMT5/full$BILL_AMT5)*100,3)
#full$SixMonth_PaymentandBill_Ratio<-round((full$PAY_AMT6/full$BILL_AMT6)*100,3)

full$Total_PaymentandBill_Ratio<-round((full$SixMonth_PaymentAmount/full$SixMonth_BillAmount)*100,3)

full$Positive_Values<-as.factor(full$Positive_Values)
full$Zero_Values<-as.factor(full$Zero_Values)
full$Flag<-as.factor(full$Flag)
full$PAY_1 <- as.factor(full$PAY_1)
full$PAY_2 <- as.factor(full$PAY_2)
full$PAY_3 <- as.factor(full$PAY_3)
full$PAY_4 <- as.factor(full$PAY_4)
full$PAY_5 <- as.factor(full$PAY_5)
full$PAY_6 <- as.factor(full$PAY_6)
full$AGE_GROUP<-as.factor(full$AGE_GROUP)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

full[is.nan(full)] <- 0

is.na(full)<-sapply(full, is.infinite)
full[is.na(full)]<-0

full<-full%>%
  select(1:14,27:58)

#full[full == -Inf] <- 0
#full[is.na(full)] <- 0

inTrain <- createDataPartition(y = full$default_0,
                               p = 22999/24000, list = FALSE)
train <- full[ inTrain,]
test <- full[ -inTrain,]


############################################################################################################################################

model_logistic<-glm(default_0 ~ ., family="binomial", data=train)

#stopCluster(cl)
summary(model_logistic) 

model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1) #AIC stepwise
summary(model_logistic_stepwiseAIC) 

par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))

###Finding predicitons: probabilities and classification
logistic_probabilities<-predict(model_logistic_stepwiseAIC,newdata=test,type="response") #Predict probabilities
logistic_classification<-rep("1",4800)
logistic_classification[logistic_probabilities<0.2211]="0" 
logistic_classification<-as.factor(logistic_classification)

###Confusion matrix  
confusionMatrix(logistic_classification,test$default_0,positive = "1") #Display confusion matrix

####ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, test$default_0)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(logistic_ROC_prediction,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(logistic_probabilities, test$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart





############################################################################################################################################

#-------------------------CART-----------------------------------------------------------------------

ctree_tree<-ctree(default_0~.,data=train) #Run ctree on training data
plot(ctree_tree, gp = gpar(fontsize = 8)) #Plotting the tree (adjust fontsize if needed)

ctree_probabilities<-predict(ctree_tree,newdata=test,type="prob") #Predict probabilities
ctree_classification<-rep("1",1000)
ctree_classification[ctree_probabilities[,2]<0.2211]="0" #Predict classification using 0.6073 threshold. Why 0.6073 - that's the average probability of being retained in the data. An alternative code: logistic_classification <- as.integer(logistic_probabilities > mean(testing$default_0 == "1"))
ctree_classification<-as.factor(ctree_classification)

###Confusion matrix  
confusionMatrix(ctree_classification,test$default_0,positive = "1")

####ROC Curve
ctree_probabilities_testing <-predict(ctree_tree,newdata=test,type = "prob") #Predict probabilities
ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], test$default_0) #Calculate errors
ctree_ROC_testing <- performance(ctree_pred_testing,"tpr","fpr") #Create ROC curve data
plot(ctree_ROC_testing) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(ctree_pred_testing,"auc") #Create AUC data
ctree_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
ctree_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(ctree_probabilities[,2],  test$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

###
### RPART
###

# The rpart method has an important "complexity parameter", cp, which determines how big the tree is.  

CART_cp = rpart.control(cp = 0.0005) #set cp to a small number to "grow" a large tree

rpart_tree<-rpart(default_0~.,data=train, method="class", control=CART_cp) #"Grow" a tree on training data

prunned_rpart_tree<-prune(rpart_tree, cp=0.001) #Prun the tree. Play with cp to see how the resultant tree changes
plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7)) #Plotting the tree (adjust fontsize if needed)

# Understand the relationship between the cross-validated error, size of the tree and cp.
plotcp(rpart_tree) # Use printcp(rpart_tree) to print the values. As a rule of thumb pick up the largest cp which does not give a substantial drop in error

rpart_prediction_class<-predict(prunned_rpart_tree,newdata=test, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class,test$default_0,positive = "1") #Display confusion matrix

rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=test,type = "prob") #Predict probabilities
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], test$default_0) #Calculate errors
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing,"auc") #Create AUC data
rpart_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing #Display AUC value

plotLift(rpart_prediction_class,  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart




############################################################################################################################################
#--------------------------------------xgboost------------------------------------------------------------
creditdata_matrix <- model.matrix(default_0~ ., data = full)[,-1]

x_train <- creditdata_matrix[ inTrain,]
x_test <- creditdata_matrix[ -inTrain,]

y_train <-train$default_0
y_test <-test$default_0

model_XGboost<-xgboost(data = data.matrix(x_train), 
                       label = as.numeric(as.character(y_train)), 
                       eta = 0.1,       # hyperparameter: learning rate 
                       max_depth = 20,  # hyperparameter: size of a tree in each boosting iteration
                       nround=50,       # hyperparameter: number of boosting iterations  
                       objective = "binary:logistic"
)

XGboost_prediction<-predict(model_XGboost,newdata=x_test, type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.2211,1,0)),y_test,positive="1") #Display confusion matrix

####ROC Curve
XGboost_ROC_prediction <- prediction(XGboost_prediction, y_test) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_ROC_prediction,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(XGboost_prediction, y_test, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_XGboost <- performance(XGboost_ROC_prediction,"lift","rpp")
plot(Lift_XGboost)

