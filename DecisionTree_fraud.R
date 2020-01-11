install.packages("C50") #
library(C50)
library(readr)
Fraud_check <- read_csv("C:/Users/Admin/Desktop/Assignments/Decision_tree/Fraud_check.csv")
view(Fraud_check)
str(Fraud_check)
summary(Fraud_check)
Fraud_check$Taxable.Income1<-factor(as.numeric(Fraud_check$Taxable.Income<='30000'),labels = c("good","risky"))
Fraud_check<- as.data.frame(Fraud_check)
Fraud_check


Fraud_check_train <- Fraud_check[1:500,]
Fraud_check_test <- Fraud_check[501:600,]

Fraud_checkc5.0_train <- C5.0(Fraud_check_train[,c(-3,-7)],Fraud_check_train$Taxable.Income1)
plot(Fraud_checkc5.0_train )

# Training accuracy
mean(Fraud_check_train$Taxable.Income1==predict(Fraud_checkc5.0_train,Fraud_check_train)) # 
predc5.0_test <- predict(Fraud_checkc5.0_train,newdata=Fraud_check_test) # 
mean(predc5.0_test==Fraud_check_test$Taxable.Income1) # 
# training accurecy
CM=table(predc5.0_test,Fraud_check_test$Taxable.Income1)
accuracy = (sum(diag(CM)))/sum(CM)
accuracy
# Cross tablez
library(gmodels)
CrossTable(Fraud_check_test$Taxable.Income1,predc5.0_test)
