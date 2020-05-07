
# loading the libraries 
library(tidyverse) # metapackage of all tidyverse packages

library(DataExplorer) # to make intro_plot()
library(gridExtra) # to make grid.arrange()

library(caret)

library(randomForest)

library(rpart) 
library(rpart.plot)

#loading the data
mydata = read_csv("../input/telco-customer-churn/WA_Fn-UseC_-Telco-Customer-Churn.csv") #Reading the Data

attr(mydata, 'spec') <- NULL # just to remove a warning about specifications 

str(mydata)

plot_intro(mydata) # let's take an overview on some ascpects about the data

head(as.integer(mydata$Churn == 'Yes')) # this how i can convert it into numerical

hist(as.integer(mydata$Churn == 'Yes') , col = 'darkblue' , main = 'distributuin of the target varible')

table(mydata$Churn)/nrow(mydata) # seeing the fraction of the zeros and ones in the data

n_distinct(mydata$customerID)

options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(mydata , aes( y = Churn ,fill = Churn)) + geom_bar() + facet_wrap(. ~ gender)+
  ggtitle("For each gender what is the distribution of zeros and ones") +
  theme(plot.title = element_text(hjust = 0.5)) 


prop.table(table(mydata$SeniorCitizen,mydata$Churn),1) # i want to get the ratio of Churn and in all levels

options(repr.plot.width = 15, repr.plot.height = 5)

ggplot(mydata , aes( y = Churn ,fill = SeniorCitizen)) + geom_bar() + facet_wrap(. ~ SeniorCitizen )+
  ggtitle("what is the distribution of Churn for SeniorCitizen and non SeniorCitizen") +
  theme(plot.title = element_text(hjust = 0.5)) 


table(mydata$Partner)/nrow(mydata) # distribution of people who have partner and who not
options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(mydata , aes( y = Churn ,fill = Partner)) + geom_bar() + facet_wrap(. ~ Partner )+
  ggtitle("what is the distribution of Churn for people with Partner and vice versa") +
  theme(plot.title = element_text(hjust = 0.5)) 


prop.table(table(mydata$Dependents,mydata$Churn),1)# i want to get the ratio of Churn and in all levels
options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(mydata , aes( y = Churn ,fill = Dependents)) + geom_bar() + facet_wrap(. ~ Dependents )+
  ggtitle("what is the distribution of Churn for people with Partner and vice versa") +
  theme(plot.title = element_text(hjust = 0.5)) 


options(repr.plot.width = 20, repr.plot.height = 8)

grid.arrange(
ggplot(mydata, aes(x=PhoneService,fill=Churn))+ 
    geom_bar(position = 'fill', show.legend = FALSE) + 
    scale_fill_ordinal() + 
    theme_minimal(),
    
    ggplot(mydata, aes(x=MultipleLines,fill=Churn))+ 
    geom_bar(position = 'fill')+
    labs(y = NULL) + scale_fill_ordinal() + 
    theme_minimal(), 
    
    ggplot(mydata, aes(x=InternetService,fill=Churn))+ 
    geom_bar(position = 'fill', show.legend = FALSE) + 
    scale_fill_ordinal() + 
    theme_minimal(),
    
    ggplot(mydata, aes(x=OnlineSecurity,fill=Churn))+ 
    geom_bar(position = 'fill')+labs(y = NULL) + 
    scale_fill_ordinal() + 
    theme_minimal(),
    
    nrow = 2
)

options(repr.plot.width = 20, repr.plot.height = 8)
grid.arrange(
ggplot(mydata, aes(x=OnlineBackup,fill=Churn))+ 
    geom_bar(position = 'fill', show.legend = FALSE) + 
    scale_fill_ordinal() + 
    theme_minimal(),
    
    ggplot(mydata, aes(x=DeviceProtection,fill=Churn))+ 
    geom_bar(position = 'fill')+
    labs(y = NULL) + 
    scale_fill_ordinal() + 
    theme_minimal(),
    
    ggplot(mydata, aes(x=TechSupport,fill=Churn))+ 
    geom_bar(position = 'fill', show.legend = FALSE) + 
    scale_fill_ordinal() + 
    theme_minimal(),
   
    ggplot(mydata, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+
    labs(y = NULL) + 
    scale_fill_ordinal() + 
    theme_minimal(),
    nrow = 2
    )

options(repr.plot.width = 15, repr.plot.height = 12)

grid.arrange( 
    ggplot(mydata, aes(x=StreamingMovies,fill=Churn))+ 
    geom_bar(position = 'fill', show.legend = FALSE) + 
    scale_fill_ordinal() + 
    theme_minimal(),
    
    ggplot(mydata, aes(x=Contract,fill=Churn))+  
    geom_bar(position = 'fill')+labs(y = NULL) + 
    scale_fill_ordinal() + 
    theme_minimal(),
    
    ggplot(mydata, aes(x=PaperlessBilling,fill=Churn))+ 
    geom_bar(position = 'fill', show.legend = FALSE) + 
    scale_fill_ordinal() + 
    theme_minimal(),
    
    ggplot(mydata, aes(x=PaymentMethod,fill=Churn))+
    geom_bar(position = 'fill')+
    labs(y = NULL) + 
    scale_fill_ordinal() + theme_minimal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
                     
                     nrow=3)


colnames(mydata)[colSums(is.na(mydata)) > 0] # getting the columns that have missing values

mydata2 = mydata %>% select(c(tenure,MonthlyCharges,TotalCharges)) %>% mutate(tenure_TotalCharges = tenure * MonthlyCharges , difference = tenure_TotalCharges -TotalCharges  )
head(mydata2)
summary(mydata2$difference)
sd(na.omit(mydata2$difference))

mydata2[rowSums(is.na(mydata2)) > 0,]

mydata[rowSums(is.na(mydata2)) > 0,'TotalCharges'] = mydata2[rowSums(is.na(mydata2)) > 0,'MonthlyCharges']
anyNA(mydata) # is there any nan in the data ?

options(repr.plot.width = 15, repr.plot.height = 8)

ggplot(mydata , aes( x = tenure , fill = Churn)) + geom_histogram(bins = 30) + theme(plot.title = element_text(hjust = 0.5))+ facet_wrap(. ~Churn )

ggplot(mydata , aes( x = TotalCharges , fill = Churn)) + geom_histogram(bins = 30) + theme(plot.title = element_text(hjust = 0.5)) + facet_wrap(. ~ Churn)

ggplot(mydata , aes( x = MonthlyCharges , fill = Churn)) + geom_histogram(bins = 30) + theme(plot.title = element_text(hjust = 0.5)) + facet_wrap(. ~ Churn)

mydata2 =subset(mydata , select = -c(customerID)) # keep a refrence to original data

binary_cols = c('Partner','Dependents','PhoneService','PaperlessBilling','Churn')
for(col in binary_cols) # for each column go and convert it to 1 if Yes and zero if No
    {
    mydata2[,col] = as.integer(mydata2[,col] == 'Yes')
    
    }

mydata2 = mydata2 %>% mutate_if(is.character,as.factor)  %>% mutate_if(is.factor, as.numeric)
str(mydata2) # check if the data is numerical now

# split the data into train, validation and test set
mydata2 = as.data.frame(mydata2)
set.seed(2)
train_index <- createDataPartition(mydata2$Churn, p = 0.7 , list = FALSE) # Taking 30% to make a validation and test set
train = mydata2[train_index,]
val_test = mydata2[-train_index,]
#making the train test data
set.seed(3)
val_index <- createDataPartition(val_test$Churn, p =0.5 , list = FALSE) # Taking 15% to make a validation and 15% and test set
val = val_test[val_index,]
test = val_test[-val_index,]

nrow(train)
nrow(val)
nrow(test)

# logistic regression
set.seed(123)
glm <- glm(Churn ~ ., data = train, family = "binomial")                 
prob_preds <- predict(glm, newdata = val , type = 'response') # predict and convert predictions into positive only
# making 0.5 as a threshold
preds <- if_else(prob_preds < 0.5, 0, 1)

# evaluating the model
confusion_table <- table(preds, val$Churn)
confusionMatrix = confusionMatrix( as.factor(preds) , as.factor(val$Churn),positive = "1" )

confusion_table
confusionMatrix

# trying multiple cutoffs for determining the class to find the threshold that maximize the sensitivity and pos pred value
# this function take a cutoff and return the accuracy, senstivity and pos_pred_value on a matrix shape and based on the results of this cutoff
cal_performance <- function(cutoff) 
{
  preds <- if_else(prob_preds < cutoff, 0, 1)
  conf <- confusionMatrix(as.factor(preds), as.factor(val$Churn), positive = "1")
  sensitivity <- conf$byClass[1]
  pos_pred_value <- conf$byClass[3] 
  accuray <- conf$overall[1]

  out <- t(as.matrix(c(sensitivity, pos_pred_value, accuray))) 
  colnames(out) <- c("sensitivity", "pos_pred_value", "accuracy")
  return(out)
}

cutoffs = seq(0.01,0.80,length=100) # trying 100 different cutoff within the range of 0.01 and 0.8
results = matrix(0,100,3) # initializing a matrix with 100 row and 3 columns one row for each cutoff results 

for(i in 1:100)
{
  results[i,] = cal_performance(cutoffs[i]) # putting the results of each cutoff in a single row in the matrix
} 

# plot the result to determine the best cutoff
options(repr.plot.width =15, repr.plot.height =8)
plot(cutoffs, results[,1],xlab="Cutoffs",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoffs,results[,2],col="darkgreen",lwd=2)
lines(cutoffs,results[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Pos_Pred_Value","Accuracy"))
axis(1, at = seq(0.1, 1, by = 0.1))

# logistic regression
set.seed(123)
glm <- glm(Churn ~ ., data = train, family = "binomial")                 
prob_preds <- predict(glm, newdata = val , type = 'response') # predict and convert predictions into positive only
# making 0.5 as a threshold
preds <- if_else(prob_preds < 0.3, 0, 1)

# evaluating the model
confusion_table <- table(preds, val$Churn)
confusionMatrix = confusionMatrix( as.factor(preds) , as.factor(val$Churn),positive = "1" )

confusion_table
confusionMatrix

# Decision tree
#train and predict
set.seed(123)
Dtree = rpart(Churn ~., data = train, method = "class")  # method = class to make it binary classification
preds <- predict(Dtree,type = "class", newdata = val)

#evaluating the model

confusion_table <- table(preds, val$Churn)
confusionMatrix = confusionMatrix( as.factor(preds) , as.factor(val$Churn),positive = "1" )

confusion_table
confusionMatrix

options(repr.plot.width = 15, repr.plot.height = 8)
rpart.plot(Dtree)

# Decision tree
#train and predict
set.seed(123)
Dtree = rpart(Churn ~., data = train, method = "class")  # method = class to make it binary classification
prob_preds <- predict(Dtree,type = "prob", newdata = val)
head(prob_preds[,2]) # these are the probabilities of class one


cal_performance <- function(cutoff) 
{
  preds = ifelse(prob_preds[,2] >= cutoff , 1 , 0) # if the probability if class one is greater than 0.3 then make the class 1 other wise make it o
  conf <- confusionMatrix(as.factor(preds), as.factor(val$Churn), positive = "1")
  sensitivity <- conf$byClass[1]
  pos_pred_value <- conf$byClass[3] 
  accuray <- conf$overall[1]

  out <- t(as.matrix(c(sensitivity, pos_pred_value, accuray))) 
  colnames(out) <- c("sensitivity", "pos_pred_value", "accuracy")
  return(out)
}


for(i in 1:100)
{
  results[i,] = cal_performance(cutoffs[i]) # putting the results of each cutoff in a single row in the matrix
} 



# plot the result to determine the best cutoff
options(repr.plot.width =15, repr.plot.height =8)
plot(cutoffs, results[,1],xlab="Cutoffs",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoffs,results[,2],col="darkgreen",lwd=2)
lines(cutoffs,results[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Pos_Pred_Value","Accuracy"))
axis(1, at = seq(0.1, 1, by = 0.1))

# Decision tree
#train and predict
Dtree = rpart(Churn ~., data = train, method = "class")  # method = class to make it binary classification
prob_preds <- predict(Dtree,type = "prob", newdata = val)
DT_preds = ifelse(prob_preds[,2] > 0.25 , 1 , 0) # if the probability if class one is greater than 0.3 then make the class 1 other wise make it o
#evaluating the model
confusion_table <- table(DT_preds, val$Churn)
confusionMatrix = confusionMatrix( as.factor(DT_preds) , as.factor(val$Churn),positive = "1" )

confusion_table
confusionMatrix


# Random forest
set.seed(123)

#train and predict
RF <- randomForest(as.factor(Churn) ~ ., data=train, importance = TRUE, ntree=500, do.trace=FALSE )
preds <- predict(RF, newdata=val)
#evaluating the model
confusion_table <- table(preds, val$Churn)
confusionMatrix = confusionMatrix( as.factor(preds) , as.factor(val$Churn),positive = "1" )

confusion_table
confusionMatrix


# plot the importance
RF <- randomForest(as.factor(Churn) ~ ., data=train, importance = TRUE, ntree=500, do.trace=FALSE )
imp_RF <- importance(RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MeanDecreaseAccuracy = imp_RF[,3])
imp_DF <- imp_DF[order(imp_DF$MeanDecreaseAccuracy, decreasing = TRUE),]

ggplot(imp_DF, aes(x=reorder(Variables, MeanDecreaseAccuracy), y=MeanDecreaseAccuracy, fill=MeanDecreaseAccuracy)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% MeanDecreaseAccuracy') + coord_flip() + theme(legend.position="none")


plot(RF , main = "Error by number of trees")

# Random forest
set.seed(123)

#train and predict
RF <- randomForest(as.factor(Churn) ~ ., data=train, importance = TRUE, ntree=100, do.trace=FALSE )
prob_preds <- predict(RF, newdata=val , type = 'prob')
head(prob_preds)

cal_performance <- function(cutoff) 
{
  preds = ifelse(prob_preds[,2] >= cutoff , 1 , 0) # if the probability if class one is greater than 0.3 then make the class 1 other wise make it o
  conf <- confusionMatrix(as.factor(preds), as.factor(val$Churn), positive = "1")
  sensitivity <- conf$byClass[1]
  pos_pred_value <- conf$byClass[3] 
  accuray <- conf$overall[1]

  out <- t(as.matrix(c(sensitivity, pos_pred_value, accuray))) 
  colnames(out) <- c("sensitivity", "pos_pred_value", "accuracy")
  return(out)
}


for(i in 1:100)
{
  results[i,] = cal_performance(cutoffs[i]) # putting the results of each cutoff in a single row in the matrix
} 


# plot the result to determine the best cutoff
options(repr.plot.width =15, repr.plot.height =8)
plot(cutoffs, results[,1],xlab="Cutoffs",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoffs,results[,2],col="darkgreen",lwd=2)
lines(cutoffs,results[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Pos_Pred_Value","Accuracy"))
axis(1, at = seq(0.1, 1, by = 0.1))

# Random forest
set.seed(123)

#train and predict
RF <- randomForest(as.factor(Churn) ~ ., data=train, importance = TRUE, ntree=100, do.trace=FALSE )
prob_preds <- predict(RF, newdata=val , type = 'prob')
RF_preds = ifelse(prob_preds[,2] >= 0.3 , 1 , 0)
#evaluating the model
confusion_table <- table(preds, val$Churn)
confusionMatrix = confusionMatrix( as.factor(RF_preds) , as.factor(val$Churn),positive = "1" )

confusion_table
confusionMatrix


# logistic regression
set.seed(123)
train_full = rbind(train,val) # concating the train and validation to train on both of them
glm <- glm(Churn ~ ., data = train_full, family = "binomial")                 
prob_preds <- predict(glm, newdata = test , type = 'response') # predict and convert predictions into positive only
# making 0.5 as a threshold
glm_preds <- if_else(prob_preds < 0.3, 0, 1)

# evaluating the model
confusion_table <- table(glm_preds, test$Churn)
confusionMatrix = confusionMatrix( as.factor(glm_preds) , as.factor(test$Churn),positive = "1" )

confusion_table
confusionMatrix

mydata3 <- read_csv("../input/telco-customer-churn/WA_Fn-UseC_-Telco-Customer-Churn.csv") #Reading the Data
attr(mydata3, 'spec') <- NULL # just to remove a warning about specifications 
mydata3$customerID <- NULL # remove customer id
mydata3[rowSums(is.na(mydata3)) > 0,'TotalCharges'] = mydata3[rowSums(is.na(mydata3)) > 0,'MonthlyCharges']


mydata3[rowSums(is.na(mydata3)) > 0,'TotalCharges'] = mydata3[rowSums(is.na(mydata3)) > 0,'MonthlyCharges']
Churn = ifelse(mydata3$Churn == 'Yes' , 1 , 0)
mydata3$Churn <- NULL # remove Churn

#making the one hot encoding
dmy <- dummyVars(" ~ .", data = mydata3)
mydata3_dmy_encoded  <- data.frame(predict(dmy, newdata = mydata))
mydata3_dmy_encoded = cbind(mydata3_dmy_encoded,Churn)
head(mydata3_dmy_encoded)

# split the data into train, validation and test set
mydata3_dmy_encoded = as.data.frame(mydata3_dmy_encoded)
set.seed(4)
train_index <- createDataPartition(mydata3_dmy_encoded$Churn, p = 0.7 , list = FALSE ) # Taking 30% to make a validation and test set
train = mydata3_dmy_encoded[train_index,]
val_test = mydata3_dmy_encoded[-train_index,]
#making the train test data
set.seed(5)
val_index <- createDataPartition(val_test$Churn, p =0.5 , list = FALSE) # Taking 15% to make a validation and 15% and test set
val = val_test[val_index,]
test = val_test[-val_index,]

# logistic regression
set.seed(123)
glm <- glm(Churn ~ ., data = train, family = "binomial")                 
prob_preds <- predict(glm, newdata = val , type = 'response') # predict and convert predictions into positive only
# making 0.3 as a threshold
preds <- if_else(prob_preds < 0.3, 0, 1)

# evaluating the model
confusion_table <- table(preds, val$Churn)
confusionMatrix = confusionMatrix( as.factor(preds) , as.factor(val$Churn),positive = "1" )

confusion_table
confusionMatrix

mydata4 <- read_csv("../input/telco-customer-churn/WA_Fn-UseC_-Telco-Customer-Churn.csv") #Reading the Data
attr(mydata4, 'spec') <- NULL # just to remove a warning about specifications 
mydata4$customerID <- NULL # remove customer id
mydata4[rowSums(is.na(mydata4)) > 0,'TotalCharges'] = mydata4[rowSums(is.na(mydata4)) > 0,'MonthlyCharges']
mydata4 = as.data.frame(mydata4)

binary_cols = c('Partner','Dependents','PhoneService','PaperlessBilling','Churn')
for(col in binary_cols) # for each column go and convert it to 1 if Yes and zero if No
    {
    mydata4[,col] = as.integer(mydata4[,col] == 'Yes')
    
    }

mydata4 = mydata4 %>% mutate_if(is.character,as.factor)  %>% mutate_if(is.factor, as.numeric)

head(mydata4)

# i'm just using mydata5 to get the categorical variables and make the interactions with it then append them in mydata4 
mydata5 <- read_csv("../input/telco-customer-churn/WA_Fn-UseC_-Telco-Customer-Churn.csv") #Reading the Data
attr(mydata5, 'spec') <- NULL # just to remove a warning about specifications 
mydata5$customerID <- NULL # remove customer id
mydata5$Churn <- NULL # removing Churn as we don't want it in the interactions
mydata5 = as.data.frame(mydata5)

cat_names <- colnames(mydata5[,sapply(mydata5,class) == 'character']) # get all the categorical names

interaction_names = c() # this will be the list with the interactions names
for (i in 2: length(cat_names) - 1 )
    {
    
    for(j in (i+1): length(cat_names) )
        {
        
          interaction_name = paste(cat_names[i],cat_names[j],sep = '_') # the interaction name will be the 2 columns name seperated with -
          interaction = paste(mydata5[,cat_names[i]] , mydata5[,cat_names[j]] , sep = '_') # this is the interaction column values i will access the 2 columns with their names and paste their values with - sperator
          mydata4[,interaction_name] = interaction # i'm appending in mydata3 which is my the converted as in the base model dataframe 
          interaction_names = c(interaction_names,interaction_name)     # append the names in a list              
        }
    
    }


interaction_names # these are the columns names,  it's an interaction between each column with all other columns                              

head(mydata4) #This is the dataframe now

# converting the columns to be numerical
mydata4 = mydata4 %>% mutate_if(is.character,as.factor)  %>% mutate_if(is.factor, as.numeric)

set.seed(4)
# split the data into train, validation and test set
train_index <- createDataPartition(mydata4$Churn, p = 0.7 , list = FALSE ) # Taking 30% to make a validation and test set
train = mydata4[train_index,]
val_test = mydata4[-train_index,]
#making the train test data
set.seed(5)
val_index <- createDataPartition(val_test$Churn, p =0.5 , list = FALSE) # Taking 15% to make a validation and 15% and test set
val = val_test[val_index,]
test = val_test[-val_index,]

# logistic regression
set.seed(123)
glm <- glm(Churn ~ ., data = train, family = "binomial")                 
prob_preds <- predict(glm, newdata = val , type = 'response') # predict and convert predictions into positive only
# making 0.3 as a threshold
preds <- if_else(prob_preds < 0.30, 0, 1)

# evaluating the model
confusion_table <- table(preds, val$Churn)
confusionMatrix = confusionMatrix( as.factor(preds) , as.factor(val$Churn),positive = "1" )

confusion_table
confusionMatrix

# plot the importance
set.seed(12345)
RF <- randomForest(as.factor(Churn) ~ ., data=train, importance = TRUE, ntree=50, do.trace=FALSE )
imp_RF <- importance(RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MeanDecreaseAccuracy = imp_RF[,3])
imp_DF <- imp_DF[order(imp_DF$MeanDecreaseAccuracy, decreasing = TRUE),]



# ploting the most 20 important column
ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MeanDecreaseAccuracy), y=MeanDecreaseAccuracy, fill=MeanDecreaseAccuracy)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% MeanDecreaseAccuracy') + coord_flip() + theme(legend.position="none")


# train on the most 50 important column
mydata4_subset = mydata4[,imp_DF[1:20,]$Variables]
Churn = mydata4$Churn
mydata4_subset = cbind(mydata4_subset,Churn)
set.seed(4)
# split the data into train, validation and test set
train_index <- createDataPartition(mydata4_subset$Churn, p = 0.7 , list = FALSE ) # Taking 30% to make a validation and test set
train = mydata4_subset[train_index,]
val_test = mydata4_subset[-train_index,]
#making the train test data
set.seed(5)
val_index <- createDataPartition(val_test$Churn, p =0.5 , list = FALSE) # Taking 15% to make a validation and 15% and test set
val = val_test[val_index,]
test = val_test[-val_index,]

# logistic regression
set.seed(123)
glm <- glm(Churn ~ ., data = train, family = "binomial")                 
prob_preds <- predict(glm, newdata = val , type = 'response') # predict and convert predictions into positive only
# making 0.3 as a threshold
preds <- if_else(prob_preds < 0.30, 0, 1)

# evaluating the model
confusion_table <- table(preds, val$Churn)
confusionMatrix = confusionMatrix( as.factor(preds) , as.factor(val$Churn),positive = "1" )

confusion_table
confusionMatrix

set.seed(12345)
combinedDF <- data.frame(RF_preds ,DT_preds,glm_preds, Churn = val$Churn)
stack.fit <- train(as.factor(Churn) ~ . , method = "treebag" , data = combinedDF  )

stack.pred.validation <- predict(stack.fit , val)
confusionMatrix( as.factor(stack.pred.validation) , as.factor(val$Churn),positive = "1" )

