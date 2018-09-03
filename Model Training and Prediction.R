#Prepare data for building models

#Add variables with previous day sentiment score
mydata<-sentiment_data
for(i in 1:nrow(sentiment_data)){
  if(i<nrow(sentiment_data)){
    mydata$PNews_1[i]=sentiment_data$Positive_News[i+1]
    mydata$NNews_1[i]=sentiment_data$Negative_News[i+1]
    mydata$NeuNews_1[i]=sentiment_data$Neutral_News[i+1]
  }
  else{
    mydata$PNews_1[i]=0
    mydata$NNews_1[i]=0
    mydata$NeuNews_1[i]=0
  }
}


for(i in 1:nrow(sentiment_data)){
  if(i<(nrow(sentiment_data)-1)){
    mydata$PNews_2[i]=sentiment_data$Positive_News[i+2]
    mydata$NNews_2[i]=sentiment_data$Negative_News[i+2]
    mydata$NeuNews_2[i]=sentiment_data$Neutral_News[i+2]
  }
  else{
    mydata$PNews_2[i]=0
    mydata$NNews_2[i]=0
    mydata$NeuNews_2[i]=0
  }
}


for(i in 1:nrow(sentiment_data)){
  if(i<(nrow(sentiment_data)-2)){
    mydata$PNews_3[i]=sentiment_data$Positive_News[i+2]
    mydata$NNews_3[i]=sentiment_data$Negative_News[i+2]
    mydata$NeuNews_3[i]=sentiment_data$Neutral_News[i+2]
  }
  else{
    mydata$PNews_3[i]=0
    mydata$NNews_3[i]=0
    mydata$NeuNews_3[i]=0
  }
}


#Convert stock_movement into a binary variable
for(i in 1:nrow(sentiment_data)){
  if(mydata$stock_movement[i]== -1){
    mydata$stock_movement_bino[i]<-0
 #   print("AA")
  }
  else{
    mydata$stock_movement_bino[i]<-1
 #   print("BB")
  }
}

#Split dtaa into test and train set
ind<-sample(2, nrow(mydata), replace=TRUE, prob=c(0.8,0.2))
train_data=mydata[ind==1,]
test_data=mydata[ind==2,]


#Support vector machine prediction
library(e1071)
library(pROC)
library(caret)
tune.out2 <- tune.svm(as.factor(stock_movement_bino) ~ PNews_1+NNews_1+NeuNews_1+PNews_2+NNews_2+NeuNews_2+
                        PNews_3+NNews_3+NeuNews_3,data=train_data, kernel='radial', probability=TRUE,
                      cost=seq(0.5,50,length.out = 10), tune.control(cross = 5),
                      gamma = seq(0.005,0.05,length.out = 10))
svm2 <- predict(tune.out2$best.model, test_data)
confusionMatrix(svm2,as.factor(test_data$stock_movement_bino))



#Prediction Using XGBoost
library(xgboost)
train_data2<-train_data[,-c(1:11)]
test_data2<-test_data[,-c(1:11)]

X_train<-as.matrix(train_data2[,-10])
Y_train<-train_data2[,10]
train_matrix<-xgb.DMatrix(data=X_train, label=Y_train)
numberOfClasses <- length(unique(mydata$stock_movement_bino))
xgb_params <- list("objective" = "multi:softmax","eval_metric" = "mlogloss","num_class" =numberOfClasses,eta=0.3,max_depth=6)
bst<-xgboost(params = xgb_params,data=X_train,label =Y_train,nrounds = 50,verbose = 0)

X_test<-as.matrix(test_data2[,-10])
test_predict<-predict(bst,X_test)
confusionMatrix(test_predict,as.factor(test_data2$stock_movement_bino))

xgb.importance(feature_names = colnames(X_train), bst) %>% xgb.plot.importance()

