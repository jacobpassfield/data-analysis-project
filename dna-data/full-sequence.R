data = read.table("dna-data/data/human-phage.txt")

library(caret)
set.seed(1234)
trainIndex = createDataPartition(data$V1, p = 0.7, list = F)
train = data[trainIndex,]
test = data[-trainIndex,]

# # # ADABOOSTING # # # -----------------------------------------------------------------------------------

library(adabag)

boost = boosting(V1 ~ ., data = train, control = rpart.control(maxdepth = 1))  

# Training set

boost.pred = predict.boosting(boost, newdata = train[,-1]) 
cm = table(true = train[,1], predicted = boost.pred$class)
(cm[1,1] + cm[2,2]) / dim(train)[1] # 0.8809524

# Test set

boost1.pred = predict.boosting(boost, newdata = test[,-1]) 
cm = table(true = test[,1], predicted = boost1.pred$class)
cm
#       predicted
# true  neg pos
#  neg  68  22
#  pos  32  58
(cm[1,1]+cm[2,2])/dim(test)[1] # 0.6777778

boost.test.acc = double(10)
for(i in 1:10){
  boost2 = boosting(V1 ~ ., data = train, control = rpart.control(maxdepth = 1))  
  boost2.pred = predict.boosting(boost2, newdata = test[,-1]) 
  cm = table(true = test[,1], predicted = boost2.pred$class)
  boost.test.acc[i] = (cm[1,1] + cm[2,2]) / dim(test)[1]
}
mean(boost.test.acc) # 0.6583333

# # # RANDOM FOREST # # # ---------------------------------------------------------------------------------

library(randomForest)

rf = randomForest(V1 ~ ., data = train, importance = TRUE)
rf # OOB error rate 26.43%

rf.pred = predict(rf, test[,-1], type = "class") 
cm = table(predicted = rf.pred, true = test[,1])
cm
#           true
# predicted neg pos
#       neg  63  28
#       pos  27  62
(cm[1,1]+cm[2,2])/dim(test)[1]
# 0.6944444

rf.test.acc = double(10)
for(i in 1:10){
  rf1 = randomForest(V1 ~ ., data = train, importance = TRUE)
  rf1.pred = predict(rf1, test[,-1], type = "class") 
  cm = table(true = test[,1], predicted = rf1.pred)
  rf.test.acc[i] = (cm[1,1] + cm[2,2]) / dim(test)[1]
}
mean(rf.test.acc) # 0.6861111

# # # COMPARING CLASSIFIERS # # # ------------------------------------------------------------------------

# Adaboosting:   0.6583333 
# Random forest: 0.6861111 < < <
