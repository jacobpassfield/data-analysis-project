load(file = "dna-data/data/e-f-data.RData")

library(caret)

set.seed(1234)
trainIndex = createDataPartition(EFdata$V1, p = 0.7, list = F)
trainEF = EFdata[trainIndex,]
testEF = EFdata[-trainIndex,]

# # # ADABOOSTING # # # -----------------------------------------------------------------------------------

library(adabag)

boost = boosting(V1 ~ ., data = trainEF, control = rpart.control(maxdepth = 1)) # 100 iterations default

# Training data

boost.pred = predict.boosting(boost, newdata = trainEF[,-1]) 
cm = table(true = trainEF[,1], predicted = boost.pred$class)
(cm[1,1]+cm[2,2])/dim(trainEF)[1] # 0.9666667

# Test data

boost1.pred = predict.boosting(boost, newdata = testEF[,-1]) 
cm = table(true = testEF[,1], predicted = boost1.pred$class)
cm
#       predicted
# true  neg pos
#  neg   86   6
#  pos   10  80
(cm[1,1]+cm[2,2])/dim(testEF)[1] # 0.9222222

boost.test.acc = double(10)
for(i in 1:10){
  boost2 = boosting(V1 ~ ., data = trainEF, control = rpart.control(maxdepth = 1))  
  boost2.pred = predict.boosting(boost2, newdata = testEF[,-1]) 
  cm = table(true = testEF[,1], predicted = boost2.pred$class)
  boost.test.acc[i] = (cm[1,1] + cm[2,2]) / dim(testEF)[1]
}
mean(boost.test.acc) # 0.9138889

# # # RANDOM FOREST # # # ---------------------------------------------------------------------------------

library(randomForest)

rf = randomForest(V1 ~ ., data = trainEF, importance = TRUE)
rf # OOB error rate 5%

rf.pred = predict(rf, testEF[,-1], type = "class") 
cm = table(predicted = rf.pred, true = testEF[,1])
cm
#           true
# predicted neg pos
#       neg  80   8
#       pos  10  82
(cm[1,1]+cm[2,2])/dim(testEF)[1]
# 0.9

rf.test.acc = double(10)
for(i in 1:10){
  rf1 = randomForest(V1 ~ ., data = trainEF, importance = TRUE)
  rf1.pred = predict(rf1, testEF[,-1], type = "class") 
  cm = table(true = testEF[,1], predicted = rf1.pred)
  rf.test.acc[i] = (cm[1,1] + cm[2,2]) / dim(testEF)[1]
}
mean(rf.test.acc) # 0.9044444

# # # COMPARING CLASSIFIERS # # # ------------------------------------------------------------------------

# Adaboosting:   0.9138889 < < <
# Random forest: 0.9044444 < < <

# # # VARIABLE IMPORTANCE # # # --------------------------------------------------------------------------

importanceplot(boost)
