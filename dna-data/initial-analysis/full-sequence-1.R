data = read.table("dna-data/data/human-phage.txt")

library(caret)
set.seed(1234)
trainIndex = createDataPartition(data$V1, p = 0.7, list = F)
train = data[trainIndex,]
test = data[-trainIndex,]

# # # DECISION TREES # # # ---------------------------------------------------------------------------------

library(tree)

tree = tree(V1~., data = train)
plot(tree)
text(tree, pretty = 1, cex = 0.7)

# Training
tree.pred = predict(tree, data = train[,-1], type = "class")
table(predicted = tree.pred, true = train[,1])
#           true
# predicted neg pos
#       neg 193   7
#       pos  17 203
(193+203)/dim(train)[1]
# 0.9428571

# Test
tree.pred = predict(tree, test[,-1], type = "class")
table(predicted = tree.pred, true = test[,1])
#           true
# predicted neg pos
#       neg  48  38
#       pos  42  52
(48+52)/dim(test)[1] 
# 0.5555556

# Overfitting

# # # PRUNING # # # --------------------------------------------------------------------------------------

cv = cv.tree(tree, FUN = prune.misclass)
plot(cv)
# Lowest misclassification rate is 17 nodes

prune = prune.misclass(tree, best = 17)
plot(prune)
text(prune, pretty = 1, cex = 0.7)

# Training
prune.pred = predict(prune, train[,-1], type = "class")
table(predicted = prune.pred, true = train[,1])
#           true
# predicted neg pos
#       neg 161  16
#       pos  49 194
(161+194)/dim(train)[1]
# 0.8452381

# Test
prune.pred = predict(prune, test[,-1], type = "class")
table(predicted = prune.pred, true = test[,1])
#           true
# predicted neg pos
#       neg  47  30
#       pos  43  60
(47+60)/dim(test)[1] 
# 0.5944444

# # # ADABOOSTING # # # -----------------------------------------------------------------------------------

library(adabag)
boost = boosting(V1 ~ ., data = train, control = rpart.control(maxdepth = 1)) # 100 iterations default

# Train data

boost.train.acc = double(10)
for(i in 1:10){
  boost1 = boosting(V1 ~ ., data = train, control = rpart.control(maxdepth = 1))  
  boost1.pred = predict.boosting(boost1, newdata = train[,-1]) 
  cm = table(true = train[,1], predicted = boost1.pred$class)
  boost.test.acc[i] = (cm[1,1] + cm[2,2]) / dim(train)[1]
}
mean(boost.test.acc)

# Test data

boost.pred = predict.boosting(boost, newdata = test[,-1]) 
cm = table(true = test[,1], predicted = boost.pred$class)
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

# # # ANN # # # ------------------------------------------------------------------------------------------

library(nnet)

nn = nnet(V1 ~ ., data = train, size = 3)
nn.pred = predict(nn, test[,-1], type = "class") 
table(true = test$V1, predicted = as.factor(nn.pred))
#      predicted
# true neg pos
#  neg  53  37
#  pos  33  57
(53+57)/dim(test)[1] # 0.6111111

# 1 HIDDEN NODE
test1.acc = double(10)
for(i in 1:10){
  nn1 = nnet(V1 ~ ., data = train, size = 1)
  nn1.pred = predict(nn1, test[,-1], type = "class") 
  cm = table(true = test$V1, predicted = as.factor(nn1.pred))
  test1.acc[i] = (cm[1,1] + cm[2,2]) / dim(test)[1]
}
mean(test1.acc) # 0.6188889

# 2 HIDDEN NODES
test2.acc = double(10)
for(i in 1:10){
  nn2 = nnet(V1 ~ ., data = train, size = 2)
  nn2.pred = predict(nn2, test[,-1], type = "class") 
  cm = table(true = test$V1, predicted = as.factor(nn2.pred))
  test2.acc[i] = (cm[1,1] + cm[2,2]) / dim(test)[1]
}
mean(test2.acc) # 0.6366667 < < <

# 3 HIDDEN NODES
test3.acc = double(10)
for(i in 1:10){
  nn3 = nnet(V1 ~ ., data = train, size = 3)
  nn3.pred = predict(nn3, test[,-1], type = "class") 
  cm = table(true = test$V1, predicted = as.factor(nn3.pred))
  test3.acc[i] = (cm[1,1] + cm[2,2]) / dim(test)[1]
}
mean(test3.acc) # 0.6211111

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
# Error rate: 0.3055556 -- higher rate than OOB

rf.test.acc = double(10)
for(i in 1:10){
  rf1 = randomForest(V1 ~ ., data = train, importance = TRUE)
  rf1.pred = predict(rf1, test[,-1], type = "class") 
  cm = table(true = test[,1], predicted = rf1.pred)
  rf.test.acc[i] = (cm[1,1] + cm[2,2]) / dim(test)[1]
}
mean(rf.test.acc) # 0.6861111

# # # COMPARING CLASSIFIERS # # # ------------------------------------------------------------------------

# Tree:          0.5555556
# Pruned tree:   0.5944444
# Adaboosting:   0.6583333 < < <
# ANN:           0.6366667 
# Random forest: 0.6861111 < < <
