data = read.table("dna-data/data/human-phage.txt")

# NUMBER OF As

numAs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  numAs[i] = length(which(data[i,] == "A")) }
numAs

# NUMBER OF Ts

numTs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  numTs[i] = length(which(data[i,] == "T")) }
numTs

# NUMBER OF Gs

numGs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  numGs[i] = length(which(data[i,] == "G")) }
numGs

# NUMBER OF Cs

numCs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  numCs[i] = length(which(data[i,] == "C")) }
numCs

# NUMBER OF CONSECUTIVE As

numAAs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    numAAs[i] = numAAs[i] + ((data[i,j] == "A") & (data[i,j+1] == "A"))} 
  }
numAAs

# NUMBER OF CONSECUTIVE Ts

numTTs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    numTTs[i] = numTTs[i] + ((data[i,j] == "T") & (data[i,j+1] == "T"))} 
}
numTTs

# NUMBER OF CONSECUTIVE Gs

numGGs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    numGGs[i] = numGGs[i] + ((data[i,j] == "G") & (data[i,j+1] == "G"))} 
}
numGGs

# NUMBER OF CONSECUTIVE Cs

numCCs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    numCCs[i] = numCCs[i] + ((data[i,j] == "C") & (data[i,j+1] == "C"))} 
}
numCCs

# NUMBER OF ATs

numATs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    numATs[i] = numATs[i] + ((data[i,j] == "A") & (data[i,j+1] == "T"))} 
}
numATs

# NUMBER OF TAs

numTAs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    numTAs[i] = numTAs[i] + ((data[i,j] == "T") & (data[i,j+1] == "A"))} 
}
numTAs

# NUMBER OF CGs

numCGs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    numCGs[i] = numCGs[i] + ((data[i,j] == "C") & (data[i,j+1] == "G"))} 
}
numCGs

# NUMBER OF GCs

numGCs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-1)){
    numGCs[i] = numGCs[i] + ((data[i,j] == "G") & (data[i,j+1] == "C"))} 
}
numGCs

# NUMBER OF GAGs

numGAGs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-2)){
    numGAGs[i] = numGAGs[i] + ((data[i,j] == "G") & (data[i,j+1] == "A") & (data[i,j+2] == "G"))} 
}
numGAGs

# NUMBER OF AGAs

numAGAs = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){
  for (j in 1:(ncol(data)-2)){
    numAGAs[i] = numAGAs[i] + ((data[i,j] == "A") & (data[i,j+1] == "G") & (data[i,j+2] == "A"))} 
}
numAGAs

# PROPORTION OF C/G to A/T

propCGtoAT = vector(mode = "integer", length = nrow(data)) 
for (i in 1:nrow(data)){ 
  propCGtoAT[i] = ( numCs[i] / numGs[i] ) / ( numAs[i] / numTs[i] )
  }
propCGtoAT

# Adding to data

EFdata = data

EFdata$numAs = numAs
EFdata$numTs = numTs
EFdata$numGs = numGs
EFdata$numCs = numCs
EFdata$numAAs = numAAs
EFdata$numTTs = numTTs
EFdata$numGGs = numGGs
EFdata$numCCs = numCCs
EFdata$numATs = numATs
EFdata$numTAs = numTAs
EFdata$numCGs = numCGs
EFdata$numGCs = numGCs
EFdata$numGAGs = numGAGs
EFdata$numAGAs = numAGAs

EFdata = EFdata[,c(1,102:115)]

#########################################################################################################

library(caret)

set.seed(1234)
trainIndex = createDataPartition(EFdata$V1, p = 0.7, list = F)
trainEF = EFdata[trainIndex,]
testEF = EFdata[-trainIndex,]

# # # DECISION TREES # # # ---------------------------------------------------------------------------------

library(tree)

tree = tree(V1~., data = trainEF)
plot(tree)
text(tree, cex = 0.8)

# Training
tree.pred = predict(tree, data = trainEF[,-1], type = "class")
table(predicted = tree.pred, true = trainEF[,1])
#           true
# predicted neg pos
#       neg 202   5
#       pos   8 205
(202+205)/dim(trainEF)[1]
# 0.9690476

# Test
tree.pred = predict(tree, testEF[,-1], type = "class")
table(predicted = tree.pred, true = testEF[,1])
#           true
# predicted neg pos
#       neg  77   6
#       pos  13  84
(77+84)/dim(testEF)[1] 
# 0.8944444

# # # PRUNING # # # --------------------------------------------------------------------------------------

cv = cv.tree(tree, FUN = prune.misclass)
plot(cv)
# Lowest misclassification rate is 17 nodes

prune = prune.misclass(tree, best = 2)
plot(prune)
text(prune, cex = 0.7)

# Training
prune.pred = predict(prune, trainEF[,-1], type = "class")
table(predicted = prune.pred, true = trainEF[,1])
#           true
# predicted neg pos
#       neg 198  17
#       pos  12 193
(198+192)/dim(trainEF)[1]
# 0.9285714

# Test
prune.pred = predict(prune, testEF[,-1], type = "class")
table(predicted = prune.pred, true = testEF[,1])
#           true
# predicted neg pos
#       neg  82  11
#       pos   8  79
(82+79)/dim(testEF)[1] 
# 0.8944444 

# # # ADABOOSTING # # # -----------------------------------------------------------------------------------

library(adabag)
boost = boosting(V1 ~ ., data = trainEF, control = rpart.control(maxdepth = 1)) # 100 iterations default

# Test data

boost.pred = predict.boosting(boost, newdata = testEF[,-1]) 
cm = table(true = testEF[,1], predicted = boost.pred$class)
cm
#       predicted
# true  neg pos
#  neg  85    5
#  pos   9   81
(cm[1,1]+cm[2,2])/dim(testEF)[1] # 0.9222222

boost.test.acc = double(10)
for(i in 1:10){
  boost1 = boosting(V1 ~ ., data = trainEF, control = rpart.control(maxdepth = 1))  
  boost1.pred = predict.boosting(boost1, newdata = testEF[,-1]) 
  cm = table(true = testEF[,1], predicted = boost1.pred$class)
  boost.test.acc[i] = (cm[1,1] + cm[2,2]) / dim(testEF)[1]
}
mean(boost.test.acc) # 0.9138889

# # # ANN # # # ------------------------------------------------------------------------------------------

library(nnet)

nn = nnet(V1 ~ ., data = trainEF, size = 10)
nn.pred = predict(nn, testEF[,-1], type = "class") 
table(true = testEF[,1], predicted = as.factor(nn.pred))
#      predicted
# true neg pos
#  neg  81   9
#  pos  12  78
(86+78)/dim(testEF)[1] # 0.8833333

# 10 HIDDEN NODES
nn10.test.acc = double(10)
for(i in 1:10){
  nn10 = nnet(V1 ~ ., data = trainEF, size = 10)  
  nn10.pred = predict(nn10, testEF[,-1], type = "class") 
  cm = table(true = testEF$V1, predicted = as.factor(nn10.pred))
  nn10.test.acc[i] = (cm[1,1] + cm[2,2]) / dim(testEF)[1]
}
mean(nn10.test.acc) # 0.9022222

# # # RANDOM FOREST # # # ---------------------------------------------------------------------------------

library(randomForest)

rf = randomForest(V1 ~ ., data = trainEF, importance = TRUE)
rf # OOB error rate 5.95%

rf.pred = predict(rf, testEF[,-1], type = "class") 
cm = table(predicted = rf.pred, true = testEF[,1])
cm
#           true
# predicted neg pos
#       neg  80   8
#       pos  10  82
(cm[1,1]+cm[2,2])/dim(testEF)[1]
# 0.9111111
# Error rate: 0.1 -- lower rate than OOB

rf.test.acc = double(10)
for(i in 1:10){
  rf1 = randomForest(V1 ~ ., data = trainEF, importance = TRUE)
  rf1.pred = predict(rf1, testEF[,-1], type = "class") 
  cm = table(true = testEF[,1], predicted = rf1.pred)
  rf.test.acc[i] = (cm[1,1] + cm[2,2]) / dim(testEF)[1]
}
mean(rf.test.acc) # 0.9094444

# # # COMPARING CLASSIFIERS # # # ------------------------------------------------------------------------

# Tree:          0.8944444
# Pruned tree:   0.8944444
# Adaboosting:   0.9166667 < < <
# ANN:           0.9022222
# Random forest: 0.9072222 < < <

# Do variable importance here.







