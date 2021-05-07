load(file = "biomedical-data/data/biomedical-data.RData")

library(caret)

set.seed(1234)
trainIndex = createDataPartition(data$class, p = 0.7, list = F)
train = data[trainIndex,]
test = data[-trainIndex,]

# # # # # NAIVE BAYES # # # ------------------------------------------------------------------------------

nb = train(class ~ ., data = train[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"))
nb$results
# laplace usekernel adjust  Accuracy     Kappa
#       0     FALSE      1 0.8161765 0.5507400
#       0      TRUE      1 0.8235294 0.5710907

nbs = train(class ~ ., data = train[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
nbs$results
# laplace usekernel adjust  Accuracy     Kappa
#       0     FALSE      1 0.8235294 0.5710907
#       0      TRUE      1 0.8235294 0.5710907

# Use scaled data.

# Changing the tuning paramters

grid1 = expand.grid(laplace = 0, usekernel = F, adjust = 1)
nb1 = train(class ~ ., data = train[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid1, preProcess = c("center", "scale"))
nb1$results

grid2 = expand.grid(laplace = 0, usekernel = T, adjust = 1)
nb2 = train(class ~ ., data = train[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid2, preProcess = c("center", "scale"))
nb2$results

grid3 = expand.grid(laplace = 1, usekernel = F, adjust = 1)
nb3 = train(class ~ ., data = train[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid3, preProcess = c("center", "scale"))
nb3$results

grid4 = expand.grid(laplace = 1, usekernel = T, adjust = 1)
nb4 = train(class ~ ., data = train[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid4, preProcess = c("center", "scale"))
nb4$results

grid5 = expand.grid(laplace = 0, usekernel = T, adjust = 0.5)
nb5 = train(class ~ ., data = train[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid5, preProcess = c("center", "scale"))
nb5$results

grid6 = expand.grid(laplace = 0, usekernel = F, adjust = 0.5)
nb6 = train(class ~ ., data = train[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid6, preProcess = c("center", "scale"))
nb6$results

#    laplace usekernel adjust  Accuracy     Kappa
# nb1      0     FALSE      1 0.8235294 0.5710907
# nb2      0      TRUE      1 0.8235294 0.5710907
# nb3      1     FALSE      1 0.8235294 0.5710907
# nb4      1      TRUE      1 0.8235294 0.5710907
# nb5      0      TRUE    0.5 0.8088235 0.5404211
# nb6      0     FALSE    0.5 0.8041237 0.5193637

# BEST! laplace = 0, usekernel = T, adjust = 1 --- 0.8235294

# As a confusion matrix
grid = expand.grid(laplace = 0, usekernel = T, adjust = 1)
nb = train(class ~ ., data = train[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid, preProcess = c("center", "scale"))
table(true = nb$pred[,2], predicted = nb$pred[,1])
#     predicted
# true   0   1
#    0  86   3
#    1  21  26
(86+26)/dim(train)[1] # 0.8235294

grid = expand.grid(laplace = 0, usekernel = T, adjust = 1)
nb = train(class ~ ., data = train[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid, preProcess = c("center", "scale"))
cm = table(true = nb$pred[,2], predicted = nb$pred[,1])
cm
(cm[1,1] + cm[2,2]) / dim(train)[1] # 0.8235294
test.pred = predict(nb, test[,c(1,4)])
cm = table(true = test[,1], predicted = as.factor(test.pred))
cm
(cm[1,1] + cm[2,2]) / dim(test)[1] # 0.7758621

# # # LDA # # # -----------------------------------------------------------------------------------------

lda = train(class ~ ., data = train[,c(1,4)], method = "lda", trControl = trainControl(method = "LOOCV"))
lda$results # 0.7720588

ldaS = train(class ~ ., data = train[,c(1,4)], method = "lda", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
ldaS$results # 0.7720588

# As a confusion matrix
lda = train(class ~ ., data = train[,c(1,4)], method = "lda", trControl = trainControl(method = "LOOCV"))
table(true = lda$pred[,2], predicted = lda$pred[,1])
#     predicted
# true   0   1
#    0  89   0
#    1  31  16
(89+16)/dim(train)[1] # 0.7720588

lda = train(class ~ ., data = train[,c(1,4)], method = "lda", trControl = trainControl(method = "LOOCV"))
cm = table(true = lda$pred[,2], predicted = lda$pred[,1])
cm
(cm[1,1] + cm[2,2]) / dim(train)[1] # 0.7720588
test.pred = predict(lda, test[,c(1,4)])
cm = table(true = test[,1], predicted = as.factor(test.pred))
cm
(cm[1,1] + cm[2,2]) / dim(test)[1] # 0.6896552

# # # ANN # # # -------------------------------------------------------------------------------------------

library(caret)

nn = train(class ~ ., data = train[,c(1,4)], method = "nnet", trControl = trainControl(method = "LOOCV"))
nn$results

#   size decay  Accuracy     Kappa
# 1    1 0e+00 0.7058824 0.1964549
# 2    1 1e-04 0.7279412 0.2613036
# 3    1 1e-01 0.7941176 0.4764916
# 4    3 0e+00 0.7205882 0.2459877
# 5    3 1e-04 0.7500000 0.3492823
# 6    3 1e-01 0.8382353 0.6111256
# 7    5 0e+00 0.8088235 0.5353482
# 8    5 1e-04 0.8088235 0.5353482
# 9    5 1e-01 0.8455882 0.6308170 < < <

nnS = train(class ~ ., data = train[,c(1,4)], method = "nnet", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
nnS$results

#   size decay  Accuracy     Kappa
# 1    1 0e+00 0.8455882 0.6308170 < < <
# 2    1 1e-04 0.8455882 0.6308170
# 3    1 1e-01 0.8308824 0.5912180
# 4    3 0e+00 0.8308824 0.5956567
# 5    3 1e-04 0.8382353 0.6111256
# 6    3 1e-01 0.8308824 0.5912180
# 7    5 0e+00 0.8308824 0.5956567
# 8    5 1e-04 0.8308824 0.5912180
# 9    5 1e-01 0.8308824 0.5912180

# Scaled data wins

nngrid = expand.grid(size = 1, decay = 0)
nn = train(class ~ ., data = train[,c(1,4)], method = "nnet", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"), tuneGrid = nngrid)
nn$results
# size decay  Accuracy    Kappa
#    1     0 0.8455882 0.630817

table(true = nn$pred[,2], predicted = nn$pred[,1])
#      predicted
# true   0   1
#    0  86   3
#    1  18  29
(86+29)/dim(train)[1] # 0.8455882

nngrid = expand.grid(size = 1, decay = 0)
nn = train(class ~ ., data = train[,c(1,4)], method = "nnet", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"), tuneGrid = nngrid)
cm = table(true = nn$pred[,2], predicted = nn$pred[,1])
cm
(cm[1,1] + cm[2,2]) / dim(train)[1] # 0.8455882
test.pred = predict(nn, test[,c(1,4)])
cm = table(true = test[,1], predicted = as.factor(test.pred))
cm
(cm[1,1] + cm[2,2]) / dim(test)[1] # 0.8103448

nngrid = expand.grid(size = 1, decay = 0)
nn.test.acc = double(10)
for(i in 1:10){
  nn = train(class ~ ., data = train[,c(1,4)], method = "nnet", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"), tuneGrid = nngrid)
  test.pred = predict(nn, test[,c(1,4)]) 
  cm = table(true = test[,1], predicted = test.pred)
  nn.test.acc[i] = (cm[1,1] + cm[2,2]) / dim(test)[1]
}
mean(nn.test.acc) # 0.8103448

# Increasing iterations leads to same accuracy.

# # # COMPARING CLASSIFIERS # # # ------------------------------------------------------------------------

# NB:  0.7758621
# LDA: 0.6896552
# ANN: 0.8103448 < < <

