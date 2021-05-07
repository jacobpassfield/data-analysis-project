load(file = "biomedical-data/data/biomedical-data.RData")

library(caret)

set.seed(1234)
trainIndex = createDataPartition(data$class, p = 0.7, list = F)
train = data[trainIndex,]
test = data[-trainIndex,]

# # # # # NAIVE BAYES # # # ------------------------------------------------------------------------------

nb = train(class ~ ., data = train[,c(1,4,7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"))
nb$results
# laplace usekernel adjust  Accuracy     Kappa
#       0     FALSE      1 0.8676471 0.6886288
#       0      TRUE      1 0.8676471 0.6951432

nbs = train(class ~ ., data = train[,c(1,4,7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
nbs$results
# laplace usekernel adjust  Accuracy     Kappa
#       0     FALSE      1 0.8676471 0.6886288
#       0      TRUE      1 0.8676471 0.6951432

# Changing the tuning paramters

grid1 = expand.grid(laplace = 0, usekernel = F, adjust = 1)
nb1 = train(class ~ ., data = train[,c(1,4,7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid1)
nb1$results

grid2 = expand.grid(laplace = 0, usekernel = T, adjust = 1)
nb2 = train(class ~ ., data = train[,c(1,4,7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid2)
nb2$results

grid3 = expand.grid(laplace = 1, usekernel = F, adjust = 1)
nb3 = train(class ~ ., data = train[,c(1,4,7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid3)
nb3$results

grid4 = expand.grid(laplace = 1, usekernel = T, adjust = 1)
nb4 = train(class ~ ., data = train[,c(1,4,7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid4)
nb4$results

grid5 = expand.grid(laplace = 0, usekernel = T, adjust = 0.5)
nb5 = train(class ~ ., data = train[,c(1,4,7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid5)
nb5$results

grid6 = expand.grid(laplace = 0, usekernel = F, adjust = 0.5)
nb6 = train(class ~ ., data = train[,c(1,4,7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid6)
nb6$results

#    laplace usekernel adjust  Accuracy     Kappa
# nb1      0     FALSE      1 0.8676471 0.6886288 < < <
# nb2      0      TRUE      1 0.8676471 0.6951432
# nb3      1     FALSE      1 0.8676471 0.6886288
# nb4      1      TRUE      1 0.8676471 0.6951432
# nb5      0      TRUE    0.5 0.8455882 0.6461843
# nb6      0     FALSE    0.5 0.8676471 0.6886288

# BEST! laplace = 0, usekernel = F, adjust = 1 --- 0.8676471

# As a confusion matrix

grid = expand.grid(laplace = 0, usekernel = F, adjust = 1)
nb = train(class ~ ., data = train[,c(1,4,7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid)
cm = table(true = nb$pred[,2], predicted = nb$pred[,1])
cm
(cm[1,1] + cm[2,2]) / dim(train)[1] # 0.8676471
test.pred = predict(nb, test[,c(1,4,7)])
cm = table(true = test[,1], predicted = as.factor(test.pred))
cm
(cm[1,1] + cm[2,2]) / dim(test)[1] # 0.8275862

# # # LDA # # # -----------------------------------------------------------------------------------------

lda = train(class ~ ., data = train[,c(1,4,7)], method = "lda", trControl = trainControl(method = "LOOCV"))
lda$results # 0.8602941

ldaS = train(class ~ ., data = train[,c(1,4,7)], method = "lda", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
ldaS$results # 0.8602941

lda = train(class ~ ., data = train[,c(1,4,7)], method = "lda", trControl = trainControl(method = "LOOCV"))
cm = table(true = lda$pred[,2], predicted = lda$pred[,1])
cm
(cm[1,1] + cm[2,2]) / dim(train)[1] # 0.8602941
test.pred = predict(lda, test[,c(1,4,7)])
cm = table(true = test[,1], predicted = test.pred)
cm
(cm[1,1] + cm[2,2]) / dim(test)[1] # 0.7758621

# # # ANN # # # -------------------------------------------------------------------------------------------

library(caret)

nn = train(class ~ ., data = train[,c(1,4,7)], method = "nnet", trControl = trainControl(method = "LOOCV"))
nn$results

#   size decay  Accuracy      Kappa
# 1    1 0e+00 0.6544118 0.03792896
# 2    1 1e-04 0.6911765 0.17670799
# 3    1 1e-01 0.7867647 0.46090760
# 4    3 0e+00 0.6985294 0.20114613
# 5    3 1e-04 0.7500000 0.36431125
# 6    3 1e-01 0.8382353 0.63503293
# 7    5 0e+00 0.7867647 0.47300909
# 8    5 1e-04 0.7794118 0.47544356
# 9    5 1e-01 0.8602941 0.67988107

nnS = train(class ~ ., data = train[,c(1,4,7)], method = "nnet", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
nnS$results

#   size decay  Accuracy     Kappa
# 1    1 0e+00 0.8602941 0.6798811 < < <
# 2    1 1e-04 0.8602941 0.6798811
# 3    1 1e-01 0.8529412 0.6612702
# 4    3 0e+00 0.8308824 0.6042510
# 5    3 1e-04 0.8382353 0.6153253
# 6    3 1e-01 0.8529412 0.6612702
# 7    5 0e+00 0.8529412 0.6540321
# 8    5 1e-04 0.8455882 0.6424637
# 9    5 1e-01 0.8529412 0.6612702

# Scaled data wins

nngrid = expand.grid(size = 1, decay = 0)
nn = train(class ~ ., data = train[,c(1,4,7)], method = "nnet", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"), tuneGrid = nngrid)
cm = table(true = nn$pred[,2], predicted = nn$pred[,1])
cm
(cm[1,1] + cm[2,2]) / dim(train)[1] # 0.8602941
test.pred = predict(nn, test[,c(1,4,7)])
cm = table(true = test[,1], predicted = test.pred)
cm
(cm[1,1] + cm[2,2]) / dim(test)[1] # 0.8275862

# Increasing iterations leads to same accuracy.

# # # COMPARING CLASSIFIERS # # # ------------------------------------------------------------------------

# NB:  0.8275862
# LDA: 0.7758621 
# ANN: 0.8275862 < < <
