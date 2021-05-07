load(file = "biomedical-data/data/biomedical-data.RData")

library(caret)

set.seed(1234)
trainIndex = createDataPartition(data$class, p = 0.7, list = F)
train = data[trainIndex,]
test = data[-trainIndex,]

# # # # # NAIVE BAYES # # # ------------------------------------------------------------------------------

nb = train(class ~ ., data = train[,c(1,4:7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"))
nb$results
# laplace usekernel adjust  Accuracy     Kappa
#       0     FALSE      1 0.9117647 0.7946136
#       0      TRUE      1 0.9191176 0.8146680

nbs = train(class ~ ., data = train[,c(1,4:7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
nbs$results
# laplace usekernel adjust  Accuracy     Kappa
#       0     FALSE      1 0.9117647 0.7946136
#       0      TRUE      1 0.9191176 0.8146680

# Changing the tuning paramters

grid1 = expand.grid(laplace = 0, usekernel = F, adjust = 1)
nb1 = train(class ~ ., data = train[,c(1,4:7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid1)
nb1$results

grid2 = expand.grid(laplace = 0, usekernel = T, adjust = 1)
nb2 = train(class ~ ., data = train[,c(1,4:7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid2)
nb2$results

grid3 = expand.grid(laplace = 1, usekernel = F, adjust = 1)
nb3 = train(class ~ ., data = train[,c(1,4:7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid3)
nb3$results

grid4 = expand.grid(laplace = 1, usekernel = T, adjust = 1)
nb4 = train(class ~ ., data = train[,c(1,4:7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid4)
nb4$results

grid5 = expand.grid(laplace = 0, usekernel = T, adjust = 0.5)
nb5 = train(class ~ ., data = train[,c(1,4:7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid5)
nb5$results

grid6 = expand.grid(laplace = 0, usekernel = F, adjust = 0.5)
nb6 = train(class ~ ., data = train[,c(1,4:7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid6)
nb6$results

#    laplace usekernel adjust  Accuracy     Kappa
# nb1      0     FALSE      1 0.9117647 0.7946136
# nb2      0      TRUE      1 0.9191176 0.8146680 < < <
# nb3      1     FALSE      1 0.9117647 0.7946136
# nb4      1      TRUE      1 0.9191176 0.8146680
# nb5      0      TRUE    0.5 0.8676471 0.6982992
# nb6      0     FALSE    0.5 0.9117647 0.7946136

# BEST! laplace = 0, usekernel = T, adjust = 1 --- 0.9191176

# As a confusion matrix

grid = expand.grid(laplace = 0, usekernel = T, adjust = 1)
nb = train(class ~ ., data = train[,c(1,4:7)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid)
cm = table(true = nb$pred[,2], predicted = nb$pred[,1])
cm
(cm[1,1] + cm[2,2]) / dim(train)[1] # 0.9191176
test.pred = predict(nb, test[,c(1,4:7)])
cm = table(true = test[,1], predicted = as.factor(test.pred))
cm
(cm[1,1] + cm[2,2]) / dim(test)[1] # 0.8103448

# # # LDA # # # -----------------------------------------------------------------------------------------

lda = train(class ~ ., data = train[,c(1,4:7)], method = "lda", trControl = trainControl(method = "LOOCV"))
lda$results # 0.875

ldaS = train(class ~ ., data = train[,c(1,4:7)], method = "lda", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
ldaS$results # 0.875

lda = train(class ~ ., data = train[,c(1,4:7)], method = "lda", trControl = trainControl(method = "LOOCV"))
cm = table(true = lda$pred[,2], predicted = lda$pred[,1])
cm
(cm[1,1] + cm[2,2]) / dim(train)[1] # 0.875
test.pred = predict(lda, test[,c(1,4:7)])
cm = table(true = test[,1], predicted = test.pred)
cm
(cm[1,1] + cm[2,2]) / dim(test)[1] # 0.7931034

# # # ANN # # # -------------------------------------------------------------------------------------------

library(caret)

nn = train(class ~ ., data = train[,c(1,4:7)], method = "nnet", trControl = trainControl(method = "LOOCV"))
nn$results

#   size decay  Accuracy     Kappa
# 1    1 0e+00 0.7352941 0.2943211
# 2    1 1e-04 0.6764706 0.1161004
# 3    1 1e-01 0.8235294 0.5614082
# 4    3 0e+00 0.7500000 0.3786617
# 5    3 1e-04 0.7426471 0.3567568
# 6    3 1e-01 0.8455882 0.6386640
# 7    5 0e+00 0.7794118 0.4697167
# 8    5 1e-04 0.7647059 0.4281209
# 9    5 1e-01 0.8235294 0.5935243

nnS = train(class ~ ., data = train[,c(1,4:7)], method = "nnet", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
nnS$results

#   size decay  Accuracy     Kappa
# 1    1 0e+00 0.9191176 0.8165768
# 2    1 1e-04 0.9264706 0.8341059 < < <
# 3    1 1e-01 0.9191176 0.8165768
# 4    3 0e+00 0.8750000 0.7135778
# 5    3 1e-04 0.9044118 0.7832271
# 6    3 1e-01 0.9264706 0.8341059
# 7    5 0e+00 0.8455882 0.6603235
# 8    5 1e-04 0.8676471 0.7102959
# 9    5 1e-01 0.9264706 0.8341059

# Scaled data wins

nngrid = expand.grid(size = 1, decay = 0.0001)
nn = train(class ~ ., data = train[,c(1,4:7)], method = "nnet", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"), tuneGrid = nngrid)
cm = table(true = nn$pred[,2], predicted = nn$pred[,1])
cm
(cm[1,1] + cm[2,2]) / dim(train)[1] # 0.9264706
test.pred = predict(nn, test[,c(1,4:7)])
cm = table(true = test[,1], predicted = test.pred)
cm
(cm[1,1] + cm[2,2]) / dim(test)[1] # 0.862069

# Increasing iterations leads to same accuracy.

# # # COMPARING CLASSIFIERS # # # ------------------------------------------------------------------------

# NB:  0.8103448
# LDA: 0.7931034 
# ANN: 0.8620690 < < <
