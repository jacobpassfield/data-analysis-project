load(file = "biomedical-data/data/biomedical-data.RData")

# # # # # NAIVE BAYES # # # ------------------------------------------------------------------------------

library(caret)

# # # LOOCV # # # ----------------------------------------------------------------------------------------

nb = train(class ~ ., data = data[,c(1,6)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"))
nb$results
# laplace usekernel adjust  Accuracy     Kappa
#       0     FALSE      1 0.7628866 0.3993808
#       0      TRUE      1 0.7886598 0.4874339

nbs = train(class ~ ., data = data[,c(1,6)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
nbs$results
# laplace usekernel adjust  Accuracy     Kappa
#       0     FALSE      1 0.7628866 0.3993808
#       0      TRUE      1 0.7886598 0.4874339

# Scaling leads to the same results, do not use scaled data.

# Changing the tuning paramters

grid1 = expand.grid(laplace = 0, usekernel = F, adjust = 1)
nb1 = train(class ~ ., data = data[,c(1,6)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid1)
nb1$results

grid2 = expand.grid(laplace = 0, usekernel = T, adjust = 1)
nb2 = train(class ~ ., data = data[,c(1,6)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid2)
nb2$results

grid3 = expand.grid(laplace = 1, usekernel = F, adjust = 1)
nb3 = train(class ~ ., data = data[,c(1,6)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid3)
nb3$results

grid4 = expand.grid(laplace = 1, usekernel = T, adjust = 1)
nb4 = train(class ~ ., data = data[,c(1,6)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid4)
nb4$results

grid5 = expand.grid(laplace = 0, usekernel = T, adjust = 0.5)
nb5 = train(class ~ ., data = data[,c(1,6)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid5)
nb5$results

grid6 = expand.grid(laplace = 0, usekernel = F, adjust = 0.5)
nb6 = train(class ~ ., data = data[,c(1,6)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid6)
nb6$results

#    laplace usekernel adjust  Accuracy     Kappa
# nb1      0     FALSE      1 0.7628866 0.3993808
# nb2      0      TRUE      1 0.7886598 0.4874339 < < <
# nb3      1     FALSE      1 0.7628866 0.3993808
# nb4      1      TRUE      1 0.7886598 0.4874339
# nb5      0      TRUE    0.5 0.7835052 0.480953
# nb6      0     FALSE    0.5 0.7628866 0.3993808

# # # LDA # # # -----------------------------------------------------------------------------------------

# # LOOCV # # -------------------------------------------------------------------------------------------

lda = train(class ~ ., data = data[,c(1,6)], method = "lda", trControl = trainControl(method = "LOOCV"))
lda$results # 0.7474227

ldaS = train(class ~ ., data = data[,c(1,6)], method = "lda", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
ldaS$results # 0.7474227

# # # ANN # # # -------------------------------------------------------------------------------------------

# # LOOCV # # ---------------------------------------------------------------------------------------------

library(caret)

nn = train(class ~ ., data = data[,c(1,6)], method = "nnet", trControl = trainControl(method = "LOOCV"))
nn$results

#   size decay  Accuracy      Kappa
# 1    1 0e+00 0.6649485 0.03872542
# 1    1 0e+00 0.7525773 0.3732669
# 2    1 1e-04 0.7886598 0.4793821
# 3    1 1e-01 0.7989691 0.5161785
# 4    3 0e+00 0.7835052 0.4769547
# 5    3 1e-04 0.7835052 0.4769547
# 6    3 1e-01 0.8041237 0.5303860 < < <
# 7    5 0e+00 0.7938144 0.5018616
# 8    5 1e-04 0.7835052 0.4769547
# 9    5 1e-01 0.8041237 0.5303860

nnS = train(class ~ ., data = data[,c(1,6)], method = "nnet", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
nnS$results

#   size decay  Accuracy     Kappa
# 1    1 0e+00 0.7938144 0.5018616
# 2    1 1e-04 0.7989691 0.5161785
# 3    1 1e-01 0.7989691 0.5161785 < < <
# 4    3 0e+00 0.7886598 0.4913672
# 5    3 1e-04 0.7886598 0.4913672
# 6    3 1e-01 0.7989691 0.5161785
# 7    5 0e+00 0.7577320 0.4213733
# 8    5 1e-04 0.7989691 0.5234916
# 9    5 1e-01 0.7989691 0.5161785

# # # COMPARING CLASSIFIERS # # # ------------------------------------------------------------------------

# NB: 0.7886598
# LDA: 0.7474227
# NN: 0.7989691 < < <
