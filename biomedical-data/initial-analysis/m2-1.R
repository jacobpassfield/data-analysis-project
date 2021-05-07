load(file = "biomedical-data/data/biomedical-data.RData")

# # # # # NAIVE BAYES # # # ------------------------------------------------------------------------------

library(caret)

# # # LOOCV # # # ----------------------------------------------------------------------------------------

nb = train(class ~ ., data = data[,c(1,5)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"))
nb$results
# laplace usekernel adjust  Accuracy    Kappa
#       0     FALSE      1 0.6958763 0.268065
#       0      TRUE      1 0.6752577 0.224394

nbs = train(class ~ ., data = data[,c(1,5)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
nbs$results
# laplace usekernel adjust  Accuracy     Kappa
#       0     FALSE      1 0.6958763 0.268065
#       0      TRUE      1 0.6752577 0.224394

# Scaling leads to the same results, do not use scaled data.

# Changing the tuning paramters

grid1 = expand.grid(laplace = 0, usekernel = F, adjust = 1)
nb1 = train(class ~ ., data = data[,c(1,5)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid1)
nb1$results

grid2 = expand.grid(laplace = 0, usekernel = T, adjust = 1)
nb2 = train(class ~ ., data = data[,c(1,5)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid2)
nb2$results

grid3 = expand.grid(laplace = 1, usekernel = F, adjust = 1)
nb3 = train(class ~ ., data = data[,c(1,5)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid3)
nb3$results

grid4 = expand.grid(laplace = 1, usekernel = T, adjust = 1)
nb4 = train(class ~ ., data = data[,c(1,5)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid4)
nb4$results

grid5 = expand.grid(laplace = 0, usekernel = T, adjust = 0.5)
nb5 = train(class ~ ., data = data[,c(1,5)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid5)
nb5$results

grid6 = expand.grid(laplace = 0, usekernel = F, adjust = 0.5)
nb6 = train(class ~ ., data = data[,c(1,5)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid6)
nb6$results

#    laplace usekernel adjust  Accuracy    Kappa
# nb1      0     FALSE      1 0.6958763 0.268065 < < <
# nb2      0      TRUE      1 0.6752577 0.224394
# nb3      1     FALSE      1 0.6958763 0.268065
# nb4      1      TRUE      1 0.6752577 0.224394
# nb5      0      TRUE    0.5 0.556701 -0.04680637
# nb6      0     FALSE    0.5 0.6958763 0.268065

# # # LDA # # # -----------------------------------------------------------------------------------------

# # LOOCV # # -------------------------------------------------------------------------------------------

lda = train(class ~ ., data = data[,c(1,5)], method = "lda", trControl = trainControl(method = "LOOCV"))
lda$results # 0.6958763

ldaS = train(class ~ ., data = data[,c(1,5)], method = "lda", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
ldaS$results # 0.6958763

# # # ANN # # # -------------------------------------------------------------------------------------------

# # LOOCV # # ---------------------------------------------------------------------------------------------

library(caret)

nn = train(class ~ ., data = data[,c(1,5)], method = "nnet", trControl = trainControl(method = "LOOCV"))
nn$results

#   size decay  Accuracy      Kappa
# 1    1 0e+00 0.6649485 0.03872542
# 2    1 1e-04 0.6546392 0.00000000
# 3    1 1e-01 0.6649485 0.11929040
# 4    3 0e+00 0.6494845 0.06691187
# 5    3 1e-04 0.6855670 0.12972496
# 6    3 1e-01 0.6958763 0.25081817 < < <
# 7    5 0e+00 0.6752577 0.12437312
# 8    5 1e-04 0.6649485 0.11929040
# 9    5 1e-01 0.6958763 0.26240495

nnS = train(class ~ ., data = data[,c(1,5)], method = "nnet", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
nnS$results

#   size decay  Accuracy     Kappa
# 1    1 0e+00 0.6855670 0.2657898 
# 2    1 1e-04 0.6855670 0.2712157
# 3    1 1e-01 0.6855670 0.2818303 < < <
# 4    3 0e+00 0.6494845 0.2080682
# 5    3 1e-04 0.6597938 0.2368578
# 6    3 1e-01 0.6752577 0.2360295
# 7    5 0e+00 0.6546392 0.2335181
# 8    5 1e-04 0.6391753 0.2020214
# 9    5 1e-01 0.6804124 0.2396005

# # # COMPARING CLASSIFIERS # # # ------------------------------------------------------------------------

# NB: 0.6958763
# LDA: 0.6958763
# NN: 0.6958763 < < <
