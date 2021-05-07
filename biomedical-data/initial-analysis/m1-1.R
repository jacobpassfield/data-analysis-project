load(file = "biomedical-data/data/biomedical-data.RData")

# # # # # NAIVE BAYES # # # ------------------------------------------------------------------------------

library(caret)

# k-fold cross validation was considered, but since results varied due to different sampling subgroups,
# LOOCV was used.

# # # LOOCV # # # ----------------------------------------------------------------------------------------

nb = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"))
nb$results
# laplace usekernel adjust  Accuracy     Kappa
#       0     FALSE      1 0.8041237 0.5193637
#       0      TRUE      1 0.8195876 0.5624436

nbs = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
nbs$results
# laplace usekernel adjust  Accuracy     Kappa
#       0     FALSE      1 0.8041237 0.5193637
#       0      TRUE      1 0.8195876 0.5624436

# Scaling leads to the same results, do not use scaled data.

# Changing the tuning paramters

nb1 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"))
nb1$results

grid2 = expand.grid(laplace = 1, usekernel = F, adjust = 1)
nb2 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid2)
nb2$results

grid3 = expand.grid(laplace = 1, usekernel = T, adjust = 1)
nb3 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid3)
nb3$results

grid4 = expand.grid(laplace = 0, usekernel = T, adjust = 0.5)
nb4 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid4)
nb4$results

grid5 = expand.grid(laplace = 0, usekernel = F, adjust = 0.5)
nb5 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid5)
nb5$results

#    laplace usekernel adjust  Accuracy     Kappa
# nb1      0     FALSE      1 0.8041237 0.5193637
# nb1      0      TRUE      1 0.8195876 0.5624436
# nb2      1     FALSE      1 0.8041237 0.5193637
# nb3      1      TRUE      1 0.8195876 0.5624436
# nb4      0      TRUE    0.5 0.8144330 0.5551026
# nb5      0     FALSE    0.5 0.8041237 0.5193637

# BEST nb1 - 81.96%: laplace = 0, usekernel = T, adjust = 1

# As a confusion matrix
grid = expand.grid(laplace = 0, usekernel = T, adjust = 1)
nb = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = trainControl(method = "LOOCV"), tuneGrid = grid)
table(true = nb$pred[,2], predicited = nb$pred[,1])
#     predicited
# true   0   1
#    0 122   5
#    1  30  37
# So accuracy is 0.8195876

# # # CV k-fold # # # ------------------------------------------------------------------------------------

# 3-fold # ----------

tC3 = trainControl(method = "cv", number = 3, savePredictions = T)

nb3 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC3)
nb3$results
# usekernel laplace adjust  Accuracy     Kappa AccuracySD    KappaSD
#     FALSE       0      1 0.8092949 0.5289796 0.02324326 0.06764044
#      TRUE       0      1 0.8195513 0.5573331 0.05419132 0.15086071

nbs3 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC3, preProcess = c("center", "scale"))
nbs3$results
# usekernel laplace adjust  Accuracy     Kappa AccuracySD    KappaSD
#     FALSE       0      1 0.8090278 0.5298096 0.03767801 0.09134827
#      TRUE       0      1 0.8142361 0.5477833 0.02868526 0.06867108

# Continue using unscaled data.

grid31 = expand.grid(laplace = 0, usekernel = F, adjust = 1)
nb31 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC3, tuneGrid = grid31)
nb31$results 

grid32 = expand.grid(laplace = 0, usekernel = T, adjust = 1)
nb32 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC3, tuneGrid = grid32)
nb32$results 

grid33 = expand.grid(laplace = 1, usekernel = F, adjust = 1)
nb33 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC3, tuneGrid = grid33)
nb33$results 

grid34 = expand.grid(laplace = 1, usekernel = T, adjust = 1)
nb34 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC3, tuneGrid = grid34)
nb34$results 

grid35 = expand.grid(laplace = 0, usekernel = F, adjust = 0.5)
nb35 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC3, preProcess = c("center", "scale"), tuneGrid = grid35)
nb35$results

grid36 = expand.grid(laplace = 0, usekernel = T, adjust = 0.5)
nb36 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC3, preProcess = c("center", "scale"), tuneGrid = grid36)
nb36$results

#      laplace usekernel adjust  Accuracy     Kappa AccuracySD   KappaSD
# nb31       0     FALSE      1 0.8042929 0.5191596 0.03433119 0.08888323
# nb32       0      TRUE      1 0.8244391 0.5716117 0.05151209 0.1427207
# nb33       1     FALSE      1 0.8038194 0.5141421  0.0466817 0.1182188
# nb34       1      TRUE      1 0.8091346 0.5297391 0.03340222 0.106282
# nb35       0     FALSE    0.5 0.8091856 0.5325378 0.02469849 0.06608663
# nb36       0      TRUE    0.5 0.752484 0.3971834 0.04153602 0.1039844

# BEST nb32 - 82.44%: laplace = 0, usekernel = T, adjust = 1

# 5-fold # --------

tC5 = trainControl(method = "cv", number = 5, savePredictions = T)

grid51 = expand.grid(laplace = 0, usekernel = F, adjust = 1)
nb51 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC5, tuneGrid = grid51)
nb51$results 

grid52 = expand.grid(laplace = 0, usekernel = T, adjust = 1)
nb52 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC5, tuneGrid = grid52)
nb52$results 

grid53 = expand.grid(laplace = 1, usekernel = F, adjust = 1)
nb53 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC5, tuneGrid = grid53)
nb53$results 

grid54 = expand.grid(laplace = 1, usekernel = T, adjust = 1)
nb54 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC5, tuneGrid = grid54)
nb54$results 

grid55 = expand.grid(laplace = 0, usekernel = F, adjust = 0.5)
nb55 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC5, tuneGrid = grid55)
nb55$results

grid56 = expand.grid(laplace = 0, usekernel = T, adjust = 0.5)
nb56 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC5, tuneGrid = grid56)
nb56$results

#      laplace usekernel adjust  Accuracy     Kappa AccuracySD    KappaSD
# nb51       0     FALSE      1 0.8040486 0.5174282 0.01519043 0.05206969
# nb52       0      TRUE      1 0.8191633 0.5577122 0.06739536 0.1716906
# nb53       1     FALSE      1 0.8030702 0.5115635 0.0661281  0.1830875
# nb54       1      TRUE      1 0.814386  0.5479669 0.04265037 0.1046173
# nb55       0     FALSE    0.5 0.8043185 0.5173452 0.03784697 0.1061803
# nb56       0      TRUE    0.5 0.7946626 0.5057634 0.06233273 0.150593

# BEST nb52 - 81.91%: laplace = 0, usekernel = T, adjust = 1

# 4-fold # -----------

tC4 = trainControl(method = "cv", number = 4, savePredictions = T)
grid42 = expand.grid(laplace = 0, usekernel = T, adjust = 1)
nb42 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC4, tuneGrid = grid42)
nb42$results 

# laplace usekernel adjust  Accuracy     Kappa AccuracySD   KappaSD
#       0      TRUE      1 0.7991071 0.5075005 0.02939214 0.0896595

# 2-fold # ---------

tC2 = trainControl(method = "cv", number = 2, savePredictions = T)
grid22 = expand.grid(laplace = 0, usekernel = T, adjust = 1)
nb22 = train(class ~ ., data = data[,c(1,4)], method = "naive_bayes", trControl = tC2, tuneGrid = grid22)
nb22$results 

# laplace usekernel adjust  Accuracy     Kappa AccuracySD   KappaSD
#       0      TRUE      1 0.8042092 0.5196233 0.01172498 0.02559356

# Using the naive_bayes() function does not work for trying to classify m1 only as the function
# needs a matrix, not a vector.

# # # LDA # # # -----------------------------------------------------------------------------------------

# # LOOCV # # -------------------------------------------------------------------------------------------

lda = train(class ~ ., data = data[,c(1,4)], method = "lda", trControl = trainControl(method = "LOOCV"))
lda$results # 0.7525773

ldaS = train(class ~ ., data = data[,c(1,4)], method = "lda", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
ldaS$results # 0.7525773

# Use unscaled data

# # k-fold CV # # ----------------------------------------------------------------------------------------

# 3-fold # ----------

tC3 = trainControl(method = "cv", number = 3, savePredictions = T)

model3_lda = train(class~., method = "lda", data = data[,c(1,4)], trControl = tC3)
model3_lda # 0.7476326

model3_ldaS = train(class~., method = "lda", data = data[,c(1,4)], trControl = tC3, preProcess=c("center", "scale"))
model3_ldaS # 0.7629006

# 5-fold # ---------
tC5 = trainControl(method = "cv", number = 5, savePredictions = T)

model5_lda = train(class~., method = "lda", data = data[,c(1,4)], trControl = tC5)
model5_lda # 0.7624831

model5_ldaS = train(class~., method = "lda", data = data[,c(1,4)], trControl = tC5, preProcess=c("center", "scale"))
model5_ldaS # 0.7522942

# Worse than naive bayes.

# # # ANN # # # -------------------------------------------------------------------------------------------

# # LOOCV # # ---------------------------------------------------------------------------------------------

library(caret)

nn = train(class ~ ., data = data[,c(1,4)], method = "nnet", trControl = trainControl(method = "LOOCV"))
nn$results

# size decay  Accuracy     Kappa
#    1 0e+00 0.6958763 0.1582586
#    1 1e-04 0.6804124 0.1193440
#    1 1e-01 0.7422680 0.3418374
#    3 0e+00 0.8144330 0.5374222
#    3 1e-04 0.8041237 0.5038363
#    3 1e-01 0.8195876 0.5723643
#    5 0e+00 0.7938144 0.4860246
#    5 1e-04 0.8092784 0.5374404
#    5 1e-01 0.8247423 0.5830067 < < <

nnS = train(class ~ ., data = data[,c(1,4)], method = "nnet", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"))
nnS$results

# size decay  Accuracy     Kappa
#    1 0e+00 0.8195876 0.5723643
#    1 1e-04 0.8247423 0.5830067
#    1 1e-01 0.8298969 0.5906126
#    3 0e+00 0.8144330 0.5618020
#    3 1e-04 0.8247423 0.5830067
#    3 1e-01 0.8298969 0.5906126
#    5 0e+00 0.8041237 0.5339487
#    5 1e-04 0.8041237 0.5303860
#    5 1e-01 0.8298969 0.5906126 < < <

# Scaled data wins

nngrid = expand.grid(size = 5, decay = 0.1)
nnmodelS = train(class ~ ., data = data[,c(1,4)], method = "nnet", trControl = trainControl(method = "LOOCV"), preProcess = c("center", "scale"), tuneGrid = nngrid)
nnmodelS$results
# size decay  Accuracy     Kappa
#    5   0.1 0.8298969 0.5906126

# Increasing iterations leads to same accuracy.

# # k-fold CV # # -----------------------------------------------------------------------------------------

# 3-fold # -------

numFolds3 = trainControl(method = "cv", number = 3, savePredictions = T)

nn3 = train(class ~ ., data = data[,c(1,4)], method = "nnet", trControl = numFolds3)
nn3$results
# size decay  Accuracy     Kappa  AccuracySD    KappaSD
#    1 0e+00 0.6963384 0.1475954 0.073573716 0.25564266
#    1 1e-04 0.6546717 0.0000000 0.002733666 0.00000000
#    1 1e-01 0.6546717 0.0000000 0.002733666 0.00000000
#    3 0e+00 0.7588384 0.3727987 0.092944646 0.32383651
#    3 1e-04 0.7588384 0.3727987 0.092944646 0.32383651
#    3 1e-01 0.8244949 0.5851200 0.020775862 0.05149517
#    5 0e+00 0.8244949 0.5851200 0.020775862 0.05149517
#    5 1e-04 0.8297033 0.5924028 0.018044267 0.05287231 < < <
#    5 1e-01 0.8244949 0.5851200 0.020775862 0.05149517

nnS3 = train(class ~ ., data = data[,c(1,4)], method = "nnet", trControl = numFolds3, preProcess = c("center", "scale"))
nnS3$results
# size decay  Accuracy     Kappa  AccuracySD    KappaSD
#    1 0e+00 0.8298611 0.5957468 0.003007033 0.02282764
#    1 1e-04 0.8298611 0.5957468 0.003007033 0.02282764
#    1 1e-01 0.8244949 0.5745945 0.020775862 0.06103210
#    3 0e+00 0.8143939 0.5531614 0.015965639 0.05567284
#    3 1e-04 0.8039773 0.5445426 0.033584064 0.07058857
#    3 1e-01 0.8244949 0.5745945 0.020775862 0.06103210
#    5 0e+00 0.7987689 0.5173339 0.018538610 0.05726982
#    5 1e-04 0.8142361 0.5580189 0.018291066 0.04202397
#    5 1e-01 0.8244949 0.5745945 0.020775862 0.06103210 < < <

# Unscaled data

nngrid1 = expand.grid(size = 5, decay = 0.1)
nnmodel1 = train(class ~ ., data = data[,c(1,4)], method = "nnet", trControl = numFolds3, tuneGrid = nngrid1)
nnmodel1$results
# size decay  Accuracy     Kappa AccuracySD    KappaSD
#    5   0.1 0.8295455 0.5981135 0.02952359 0.07483661

# 5-fold # ----- 

numFolds5 = trainControl(method = "cv", number = 5, savePredictions = T)

nn5 = train(class ~ ., data = data[,c(1,4)], method = "nnet", trControl = numFolds5)
nn5$results
# size decay  Accuracy     Kappa AccuracySD    KappaSD
#    1 0e+00 0.7376518 0.2637874 0.10990824 0.36221463
#    1 1e-04 0.6546559 0.0000000 0.01294773 0.00000000
#    1 1e-01 0.7161943 0.2195349 0.09547801 0.31850109
#    3 0e+00 0.8299595 0.5907647 0.04627916 0.12122418 
#    3 1e-04 0.8299595 0.5904130 0.03399363 0.09664841
#    3 1e-01 0.8197031 0.5716699 0.04746041 0.11730324
#    5 0e+00 0.7838057 0.4512298 0.08924960 0.27340654
#    5 1e-04 0.8299595 0.5981053 0.03399363 0.08887104 < < <
#    5 1e-01 0.8248313 0.5809688 0.05524334 0.13655545

nnS5 = train(class ~ ., data = data[,c(1,4)], method = "nnet", trControl = numFolds5, preProcess = c("center", "scale"))
nnS5$results
# size decay  Accuracy     Kappa AccuracySD    KappaSD
#    1 0e+00 0.8299055 0.5916343 0.03426599 0.09166863
#    1 1e-04 0.8299055 0.5916343 0.03426599 0.09166863
#    1 1e-01 0.8350337 0.6024578 0.04292661 0.10991041 < < <
#    3 0e+00 0.8195142 0.5635662 0.02605980 0.06999322
#    3 1e-04 0.8092578 0.5520320 0.03883565 0.09693771
#    3 1e-01 0.8350337 0.6024578 0.04292661 0.10991041
#    5 0e+00 0.8143860 0.5478821 0.02165805 0.05462369
#    5 1e-04 0.8092578 0.5365772 0.02284763 0.05470626
#    5 1e-01 0.8299055 0.5891481 0.04279744 0.10773714

# Scaled data

nngrid2 = expand.grid(size = 1, decay = 0.1)
nnmodel2 = train(class ~ ., data = data[,c(1,4)], method = "nnet", trControl = numFolds5, tuneGrid = nngrid2, preProcess = c("center", "scale"))
nnmodel2$results
# size decay Accuracy     Kappa AccuracySD    KappaSD
#    1   0.1     0.83 0.5899451  0.0211838 0.05769585