load(file = "biomedical-data/data/biomedical-data.RData")

# Comparing m1, m2, m3 and m4
pca = prcomp(data[,4:7])
summary(pca)
pcs = prcomp(data[,4:7], scale = T)
summary(pcs)

biplot(pca)
biplot(pcs)

plot(pca$x[,1], pca$x[,2], xlab="PC1 (88.48%)", ylab="PC2 (10.78%)", pch=20, col = data$class)
legend("bottomright", inset = 0.01, legend = unique(data$class), col = unique(data$class), pch = 20)
which(pca$x[,1] > 800)
# 167 192

plot(pcs$x[,1], pcs$x[,2], xlab="PC1 (57.84%)", ylab="PC2 (24.14%)", pch=20, col = data$class)
legend("topright", inset = 0.01, legend = unique(data$class), col = unique(data$class), pch = 20)
which(pcs$x[,1] > 4)
# 129 157 167 170 178 192

plot(pca$x[,3], pca$x[,4], xlab="PC3 (0.52%)", ylab="PC4 (0.23%)", pch=20, col = data$class)
legend("bottomright", inset = 0.01, legend = unique(data$class), col = unique(data$class), pch = 20)
which(pca$x[,3] > 40)
# 34

plot(pcs$x[,3], pcs$x[,4], xlab="PC3 (11.31%)", ylab="PC4 (6.70%)", pch=20, col = data$class)
legend("topright", inset = 0.01, legend = unique(data$class), col = unique(data$class), pch = 20)
which(pcs$x[,4] < -3)
# 167

# Poor seperation between groups, leading to potential porblems for classificvation.

# Removing 167 and 192.

newData = data[-c(167,192),]
newPca = prcomp(newData[,4:7])
biplot(newPca)
newPcs = prcomp(newData[,4:7], scale = T)
biplot(newPcs)

# Leave observations in the data. Use unscaled data. M1 used to classify. Time and cost. Do not
# change.

plot(pca)
summary(pca)
# Proportion of Variance   0.8848  0.1078  0.00518 0.00225
# Cumulative Proportion    0.8848  0.9926  0.99775 1.00000

plot(pcs)
summary(pcs)
# Proportion of Variance 0.5784 0.2414 0.1131 0.0670
# Cumulative Proportion  0.5784 0.8199 0.9330 1.0000