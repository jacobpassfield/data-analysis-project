load(file = "biomedical-data/data/biomedical-data.RData")
head(data)

# BIPLOT

pca = prcomp(data[,4:7])
summary(pca)
#                             PC1     PC2      PC3     PC4
# Proportion of Variance   0.8848  0.1078  0.00518 0.00225
# Cumulative Proportion    0.8848  0.9926  0.99775 1.00000

pca$rotation

biplot(pca)
# M1 dominates analysis. M4 also significant. Observation 167 and 190 look sus.

pca$x

plot(pca$x[,1], pca$x[,2], xlab="PC1 (88.48%)", ylab="PC2 (10.78%)", pch=20, col = data$class)
text(pca$x[,1], pca$x[,2], pos=2, cex = 0.65)
legend("bottomright", inset = 0.01, legend = unique(data$class), col = unique(data$class), pch = 20)
# 167 and 192 still sus.
# Not great seperation between groups.

plot(pca$x[,3], pca$x[,4], xlab="PC3 (0.52%)", ylab="PC4 (0.23%)", pch=20, col = data$class)
# Not good
plot(pca$x[,1], pca$x[,3], xlab="PC1 (88.48%)", ylab="PC3 (0.52%)", pch=20, col = data$class)
# Not good
plot(pca$x[,1], pca$x[,4], xlab="PC1 (88.48%)", ylab="PC4 (0.23%)", pch=20, col = data$class)
# Not good
plot(pca$x[,2], pca$x[,3], xlab="PC2 (10.78%)", ylab="PC3 (0.52%)", pch=20, col = data$class)
# Not good
plot(pca$x[,2], pca$x[,4], xlab="PC2 (10.78%)", ylab="PC4 (0.23%)", pch=20, col = data$class)
# Not good

data[167,]
#     class age       date   m1 m2   m3  m4
# 167     1  35 1979-04-15 1288 82 51.6 368
data[192,]
#     class age       date  m1 m2   m3  m4
# 192     1  35 1979-09-15 925 81 62.9 279

# Hesistant to remove these observations as m1 is used to classify and these observations are carriers.

summary(data[,4:7])

boxplot(data[,4:7]) # M1 has bare outliers. M4 has biggest range.

pcs = prcomp(data[,4:7], scale = T)
summary(pcs)
#                           PC1    PC2    PC3    PC4
# Proportion of Variance 0.5784 0.2414 0.1131 0.0670
# Cumulative Proportion  0.5784 0.8199 0.9330 1.0000

biplot(pcs)
# All variable dominate analysis. # Practical 1A.

plot(pcs$x[,1], pcs$x[,2], xlab="PC1 (57.84%)", ylab="PC2 (24.14%)", pch=20, col = data$class)
legend("topright", inset = 0.01, legend = unique(data$class), col = unique(data$class), pch = 20)

plot(pcs$x[,3], pcs$x[,4], xlab="PC3 (0.52%)", ylab="PC4 (0.23%)", pch=20, col = data$class)
# Not good
plot(pcs$x[,1], pcs$x[,3], xlab="PC1 (88.48%)", ylab="PC3 (0.52%)", pch=20, col = data$class)
# Not good
plot(pcs$x[,1], pcs$x[,4], xlab="PC1 (88.48%)", ylab="PC4 (0.23%)", pch=20, col = data$class)
# Not good
plot(pcs$x[,2], pcs$x[,3], xlab="PC2 (10.78%)", ylab="PC3 (0.52%)", pch=20, col = data$class)
# Not good
plot(pcs$x[,2], pcs$x[,4], xlab="PC2 (10.78%)", ylab="PC4 (0.23%)", pch=20, col = data$class)
# Not good

# DO NOT USE PCA
