load(file = "biomedical-data/data/biomedical-data.RData")

pca = prcomp(data[,4:7])
summary(pca)

pcs = prcomp(data[,4:7], scale = T)
summary(pcs)

biplot(pca, cex = 0.8)
biplot(pcs, cex = 0.8)
# Practical 1A
# pca: m1, m4 dominate analysis
# pcs: all m's dominate analysis equally

# Not scaled, want variation, reduce cost, less time consuming

plot(pca)
plot(pcs)

# PC1 and PC2

plot(pca$x[,1], pca$x[,2], xlab="PC1 (88.48%)", ylab="PC2 (10.78%)", pch=20, col = data$class)
legend("bottomright", inset = 0.01, legend = unique(data$class), col = unique(data$class), pch = 20)

plot(pcs$x[,1], pcs$x[,2], xlab="PC1 (57.84%)", ylab="PC2 (24.14%)", pch=20, col = data$class)
legend("topright", inset = 0.01, legend = unique(data$class), col = unique(data$class), pch = 20)

# Seperation is not good leads to potential problems for classification

# PC3 and PC4

plot(pca$x[,3], pca$x[,4], xlab="PC3 (0.52%)", ylab="PC4 (0.23%)", pch=20, col = data$class)
legend("bottomright", inset = 0.01, legend = unique(data$class), col = unique(data$class), pch = 20)

plot(pcs$x[,3], pcs$x[,4], xlab="PC3 (11.31%)", ylab="PC4 (0.67%)", pch=20, col = data$class)
legend("bottomleft", inset = 0.01, legend = unique(data$class), col = unique(data$class), pch = 20)

# Worse seperation





