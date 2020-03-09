#<!--Improt libraries-->


options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

#<!--Dimensions and properties of database-->

#How many samples are in the dataset?

   dim(brca$x)[1]

#How many predictors are in the matrix?

  dim(brca$x)[2]

#What proportion of the samples are malignant?

  mean(brca$y == "M")

#Which column number has the highest mean?

  which.max(colMeans(brca$x))

#Which column number has the lowest standard deviation?

  which.min(colSds(brca$x))

 #<!--Scaling the matrix--> 
 
 #Use sweep two times to scale each column: subtract the column mean, then divide by the column standard deviation.

#After scaling, what is the standard deviation of the first column?

x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")

sd(x_scaled[,1])

#median value of the first column:
median(x_scaled[,1])


 #<!--Distance :Calculate the distance between all samples using the scaled matrix.--> 
 # The average distance between the first sample, which is benign, and other benign samples?
 d_samples <- dist(x_scaled)
dist_BtoB <- as.matrix(d_samples)[1, brca$y == "B"]
mean(dist_BtoB[2:length(dist_BtoB)])
# the average distance between the first sample and malignant samples?
dist_BtoM <- as.matrix(d_samples)[1, brca$y == "M"]
mean(dist_BtoM)

# <---heatmap of the relationship between features using the scaled matrix-->

d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

#<--Hierarchical clustering--->

h <- hclust(d_features)
groups <- cutree(h, k = 5)
split(names(groups), groups)

#<--proportion of variance--->
 #proportion of variance is explained by the first principal component:

 pca <- prcomp(x_scaled)
summary(pca)    # see PC1 Cumulative Proportion

# the principal components are required to explain at least 90% of the variance:
pca <- prcomp(x_scaled)
summary(pca)     # first value of Cumulative Proportion that exceeds 0.9: PC7


#<----plotting PCs--->

#Plot the first two principal components with color representing tumor type (benign/malignant).

data.frame(pca$x[,1:2], type = brca$y) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()  # Malignant tumors tend to have larger values of PC1 than benign tumors.




  #<---PCA: PC boxplot--->


 # boxplot of the first 10 PCs grouped by tumor type:
  data.frame(type = brca$y, pca$x[,1:10]) %>%
    gather(key = "PC", value = "value", -type) %>%
    ggplot(aes(PC, value, fill = type)) +
    geom_boxplot()  # PC1



