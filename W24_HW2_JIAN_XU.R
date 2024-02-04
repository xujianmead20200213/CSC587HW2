library(ggplot2)
library(tidyverse)
setwd("C:\\Users\\DELL\\Desktop\\CSC587\\datamining-main\\Rscripts")
data.metabolite <- file.path('data', 'metabolite.csv')


#2.
#Create function for Manhattan and Euclidean distances
Manhattan_Distance <- function(vector1, vector2) {
  sum(abs(vector1 - vector2))
}
Euclidean_Distance <- function(vector1, vector2) {
  sqrt(sum((vector1 - vector2)^2))
}
#Create 2 vectors
vector_s1 <- c(9, 7, 3, 10, 25)
vector_s2 <- c(4, 10, 30, 22, 100)
#Get the result use the distance function with the vectors
M_Result <- Manhattan_Distance(vector_s1, vector_s2)
E_Result <- Euclidean_Distance(vector_s1, vector_s2)
#Print the result
cat("Manhattan Distances: ", M_Result, "\n")
cat("Euclidean Distances: ", E_Result, "\n")

#4.
cor(mtcars$mpg,mtcars$wt)
scatter.smooth(x=mtcars$wt,y=mtcars$mpg,evaluation = 0)

#5.
metabolite = read.delim(data.metabolite, sep=',', header = T)
#Get the list of the columns which will not be removed.
columns_remain <- NULL
for (i in 1:ncol(metabolite)) {
  missing_proportion <- sum(is.na(metabolite[,i])) / length(metabolite[,i])
  if(missing_proportion>0.75){
    drop_column_name <- colnames(metabolite)[i]
    print(drop_column_name)
  }else{
    remain_column_name <- colnames(metabolite)[i]
    columns_remain <- c(columns_remain,remain_column_name)
  }
}
metabolite <- subset(metabolite,select=columns_remain)
#Fill the missing values with the median value in their own columns
for (i in 1:ncol(metabolite)) {
  median_value <- median(metabolite[,i],na.rm = TRUE)
  for (j in 1:nrow(metabolite)) {
    if(is.na(metabolite[j,i])){
      metabolite[j,i] <- median_value
    }
  }
}
print(metabolite)


#6.
# Subset Label column
metabolite_pca <- subset(metabolite,select=-Label)
pca_apply <- prcomp(metabolite_pca)
print(pca_apply)
pc_scores <- as.data.frame(pca_apply$x[, 1:2])
# Add back the Label column
pc_scores$Label <- metabolite$Label
print(pc_scores)
ggplot(pc_scores, aes(x = PC1, y = PC2, color = Label)) +
  geom_point() +
  labs(title = "PCA Scatter Plot of Metabolites Data",
       x = "Principal Component 1",
       y = "Principal Component 2")
