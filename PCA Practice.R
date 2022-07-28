mtcars
head(mtcars)
str(mtcars)

# computing the principal components
## exclude `vs` and `am` because they are categorical variables
mtcars.pca <- princomp(mtcars[, c(1:7, 10, 11)], cor =TRUE)

## 9 principal components, PC1-9. Each of these explains a percentage of total variation in the dataset
## PC1 explains 63% of total variance
## PC2 explains 23% of the variance
summary(mtcars.pca)  

## looking at PCA object
str(mtcars.pca)

# eigenvalues/ eigenvectors
eigenvectors <- mtcars.pca$loadings
eigenvalues <- (mtcars.pca$sdev)^2

eigenvectors
eigenvalues

# plotting PCA
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

## call ggbiplot on PCA
ggbiplot(mtcars.pca)

## give rownames as labels
ggbiplot(mtcars.pca, labels=rownames(mtcars))

## looking at the origin of each cars
mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))

ggbiplot(mtcars.pca,ellipse=TRUE,  labels=rownames(mtcars), groups=mtcars.country)

## looking at PC3 and PC4
## not informative since it explains very small percentages of the total variation
ggbiplot(mtcars.pca,ellipse=TRUE,choices=c(3,4),   labels=rownames(mtcars), groups=mtcars.country)

## looking at original ggbiplot
## `cyl`, `disp`, `wt`, and `mpg` are variables with the highest values
## if we build a classification model, these variables might be useful
ggbiplot(mtcars.pca)





