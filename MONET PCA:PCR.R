# install.load packages
library(readxl)
library(tidyverse)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(factoextra)

# load dataset, scaled dataset they first provided
EEMO_Analysis <- read_excel("~/Downloads/180 EEMO Campaign v1 and v2 - Full Data and Tik-Tok Behavior - for MV Reg Analyses.xlsx", sheet = "All 203 tiktok formulated",  skip = 2)

# split two versions
eemo.v1 <-
  EEMO_Analysis %>%
  filter(`Eemo Version` == "Eemo 1.0")

eemo.v2 <-
  EEMO_Analysis %>%
  filter(`Eemo Version` == "Eemo 2.0")

# remove tiktok metrics + scores
eemo.v1 <- eemo.v1[,-c(1:15, 18:19, 23:59)]
eemo.v2 <- eemo.v2[,-c(1:15, 18:19, 23:59)]

# Principal Component Analysis: version 1
pc.eemo.v1 <- princomp(eemo.v1, cor = TRUE, scores = TRUE)
pc.eemo.v1

# information output
names(pc.eemo.v1)

# quick summary
summary(pc.eemo.v1)

# eigenvalues (number of how much variance in that direction)/ eigenvectors (the direction)
# eigenvector with the highest eigenvalue = first principal component
eigenvectors.v1 <- pc.eemo.v1$loadings
eigenvalues.v1 <- (pc.eemo.v1$sdev)^2

eigenvectors.v1
eigenvalues.v1

# plotting PCA
# calling ggbiplot on pc.eemo
## scaledReactionValence
## scaled_lift
## scaledreactionIntensity
ggbiplot(pc.eemo.v1)

# screeplot
screeplot(pc.eemo.v1, type = "l", main = "Screeplot")
abline(1, 0, col = "red", lty = 2) # 2 components

############################################################

# Principal Component Analysis: version 2
pc.eemo.v2 <- princomp(eemo.v2, cor = TRUE, scores = TRUE)
pc.eemo.v2

# information output
names(pc.eemo.v2)

# quick summary
summary(pc.eemo.v2)

# eigenvalues (number of how much variance in that direction)/ eigenvectors (the direction)
# eigenvector with the highest eigenvalue = first principal component
eigenvectors.v2 <- pc.eemo.v2$loadings
eigenvalues.v2 <- (pc.eemo.v2$sdev)^2

eigenvectors.v2
eigenvalues.v2

# plotting PCA
## reactionvalence
## reactionIntensity
## scaled Lift
## evalance?? 
ggbiplot(pc.eemo.v2)

# screeplot
screeplot(pc.eemo.v2, type = "l", main = "Screeplot")
abline(1, 0, col = "red", lty = 2) # 3 components


