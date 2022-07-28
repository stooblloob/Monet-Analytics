# load packages
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggbiplot)
library(factoextra)
library(devtools)


# load raw dataset
raw_data <- read_excel("UCR Sheet with Raw Data - 180 EEMO Campaign v1 and v2 - Full Data and Tik-Tok Behavior.xlsx", 
                       sheet = "All 180 tiktok formulated", 
                       col_types = c("numeric", "text", "text", 
                                     "text", "numeric", "numeric", "numeric", 
                                     "text", "date", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "date", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "text", "numeric", "numeric", "text", 
                                     "numeric", "numeric", "text", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric"), 
                       skip = 2)
eemo_transformed <- raw_data

# keep only monet scores
eemo_transformed <- eemo_transformed[ , -c(1:3, 5:8, 10:14, 28:80)]
eemo_transformed <- eemo_transformed[ , -c(5, 7,10,12:15)]

# add new variables to dataset
## normalized views/follower
eemo_transformed$views_per_follower <- log(raw_data$Views/raw_data$Followers)

## create variable for number of days the video has been on the platform
eemo_transformed$num_days <- as.numeric(difftime(raw_data$`Finish Date`, raw_data$`Upload Date`, units = "days"))

# rescale data from 0-100
eemo_transformed$`raw valence` <- rescale(eemo_raw$`raw valence`, to = c(0, 100))  
eemo_transformed$`raw arousal`<- rescale(eemo_raw$`raw arousal`, to = c(0, 100))  
eemo_transformed$`raw reaction valence (Monet)`<- rescale(eemo_raw$`raw reaction valence (Monet)`, to = c(0, 100))  
eemo_transformed$`raw reaction intensity` <- rescale(eemo_raw$`raw reaction intensity`, to = c(0,100))
eemo_transformed$EmotionEnd <- rescale(eemo_raw$EmotionEnd, to = c(0,100))

# omit NAs
eemo_transformed <- na.omit(eemo_transformed)

## scatterplots of TRANSFORMED data
ggplot(data = eemo_transformed, mapping = aes(x = `raw valence`, y = views_per_follower, color = `Eemo Version`)) +
  geom_point() + 
  geom_smooth(se = FALSE)+
  ggtitle("Transformed Valence vs. Views per Follower")

ggplot(data = eemo_transformed, mapping = aes(x = `raw arousal`, y = views_per_follower, color = `Eemo Version`)) +
  geom_point() + 
  geom_smooth(se = FALSE) + 
  ggtitle("Transformed Arousal vs. Views per Follower")

ggplot(data = eemo_transformed, mapping = aes(x = EmotionEnd, y = views_per_follower, color = `Eemo Version`)) +
  geom_point() + 
  geom_smooth(se = FALSE)+
  ggtitle("Transformed Emotion End vs. Views per Follower")  

ggplot(data = eemo_transformed, mapping = aes(x = `raw reaction valence (Monet)`, y = views_per_follower, color = `Eemo Version`)) +
  geom_point() + 
  geom_smooth(se = FALSE)+
  ggtitle("Transformed Reaction Valence vs. Views per Follower") 

ggplot(data = eemo_transformed, mapping = aes(x = `raw reaction intensity`, y = views_per_follower, color = `Eemo Version`)) +
  geom_point() + 
  geom_smooth(se = FALSE)+
  ggtitle("Transformed Reaction Intensity vs. Views per Follower") 

ggplot(data = eemo_transformed, mapping = aes(x = num_days, y = views_per_follower, color = `Eemo Version`)) +
  geom_point() + 
  geom_smooth(se = FALSE)+
  ggtitle("Number of Days vs. Views per Follower")   

######################################################################################################
# regression with views_per_follower
model1 <- glm(views_per_follower ~ `raw valence` +
                                   `raw arousal` +
                                    EmotionEnd +
                                   `raw reaction valence (Monet)` +
                                   `raw reaction intensity`,
              data = eemo_transformed)
summary(model1) #aic = 748.46
step(model1)
model1.5 <- glm(views_per_follower ~ `raw valence` +
                  `raw arousal` +
                  `raw reaction intensity`,
                data = eemo_transformed )

summary(model1.5) #aic = 745.96
with(summary(model1.5), 1 - deviance/null.deviance) # (0.0498358)

# lm() function
model2 <- lm(views_per_follower ~ `raw valence` +
               `raw arousal` +
               EmotionEnd +
               `raw reaction valence (Monet)` +
               `raw reaction intensity`,
             data = eemo_transformed)

summary(model2) # p-value = 0.06264

selectedModel2 <- step(model2)

summary(selectedModel2) #p-values = 0.02833, adj r2 = 0.03373

model2.5 <- lm(views_per_follower ~ `raw valence` +
                 `raw arousal` +
                 `raw reaction intensity`,
               data = eemo_raw )

all_vifs <- car::vif(selectedModel2)
print(all_vifs)
######################################################################################################
# split versions
eemo_transformed_v1 <-
  eemo_transformed %>%
  filter(`Eemo Version` == "Eemo 1.0")

eemo_transformed_v2 <-
  eemo_transformed %>%
  filter(`Eemo Version` == "Eemo 2.0")

##################################################################
################# PCA ################# 
##################################################################

# VERSION 1
eemo_transformed_v1 <- eemo_transformed_v1[,-c(1:3)]

pc.eemo.v1 <- princomp(eemo_transformed_v1, cor = TRUE, scores = TRUE)
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
## raw arousal
## raw valence
## raw reaction Intensity
## Emotion end

ggbiplot(pc.eemo.v1)

# screeplot
screeplot(pc.eemo.v1, type = "l", main = "Screeplot")
abline(1, 0, col = "red", lty = 2) # 3 components

###########################################################
eemo_transformed_v2 <- eemo_transformed_v2[,-c(1:3)]

# Principal Component Analysis: version 2
pc.eemo.v2 <- princomp(eemo_transformed_v2, cor = TRUE, scores = TRUE)
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
## raw arousal
## raw valence
## raw reaction intensity
ggbiplot(pc.eemo.v2)

# screeplot
screeplot(pc.eemo.v2, type = "l", main = "Screeplot")
abline(1, 0, col = "red", lty = 2) # 3 components

######################################################################################################