# load packages
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggbiplot)
library(ggpubr)
library(factoextra)
library(devtools)
library(glmnet)

# load raw dataset
raw_data <- read_excel("~/Downloads/UCR Sheet with Raw Data - 180 EEMO Campaign v1 and v2 - Full Data and Tik-Tok Behavior.xlsx", 
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
eemo_raw <- raw_data

# keep only monet scores
eemo_raw <- raw_data[ , -c(1:3, 5:8, 10:14, 28:80)]
eemo_raw <- eemo_raw[ , -c(5, 7,10,12:15)]

# add new variables to dataset
## normalized views/follower
eemo_raw$views_per_follower <- log(raw_data$Views/raw_data$Followers)

## create variable for number of days the video has been on the platform
eemo_raw$num_days <- as.numeric(difftime(raw_data$`Finish Date`, raw_data$`Upload Date`, units = "days"))

# look at number of days
### create clusters of 1 month, 2 month, 3 month, 4 month, 5 month
frequency(eemo_raw$num_days)
summary(eemo_raw$num_days)
count(eemo_raw$num_days)
sd(eemo_raw$num_days)

## filter any observation with that month (version 1.0)
eemo_raw <-
eemo_raw %>%
  mutate(month = (num_days > 0 & num_days < 31) * 1 +
                 (num_days > 30 & num_days < 61) * 2 +
                 (num_days > 60 & num_days < 91) * 3 +
                 (num_days > 90 & num_days < 121) * 4 +
                 (num_days > 120 & num_days < 151) * 5)


## remove observations > 150 and NA
eemo_raw <- eemo_raw[eemo_raw$num_days < 151, ] 
eemo_raw <- na.omit(eemo_raw)
eemo_raw$month <- as.character(eemo_raw$month)

# split versions
eemo_raw_v1 <-
  eemo_raw %>%
  filter(`Eemo Version` == "Eemo 1.0")

eemo_raw_v2 <-
  eemo_raw %>%
  filter(`Eemo Version` == "Eemo 2.0")

# scatterplots of days #
###### less than 1 month
## raw valence
eemo_raw %>%
  filter(num_days < 32) %>%
  ggplot(mapping = aes(x = num_days, y =`raw valence`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Valence") +
  xlab("Number of Days") +
  ylab("Raw Valence") + 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## raw arousal
eemo_raw %>%
  filter(num_days < 32) %>%
  ggplot(mapping = aes(x = num_days, y =`raw arousal`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Arousal") +
  xlab("Number of Days") +
  ylab("Raw Arousal") + 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## Emotion End
eemo_raw %>%
  filter(num_days < 32) %>%
  ggplot(mapping = aes(x = num_days, y =EmotionEnd, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Emotion End") +
  xlab("Number of Days") +
  ylab("Emotion End")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## Raw Reaction Valence
eemo_raw %>%
  filter(num_days < 32) %>%
  ggplot(mapping = aes(x = num_days, y = `raw reaction valence (Monet)`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Reaction Valence") +
  xlab("Number of Days") +
  ylab("Raw Reaction Valence") + 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## Raw Reaction Intensity
eemo_raw %>%
  filter(num_days < 32) %>%
  ggplot(mapping = aes(x = num_days, y = `raw reaction intensity`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Reaction Intensity") +
  xlab("Number of Days") +
  ylab("Raw Reaction Intensity") + 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

###### 1 - 2 months
## raw valence
eemo_raw %>%
  filter(num_days > 31 & num_days < 62) %>%
  ggplot(mapping = aes(x = num_days, y =`raw valence`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Valence") +
  xlab("Number of Days") +
  ylab("Raw Valence") + 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## raw arousal
eemo_raw %>%
  filter(num_days > 31 & num_days < 62) %>%
  ggplot(mapping = aes(x = num_days, y =`raw arousal`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Arousal") +
  xlab("Number of Days") +
  ylab("Raw Arousal") + 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## Emotion End
eemo_raw %>%
  filter(num_days > 31 & num_days < 62) %>%
  ggplot(mapping = aes(x = num_days, y =EmotionEnd, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Emotion End") +
  xlab("Number of Days") +
  ylab("Emotion End") + 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## Raw Reaction Valence
eemo_raw %>%
  filter(num_days > 31 & num_days < 62) %>%
  ggplot(mapping = aes(x = num_days, y = `raw reaction valence (Monet)`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Reaction Valence") +
  xlab("Number of Days") +
  ylab("Raw Reaction Valence")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## Raw Reaction Intensity
eemo_raw %>%
  filter(num_days > 31 & num_days < 62) %>%
  ggplot(mapping = aes(x = num_days, y = `raw reaction intensity`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Reaction Intensity") +
  xlab("Number of Days") +
  ylab("Raw Reaction Intensity")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

###### 2 - 3 months
## raw valence
eemo_raw %>%
  filter(num_days > 61 & num_days < 92) %>%
  ggplot(mapping = aes(x = num_days, y =`raw valence`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Valence") +
  xlab("Number of Days") +
  ylab("Raw Valence")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## raw arousal
eemo_raw %>%
  filter(num_days > 61 & num_days < 92) %>%
  ggplot(mapping = aes(x = num_days, y =`raw arousal`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Arousal") +
  xlab("Number of Days") +
  ylab("Raw Arousal")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## Emotion End
eemo_raw %>%
  filter(num_days > 61 & num_days < 92) %>%
  ggplot(mapping = aes(x = num_days, y =EmotionEnd, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Emotion End") +
  xlab("Number of Days") +
  ylab("Emotion End")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## Raw Reaction Valence
eemo_raw %>%
  filter(num_days > 61 & num_days < 92) %>%
  ggplot(mapping = aes(x = num_days, y = `raw reaction valence (Monet)`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Reaction Valence") +
  xlab("Number of Days") +
  ylab("Raw Reaction Valence")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## Raw Reaction Intensity
eemo_raw %>%
  filter(num_days > 61 & num_days < 92) %>%
  ggplot(mapping = aes(x = num_days, y = `raw reaction intensity`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Reaction Intensity") +
  xlab("Number of Days") +
  ylab("Raw Reaction Intensity")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

###### 3 - 4 months
## raw valence
eemo_raw %>%
  filter(num_days > 91 & num_days < 122) %>%
  ggplot(mapping = aes(x = num_days, y =`raw valence`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Valence") +
  xlab("Number of Days") +
  ylab("Raw Valence")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## raw arousal
eemo_raw %>%
  filter(num_days > 91 & num_days < 122) %>%
  ggplot(mapping = aes(x = num_days, y =`raw arousal`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Arousal") +
  xlab("Number of Days") +
  ylab("Raw Arousal")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## Emotion End
eemo_raw %>%
  filter(num_days > 91 & num_days < 122) %>%
  ggplot(mapping = aes(x = num_days, y =EmotionEnd, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Emotion End") +
  xlab("Number of Days") +
  ylab("Emotion End")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## Raw Reaction Valence
eemo_raw %>%
  filter(num_days > 91 & num_days < 122) %>%
  ggplot(mapping = aes(x = num_days, y = `raw reaction valence (Monet)`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Reaction Valence") +
  xlab("Number of Days") +
  ylab("Raw Reaction Valence")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## Raw Reaction Intensity
eemo_raw %>%
  filter(num_days > 91 & num_days < 122) %>%
  ggplot(mapping = aes(x = num_days, y = `raw reaction intensity`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Reaction Intensity") +
  xlab("Number of Days") +
  ylab("Raw Reaction Intensity")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

###### 4 - 5 months
## raw valence
eemo_raw %>%
  filter(num_days > 121 & num_days < 152) %>%
  ggplot(mapping = aes(x = num_days, y =`raw valence`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Valence") +
  xlab("Number of Days") +
  ylab("Raw Valence")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## raw arousal
eemo_raw %>%
  filter(num_days > 121 & num_days < 152) %>%
  ggplot(mapping = aes(x = num_days, y =`raw arousal`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Arousal") +
  xlab("Number of Days") +
  ylab("Raw Arousal")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## Emotion End
eemo_raw %>%
  filter(num_days > 121 & num_days < 152) %>%
  ggplot(mapping = aes(x = num_days, y =EmotionEnd, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Emotion End") +
  xlab("Number of Days") +
  ylab("Emotion End")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## Raw Reaction Valence
eemo_raw %>%
  filter(num_days > 121 & num_days < 152) %>%
  ggplot(mapping = aes(x = num_days, y = `raw reaction valence (Monet)`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Reaction Valence") +
  xlab("Number of Days") +
  ylab("Raw Reaction Valence")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

## Raw Reaction Intensity
eemo_raw %>%
  filter(num_days > 121 & num_days < 152) %>%
  ggplot(mapping = aes(x = num_days, y = `raw reaction intensity`, color = `Eemo Version`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Raw Reaction Intensity") +
  xlab("Number of Days") +
  ylab("Raw Reaction Intensity")+ 
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

# analyze months #
## raw valence
ggplot(data = eemo_raw, mapping = aes(x = num_days, y = `raw valence`, group = month, color = `Eemo Version`)) +
  geom_point() +
  facet_wrap(~month) +
  ggtitle("Num_Days vs. Raw Valence")

## raw arousal
ggplot(data = eemo_raw, mapping = aes(x = num_days, y = `raw arousal`, group = month, color =  `Eemo Version`)) +
  geom_point() +
  facet_wrap(~month) +
  ggtitle("Num_Days vs. Raw Arousal")

## emotion end
ggplot(data = eemo_raw, mapping = aes(x = num_days, y = EmotionEnd, group = month, color =  `Eemo Version`)) +
  geom_point() +
  facet_wrap(~month) +
  ggtitle("Num_Days vs. Emotion End")

## raw reaction valence
ggplot(data = eemo_raw, mapping = aes(x = num_days, y = `raw reaction valence (Monet)`, group = month, color =  `Eemo Version`)) +
  geom_point() +
  facet_wrap(~month) +
  ggtitle("Num_Days vs. Raw Reaction Valence")

## raw reaction intensity
ggplot(data = eemo_raw, mapping = aes(x = num_days, y = `raw reaction intensity`, group = month, color =  `Eemo Version`)) +
  geom_point() +
  facet_wrap(~month) +
  ggtitle("Num_Days vs. Raw Reaction Intensity")

## scatterplots of RAW data
ggplot(data = eemo_raw, mapping = aes(x = `raw valence`, y = views_per_follower, color = `Eemo Version`)) +
  geom_point() + 
  geom_smooth(se = FALSE)+
  ggtitle("Raw Valence vs. Views per Follower")

ggplot(data = eemo_raw, mapping = aes(x = `raw arousal`, y = views_per_follower, color = `Eemo Version`)) +
  geom_point() + 
  geom_smooth(se = FALSE) + 
  ggtitle("Raw Arousal vs. Views per Follower")
  
ggplot(data = eemo_raw, mapping = aes(x = EmotionEnd, y = views_per_follower, color = `Eemo Version`)) +
  geom_point() + 
  geom_smooth(se = FALSE)+
  ggtitle("Emotion End vs. Views per Follower")  

ggplot(data = eemo_raw, mapping = aes(x = `raw reaction valence (Monet)`, y = views_per_follower, color = `Eemo Version`)) +
  geom_point() + 
  geom_smooth(se = FALSE)+
  ggtitle("Raw Reaction Valence vs. Views per Follower") 

ggplot(data = eemo_raw, mapping = aes(x = `raw reaction intensity`, y = views_per_follower, color = `Eemo Version`)) +
  geom_point() + 
  geom_smooth(se = FALSE)+
  ggtitle("Raw Reaction Intensity vs. Views per Follower") 

ggplot(data = eemo_raw, mapping = aes(x = num_days, y = views_per_follower, color = `Eemo Version`)) +
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
              data = eemo_raw_v1)
summary(model1) #aic = 748.46
step(model1)
model1.5 <- glm(views_per_follower ~ `raw valence` +
                                     `raw arousal` +
                                     `raw reaction intensity`,
                data = eemo_raw )

summary(model1.5) #aic = 745.96
with(summary(model1.5), 1 - deviance/null.deviance) # (0.0498358)

# lm() function
model2 <- lm(views_per_follower ~ `raw valence` +
                                  `raw arousal` +
                                   EmotionEnd +
                                  `raw reaction valence (Monet)` +
                                  `raw reaction intensity`,
              data = eemo_raw_v2)

summary(model2) 

selectedModel2 <- step(model2)

summary(selectedModel2) #p-values = .02777, adj r2 = .03378

model2.5 <- lm(views_per_follower ~ `raw reaction valence (Monet)`+
                 `raw valence`,
                data = eemo_raw_v2 )
summary(model2.5)$r.squared

all_vifs <- car::vif(selectedModel2)
print(all_vifs)

######################################################################################################
# creating a new column for ranking of views and shares by quantiles (25,50,75 as low, med, high respectively)
eemo_raw <-
  eemo_raw %>%
  mutate(rank_views = case_when(views_per_follower < quantile(views_per_follower, 0.25) ~ "Low",
                                views_per_follower >= quantile(views_per_follower, 0.25) & views_per_follower <= quantile(views_per_follower, 0.75) ~ "Medium",
                                views_per_follower > quantile(views_per_follower, 0.75) ~ "High"))

################################
####### lasso regression #######
################################
# low ranking & version 1
eemo_raw.low <- eemo_raw %>%
  filter(rank_views == "Low" & `Eemo Version` == "Eemo 1.0")

# define response variable
y <- eemo_raw.low$views_per_follower

# define matrix of predictor variables
x <- data.matrix( eemo_raw.low[, c(4:8)])

# perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #(0.1639951)

# produce plot of test MSE by lambda values
plot(cv_model)

# find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # the only variables that seem to be influential is "raw valence"

# use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# find r-squared
rsq <- 1 - sse/sst
rsq

# find rmse
sqrt(cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]) # (0.565268)

######################################################################
# low ranking & version 2
eemo_raw.low <- eemo_raw %>%
  filter(rank_views == "Low" & `Eemo Version` == "Eemo 2.0")

# define response variable
y <- eemo_raw.low$views_per_follower

# define matrix of predictor variables
x <- data.matrix( eemo_raw.low[, c(4:8)])

# perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #(0.2610288)

# produce plot of test MSE by lambda values
plot(cv_model)

# find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # the only variable that seem to be influential "raw reaction intensity"

# use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# find r-squared
rsq <- 1 - sse/sst
rsq #(0)

# find rmse
sqrt(cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]) # (0.5474947 )

######################################################################
# medium ranking & version 1
eemo_raw.med <- eemo_raw %>%
  filter(rank_views == "Medium" & `Eemo Version` == "Eemo 1.0")

# define response variable
y <- eemo_raw.med$views_per_follower

# define matrix of predictor variables
x <- data.matrix( eemo_raw.med[, c(4:8)])

# perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #(0.1301968)

# produce plot of test MSE by lambda values
plot(cv_model)

# find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # the only variables that seem to be influential is "raw arousal"

# use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# find r-squared
rsq <- 1 - sse/sst
rsq #(0)

# find rmse
sqrt(cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]) # (0.5853416)

######################################################################
# medium ranking & version 2
eemo_raw.med <- eemo_raw %>%
  filter(rank_views == "Medium" & `Eemo Version` == "Eemo 2.0")

# define response variable
y <- eemo_raw.med$views_per_follower

# define matrix of predictor variables
x <- data.matrix( eemo_raw.med[, c(4:8)])

# perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #(0.236534)

# produce plot of test MSE by lambda values
plot(cv_model)

# find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # the only variables that seem to be influential is "emotion end"

# use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# find r-squared
rsq <- 1 - sse/sst
rsq #(2.220446e-16)

# find rmse
sqrt(cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]) # (0.568769 )

######################################################################
# high ranking & version 1
eemo_raw.high <- eemo_raw %>%
  filter(rank_views == "High" & `Eemo Version` == "Eemo 1.0")

# define response variable
y <- eemo_raw.high$views_per_follower

# define matrix of predictor variables
x <- data.matrix( eemo_raw.high[, c(4:8)])

# perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #(0.3241974)

# produce plot of test MSE by lambda values
plot(cv_model)

# find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # the only variables that seem to be influential is "raw valance"

# use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# find r-squared
rsq <- 1 - sse/sst
rsq #(0)

# find rmse
sqrt(cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]) # (1.145048)

######################################################################
# high ranking & version 2
eemo_raw.high <- eemo_raw %>%
  filter(rank_views == "High" & `Eemo Version` == "Eemo 2.0")

# define response variable
y <- eemo_raw.high$views_per_follower

# define matrix of predictor variables
x <- data.matrix( eemo_raw.high[, c(4:8)])

# perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #(0.003991929)

# produce plot of test MSE by lambda values
plot(cv_model)

# find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # the only variables that seem to be influential are all of them

# use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# find r-squared
rsq <- 1 - sse/sst
rsq #(0.9722353)

# find rmse
sqrt(cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]) # (1.559926)

##################################################################
################# PCA ################# 
##################################################################
# separating ranks of views_per_follower by percentiles
eemo.rank.views.v1 <-
  eemo_raw_v1 %>%
  mutate(rank_views = case_when(views_per_follower < quantile(views_per_follower, 0.25) ~ "Low",
                                views_per_follower >= quantile(views_per_follower, 0.25) & views_per_follower <= quantile(views_per_follower, 0.75) ~ "Medium",
                                views_per_follower > quantile(views_per_follower, 0.75) ~ "High"))
eemo.rank.views.v1 <- eemo.rank.views.v1[, -c(1:11)]

# VERSION 1
eemo_raw_v1 <- eemo_raw_v1[,-c(1:3, 9:11)]

pc.eemo.v1 <- princomp(eemo_raw_v1, cor = TRUE, scores = TRUE)
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
eemo.rank.views.v1<-
  eemo.rank.views.v1 %>%
  deframe()

ggbiplot(pc.eemo.v1,  groups = eemo.rank.views.v1, ellipse = TRUE)
ggbiplot(pc.eemo.v1,choices=c(1,3), groups = eemo.rank.views.v1, ellipse = TRUE)
ggbiplot(pc.eemo.v1,choices=c(2,3), groups = eemo.rank.views.v1, ellipse = TRUE)
ggbiplot(pc.eemo.v1,choices=c(3,4), groups = eemo.rank.views.v1, ellipse = TRUE)
ggbiplot(pc.eemo.v1,choices=c(4, 5), groups = eemo.rank.views.v1, ellipse = TRUE)

# screeplot
screeplot(pc.eemo.v1, type = "l", main = "Screeplot")
abline(1, 0, col = "red", lty = 2) # 3 components

###########################################################
eemo_raw_v2 <- eemo_raw_v2[,-c(1:3, 9:11)]

# Principal Component Analysis: version 2
pc.eemo.v2 <- princomp(eemo_raw_v2, cor = TRUE, scores = TRUE)
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
## num_days/ month
ggbiplot(pc.eemo.v2)

# screeplot
screeplot(pc.eemo.v2, type = "l", main = "Screeplot")
abline(1, 0, col = "red", lty = 2) # 3 components

######################################################################################################



