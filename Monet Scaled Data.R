# load packages
library(readxl)
library(tidyverse)
library(ggplot2)
#install.packages("glmnet")
library(glmnet) 

# load dataset
scaled_data <- read_excel("~/Downloads/UCR Sheet with Raw Data - 180 EEMO Campaign v1 and v2 - Full Data and Tik-Tok Behavior.xlsx", 
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

# removing raw data columns
eemo_scaled <- scaled_data[ , -c(1:3, 5:16, 18, 20:21, 23, 28:80)]

# add new variables to dataset
## normalized views/follower
eemo_scaled$views_per_follower <- log(scaled_data$Views/scaled_data$Followers)

## create variable for number of days the video has been on the platform
eemo_scaled$num_days <- as.numeric(difftime(scaled_data$`Finish Date`, scaled_data$`Upload Date`, units = "days"))

# creating a new column for ranking of views and shares by quantiles (25,50,75 as low, med, high respectively)
eemo_scaled <-
  eemo_scaled %>%
  mutate(rank_views = case_when(views_per_follower < quantile(views_per_follower, 0.25) ~ "Low",
                                views_per_follower >= quantile(views_per_follower, 0.25) & views_per_follower <= quantile(views_per_follower, 0.75) ~ "Medium",
                                views_per_follower > quantile(views_per_follower, 0.75) ~ "High"))
# split versions
eemo_scaled_v1 <-
  eemo_scaled %>%
  filter(`Eemo Version` == "Eemo 1.0")

eemo_scaled_v2 <-
  eemo_scaled %>%
  filter(`Eemo Version` == "Eemo 2.0")

################################# analyze various different methods by ranking of views #################################
# low ranking & version 1
eemo_scaled.low <- eemo_scaled %>%
  filter(rank_views == "Low" & `Eemo Version` == "Eemo 1.0")

# define response variable
y <- eemo_scaled.low$views_per_follower

# define matrix of predictor variables
x <- data.matrix(eemo_scaled.low[, c(2:8)])

#########################################
####### stepwise linear regression ######
#########################################

# low ranking
eemo_scaled.low <- eemo_scaled %>%
  filter(rank_views == "Low")
         
model <- lm(views_per_follower ~ `evalance...17`+
              `earousal...19` +
              reactionValance +
              reactionIntensity +
              `scaledReactionValence...25` +
              `scaledReactionIntensity...26` +
              scaled_lift,
              data = eemo_scaled_v2)

summary(model)
step(model)

model <- lm(views_per_follower ~ 
              `evalance...17`, data = eemo_scaled_v2)
summary(model)$r.squared

# medium ranking
eemo_scaled.med <- eemo_scaled %>%
  filter(rank_views == "Medium")

model <- lm(views_per_follower ~ `evalance...17`+
              `earousal...19` +
              reactionValance +
              reactionIntensity +
              `scaledReactionValence...25` +
              `scaledReactionIntensity...26` +
              scaled_lift,
            data = eemo_scaled.med)

summary(model)
step(model)

model <- lm(views_per_follower ~ `scaledReactionValence...25`, data = eemo_scaled.med)
summary(model) # p-value = 0.01107 with adj-r-square = 0.06055

# high ranking
eemo_scaled.high <- eemo_scaled %>%
  filter(rank_views == "High")

model <- lm(views_per_follower ~ `evalance...17`+
              `earousal...19` +
              reactionValance +
              reactionIntensity +
              `scaledReactionValence...25` +
              `scaledReactionIntensity...26` +
              scaled_lift,
            data = eemo_scaled.high)

summary(model)
step(model)

model <- lm(views_per_follower ~ reactionValance +
              reactionIntensity +
              `scaledReactionValence...25`+
              `scaledReactionIntensity...26`, data = eemo_scaled.high)
summary(model) # p-value = 0.03837 with adj-r-square = 0.1377

################################
####### lasso regression #######
################################

# perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #(0.1630843)

# produce plot of test MSE by lambda values
plot(cv_model)

# find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # the only variables that seem to be influential are "evalance" and "earousal"

# use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# find r-squared
rsq <- 1 - sse/sst
rsq

# find rmse
sqrt(cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]) # (0.6368339)

######################################################################
# low ranking & version 2
eemo_scaled.low <- eemo_scaled %>%
  filter(rank_views == "Low" & `Eemo Version` == "Eemo 2.0")

# define response variable
y <- eemo_scaled.low$views_per_follower

# define matrix of predictor variables
x <- data.matrix(eemo_scaled.low[, c(2:8)])

# perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #(0.2543006)

# produce plot of test MSE by lambda values
plot(cv_model)

# find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # the only variable that seem to be influential "scaledReactionValence"

# use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# find r-squared
rsq <- 1 - sse/sst
rsq #(0.1442239)

# find rmse
sqrt(cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]) # (0.7668345 )

######################################################################
# medium ranking & version 1
eemo_scaled.med <- eemo_scaled %>%
  filter(rank_views == "Medium" & `Eemo Version` == "Eemo 1.0")

# define response variable
y <- eemo_scaled.med$views_per_follower

# define matrix of predictor variables
x <- data.matrix(eemo_scaled.med[, c(2:8)])

# perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #(0.09375854)

# produce plot of test MSE by lambda values
plot(cv_model)

# find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # the only variables that seem to be influential are "scaledReactionIntensity" and "reactionValance"

# use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# find r-squared
rsq <- 1 - sse/sst
rsq #(0.08475491)

# find rmse
sqrt(cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]) # (0.6464128 )

######################################################################
# medium ranking & version 2
eemo_scaled.med <- eemo_scaled %>%
  filter(rank_views == "Medium" & `Eemo Version` == "Eemo 2.0")

# define response variable
y <- eemo_scaled.med$views_per_follower

# define matrix of predictor variables
x <- data.matrix(eemo_scaled.med[, c(2:8)])

# perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #(0.1100469)

# produce plot of test MSE by lambda values
plot(cv_model)

# find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # the only variables that seem to be influential is "evalance"

# use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# find r-squared
rsq <- 1 - sse/sst
rsq #(1.110223e-16)

# find rmse
sqrt(cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]) # (0.6645074 )

######################################################################
# high ranking & version 1
eemo_scaled.high <- eemo_scaled %>%
  filter(rank_views == "High" & `Eemo Version` == "Eemo 1.0")

# define response variable
y <- eemo_scaled.high$views_per_follower

# define matrix of predictor variables
x <- data.matrix(eemo_scaled.high[, c(2:8)])

# perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #(0.2118648)

# produce plot of test MSE by lambda values
plot(cv_model)

# find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # the only variables that seem to be influential are "reactionValance" and "scaledReactionIntensity

# use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# find r-squared
rsq <- 1 - sse/sst
rsq #(0.1174632)

# find rmse
sqrt(cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]) # (0.9326)

######################################################################
# high ranking & version 2
eemo_scaled.high <- eemo_scaled %>%
  filter(rank_views == "High" & `Eemo Version` == "Eemo 2.0")

# define response variable
y <- eemo_scaled.high$views_per_follower

# define matrix of predictor variables
x <- data.matrix(eemo_scaled.high[, c(2:8)])

# perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

# find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #(0.2255715)

# produce plot of test MSE by lambda values
plot(cv_model)

# find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # the only variables that seem to be influential is "scaledReactionIntensity"

# use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# find r-squared
rsq <- 1 - sse/sst
rsq #(0.4122614)

# find rmse
sqrt(cv_model$cvm[cv_model$lambda == cv_model$lambda.1se]) # (0.9089782)
