# load packages
#install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

# load the dataset
EEMO_Analysis <- read_excel("~/Downloads/180 EEMO Campaign v1 and v2 - Full Data and Tik-Tok Behavior - for MV Reg Analyses.xlsx", sheet = "All 203 tiktok formulated",  skip = 2)
EEMO_backup <- EEMO_Analysis # back up original data set
head(EEMO_Analysis)

# remove unnecessary columns
EEMO_Analysis <- EEMO_Analysis[-c(2:3,7,26:59)]
head(EEMO_Analysis)

### adding a new column for Days: time video has been on the platform
EEMO_backup$Days <- as.numeric(difftime(EEMO_Analysis$`Finish Date`, EEMO_Analysis$`Upload Date`, units = "days"))

# remove columns again
#EEMO_Analysis <- EEMO_Analysis[-c(2,5,6,12,21)]
#EEMO_Analysis <- EEMO_Analysis[-c(3)]
#EEMO_Analysis <- EEMO_Analysis[-c(15,16)]

# remove observation with 0 comments and shares
EEMO_Analysis <- EEMO_Analysis[-c(118), ]

# normalize likes/ followers and shares/followers
views_per_follower <- log(EEMO_Analysis$Views/ EEMO_Analysis$Followers)
shares_per_follower <- log(EEMO_Analysis$Shares/ EEMO_Analysis$Followers)

# add to the dataset
EEMO_Analysis$view_per_follower <- views_per_follower
EEMO_Analysis$shares_per_follower <- shares_per_follower

# histogram
hist(views_per_follower)
hist(shares_per_follower)
summary(views_per_follower)

# creating a new column for ranking of views and shares by quantiles
EEMO_Analysis <-
EEMO_Analysis %>%
  mutate(rank_views = case_when(views_per_follower < quantile(views_per_follower, 0.25) ~ "Low",
                          views_per_follower >= quantile(views_per_follower, 0.25) & views_per_follower <= quantile(views_per_follower, 0.75) ~ "Medium",
                          views_per_follower > quantile(views_per_follower, 0.75) ~ "High"))

EEMO_Analysis <-
  EEMO_Analysis %>%
  mutate(rank_shares = case_when(shares_per_follower < quantile(shares_per_follower, 0.25) ~ "Low",
                                 shares_per_follower >= quantile(shares_per_follower, 0.25) & shares_per_follower <= quantile(shares_per_follower, 0.75) ~ "Medium",
                                 shares_per_follower > quantile(shares_per_follower, 0.75) ~ "High"))

# analyze views_per_follower by every potential predictor
ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x = Length, y = views_per_follower)) +
  geom_smooth(mapping = aes(x = Length, y = views_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Length vs. Views per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x = Likes, y = views_per_follower)) +
  geom_smooth(mapping = aes(x = Likes, y = views_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Likes vs. Views per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x = Comments, y = views_per_follower)) +
  geom_smooth(mapping = aes(x = Comments, y = views_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Comments vs. Views per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x = Shares, y = views_per_follower)) +
  geom_smooth(mapping = aes(x = Shares, y = views_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Shares vs. Views per Follower")

ggplot(data = EEMO_Analysis) + # possibly a cubic term??
  geom_point(mapping = aes(x = evalance...16, y = views_per_follower)) +
  geom_smooth(mapping = aes(x = evalance...16, y = views_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Evalance vs. Views per Follower")

ggplot(data = EEMO_Analysis) + #possibly a cubic term?
  geom_point(mapping = aes(x = earousal...17, y = views_per_follower)) +
  geom_smooth(mapping = aes(x = earousal...17, y = views_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Earousal vs. Views per Follower")

ggplot(data = EEMO_Analysis) + # quadratic term?
  geom_point(mapping = aes(x = reactionValance, y = views_per_follower)) +
  geom_smooth(mapping = aes(x = reactionValance, y = views_per_follower), se = FALSE)+
  ggtitle("Scatterplot of reactionValance vs. Views per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x = reactionIntensity, y = views_per_follower)) +
  geom_smooth(mapping = aes(x = reactionIntensity, y = views_per_follower), se = FALSE)+
  ggtitle("Scatterplot of reactionIntensity vs. Views per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x = scaledReactionValence...20, y = views_per_follower)) +
  geom_smooth(mapping = aes(x = scaledReactionValence...20, y = views_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Scaled reactionValence vs. Views per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x =scaledReactionIntensity...21, y = views_per_follower)) +
  geom_smooth(mapping = aes(x = scaledReactionIntensity...21, y = views_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Scaled reactionIntensity vs. Views per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x =scaled_lift, y = views_per_follower)) +
  geom_smooth(mapping = aes(x = scaled_lift, y = views_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Scaled Lift vs. Views per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x =Days, y = views_per_follower)) +
  geom_smooth(mapping = aes(x = Days, y = views_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Days vs. Views per Follower")

# analyze shares_per_follower by every potential predictor
ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x = Length, y = shares_per_follower)) +
  geom_smooth(mapping = aes(x = Length, y = shares_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Length vs. Shares per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x = Likes, y = shares_per_follower)) +
  geom_smooth(mapping = aes(x = Likes, y = shares_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Likes vs. Shares per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x = Comments, y = shares_per_follower)) +
  geom_smooth(mapping = aes(x = Comments, y = shares_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Comments vs. Shares per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x = Views, y = shares_per_follower)) +
  geom_smooth(mapping = aes(x = Views, y = shares_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Views vs. Shares per Follower")

ggplot(data = EEMO_Analysis) + 
  geom_point(mapping = aes(x = evalance...16, y = shares_per_follower)) +
  geom_smooth(mapping = aes(x = evalance...16, y = shares_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Evalance vs. Shares per Follower")

ggplot(data = EEMO_Analysis) + #x^3?
  geom_point(mapping = aes(x = earousal...17, y = shares_per_follower)) +
  geom_smooth(mapping = aes(x = earousal...17, y = shares_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Earousal vs. Shares per Follower")

ggplot(data = EEMO_Analysis) + # x^2?
  geom_point(mapping = aes(x = reactionValance, y = shares_per_follower)) +
  geom_smooth(mapping = aes(x = reactionValance, y = shares_per_follower), se = FALSE)+
  ggtitle("Scatterplot of reactionValance vs. Shares per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x = reactionIntensity, y = shares_per_follower)) +
  geom_smooth(mapping = aes(x = reactionIntensity, y = shares_per_follower), se = FALSE)+
  ggtitle("Scatterplot of reactionIntensity vs. Shares per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x = scaledReactionValence...20, y = shares_per_follower)) +
  geom_smooth(mapping = aes(x = scaledReactionValence...20, y = shares_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Scaled reactionValence vs. Shares per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x =scaledReactionIntensity...21, y = shares_per_follower)) +
  geom_smooth(mapping = aes(x = scaledReactionIntensity...21, y = shares_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Scaled reactionIntensity vs. Shares per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x =scaled_lift, y = shares_per_follower)) +
  geom_smooth(mapping = aes(x = scaled_lift, y = shares_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Scaled Lift vs. Shares per Follower")

ggplot(data = EEMO_Analysis) +
  geom_point(mapping = aes(x =Days, y = shares_per_follower)) +
  geom_smooth(mapping = aes(x = Days, y = shares_per_follower), se = FALSE)+
  ggtitle("Scatterplot of Days vs. Shares per Follower")

# regression with views_per_follower
model1 <- lm(views_per_follower ~  
                                    evalance...16+
                                    earousal...17 + 
                                    scaled_lift + 
                                    scaledReactionValence...20 +
                                    scaledReactionIntensity...21,
                                    data = EEMO_Analysis)
summary(model1) #aic = 705.48

summary(step(model1))
model1.5 <- glm(views_per_follower ~ Likes +
                                     Comments +
                                     Days,
                                     data = EEMO_Analysis )

summary(model1.5) #aic = 697.1
with(summary(model1.5), 1 - deviance/null.deviance) # (0.2627529)

# regression with shares_per_followers
model2 <- lm(shares_per_follower ~ Length +
                                         Likes +
                                         Comments +
                                         evalance...16+
                                         earousal...17 +
                                         scaled_lift + 
                                         scaledReactionValence...20 +
                                         scaledReactionIntensity...21 +
                                         Days,
                                         data = EEMO_Analysis)
summary(model2) #aic = 812.43

step(model2)
model2.5 <- glm(shares_per_follower ~ Days +
                                                scaledReactionValence...20 +
                                                Likes +
                                                Length, 
                                                data = EEMO_Analysis )
summary(model2.5) #aic = 804.95
with(summary(model2.5), 1 - deviance/null.deviance) # (0.3602509)

### polynomial regression: views_per_follower 
model3 <- glm(views_per_follower ~  poly(Length, 2) +
                Likes +
                poly(evalance...16, 3)+
                poly(earousal...17, 3) + 
                poly(reactionValance, 2) +
                reactionIntensity +
                scaled_lift + 
                scaledReactionValence...20 +
                scaledReactionIntensity...21 +
                Days, data = EEMO_Analysis)
summary(model3) # aic = 712.55

step(model3)
model3.5 <- glm(views_per_follower ~ Likes +
                                     Days,
                                     data = EEMO_Analysis)
summary(model3.5) # aic = 697.14
with(summary(model3.5), 1 - deviance/null.deviance) # (0.2543546)

### polynomial regression: shares_per_follower 
model4 <- glm(shares_per_follower ~  Length +
                Likes +
                Comments +
                evalance...16+
                poly(earousal...17, 3) + 
                poly(reactionValance, 2) +
                reactionIntensity +
                poly(scaled_lift, 2) + 
                poly(scaledReactionValence...20, 3) +
                scaledReactionIntensity...21 +
                Days, data = EEMO_Analysis)

summary(model4) # aic = 817.04

step(model4)
model4.5 <- glm(shares_per_follower ~ Days +
                                      reactionIntensity +
                                      poly(reactionValance, 2)+
                                      Likes, 
                                      data = EEMO_Analysis)
summary(model4.5) # aic = 800.36
with(summary(model4.5), 1 - deviance/null.deviance) # (0.383262)

################################################################################

# Principal Component Analysis
eemopc <- EEMO_Analysis[, 2:15]
eemopc <- eemopc[, 7:8]
eemopc <- EEMO_Analysis[, -c(1:11, 15:16, 23:25)]
eemopc <- eemopc[, - c(7:9)]
pc.eemo <- princomp(eemopc, cor = TRUE)
pc.eemo

# information output
names(pc.eemo)

# quick summary
summary(pc.eemo)

# eigenvalues/ eigenvectors
eigenvectors <- pc.eemo$loadings
eigenvalues <- (pc.eemo$sdev)^2

eigenvectors
round(cor(EEMO_Analysis[, c(2,3,4,8,9,12:15)], pc.eemo$scores), 3)

# scree plot

eemopc2 <-eemopc[, -c(2,6)]
pc.eemo2 <- princomp(eemopc2, cor = TRUE)

screeplot(pc.eemo, type = "l", main = "Screeplot")
abline(1, 0, col = "red", lty = 2) # 4 components

plot(pc.eemo$scores[, 1:4], type = 'n', xlab = "PC1", ylab = "PC2")
points(pc.eemo$scores[, 1:4], cex = 0.5)

################################################################################
# principal component regression: views/follower
eemo <- EEMO_Analysis[, -c(1, 3, 7, 17:19)]
eemo <- eemo[, -c(4,7:8)]
str(eemo)

# split data to train and test set
# 75% of sample size
idx <- floor(0.75 * nrow(eemo))

# set seed for reproducibility
train_ind <- sample(seq_len(nrow(eemo)), size = idx)
train <- eemo[train_ind, ]
test <- eemo[-train_ind, ]

# simple linear regression model
simple_lm <- lm(view_per_follower ~ ., data = train)
summary(simple_lm)

library(Metrics)
lm_pred <- predict(simple_lm, test)
rmse(actual = test$view_per_follower, predicted = as.numeric(lm_pred))
# RMSE : a standard way to measure the error of a model in predicting quantitative data (1.870115)

#install.packages("pls")
library(pls)
pcr_model <- pcr(view_per_follower ~ ., 
                 data = train,
                 scale = TRUE,
                 validation = "CV")
summary(pcr_model)
pcr_pred <- predict(pcr_model, test, ncomp = 3)
rmse(actual = test$view_per_follower, predicted = as.numeric(pcr_pred)) # (1.818364)

#psych package
#install.packages("psych")
library(psych)
pc.fit <- prcomp( ~ Length+
                    Likes+
                    Comments+
                    evalance...16+
                    earousal...17+
                    reactionValance+
                    reactionIntensity+
                    scaledReactionValence...20+
                    scaledReactionIntensity...21+
                    scaled_lift+
                    Days,
                  data = train,
                  scale = TRUE)
summary(pc.fit)
screeplot(pc.fit, type = "l", main = "Screeplot")
abline(1, 0, col = "red", lty = 2) # 4 components

# 4 or 5 components look to be the best
trans_test <- as.data.frame(predict(pc.fit, test)[,1:4])

new_train <- as.data.frame(cbind(train$view_per_follower, pc.fit$x[,1:4]))
colnames(new_train)[1] <- "view_per_follower"
str(new_train)
pcr_lm_model <- lm(view_per_follower ~., data = new_train)
summary(pcr_lm_model)

pcr_predictions <- predict(pcr_lm_model, trans_test)
rmse(actual = test$view_per_follower, predicted = as.numeric(pcr_predictions)) # (1.950667)



