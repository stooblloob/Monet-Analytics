# load packages
#install.packages("tidyverse")
#install.packages("lattice")
#install.packages("reshape2")
library(reshape2)
library(lattice)
library(tidyverse)
library(readxl)
library(dplyr)

# load the dataset
EEMO_Analysis2 <- read_excel("~/Downloads/180 EEMO Campaign v1 and v2 - Full Data and Tik-Tok Behavior - for MV Reg Analyses.xlsx", sheet = "All 203 tiktok formulated",  skip = 2)
EEMO_backup <- EEMO_Analysis2 # back up original data set
head(EEMO_Analysis2)

# remove unnecessary columns
EEMO_Analysis2 <- EEMO_Analysis2[-c(2:3,7,26:59)]
head(EEMO_Analysis2)

### finding the percentiles of followers
# quartiles
followers <- EEMO_Analysis2$Followers
quantile(followers, c(.25, .50, .75))

# deciles
quantile(followers, probs = seq(.1, .9, by = .1))

# separating data frame by followers percentiles (Quartiles)
twenty_fifth_percentile <- 
EEMO_Analysis2 %>%
  filter(Followers > 801900)

fiftieth_percentile <-
  EEMO_Analysis2 %>%
  filter(Followers > 3200000)

seventy_fifth_percentile <-
  EEMO_Analysis2 %>%
  filter(Followers > 12100000)

# plotting by followers percentiles
ggplot(data = twenty_fifth_percentile) +
  geom_point(mapping = aes(x = `Upload Date`, y = Views)) +
  geom_smooth(mapping = aes(x = `Upload Date`, y = Views), se = FALSE)+
  ggtitle("Scatterplot of 25th Percentile: Upload Date vs. Views")

ggplot(data = fiftieth_percentile) +
  geom_point(mapping = aes(x = `Upload Date`, y = Views)) +
  geom_smooth(mapping = aes(x = `Upload Date`, y = Views), se = FALSE)+
  ggtitle("Scatterplot of 50th Percentile: Upload Date vs. Views")

ggplot(data = seventy_fifth_percentile) +
  geom_point(mapping = aes(x = `Upload Date`, y = Views)) +
  geom_smooth(mapping = aes(x = `Upload Date`, y = Views), se = FALSE)+
  ggtitle("Scatterplot of 75th Percentile: Upload Date vs. Views")

ggplot(data = twenty_fifth_percentile) +
  geom_point(mapping = aes(x = Views, y = score_v1_5...23)) +
  geom_smooth(mapping = aes(x = Views, y = score_v1_5...23), se = FALSE)+
  ggtitle("Scatterplot of 25th Percentile: Views vs. Score_v1_5")

ggplot(data = fiftieth_percentile) +
  geom_point(mapping = aes(x = Views, y = score_v1_5...23)) +
  geom_smooth(mapping = aes(x = Views, y = score_v1_5...23), se = FALSE)+
  ggtitle("Scatterplot of 50th Percentile: Views vs. Score_v1_5")

ggplot(data = seventy_fifth_percentile) +
  geom_point(mapping = aes(x = Views, y = score_v1_5...23)) +
  geom_smooth(mapping = aes(x = Views, y = score_v1_5...23), se = FALSE)+
  ggtitle("Scatterplot of 75th Percentile: Views vs. Score_v1_5")

### separate two versions
EEMO_V1 <-
EEMO_Analysis2 %>%
  filter(`Eemo Version` == "Eemo 1.0")

EEMO_V2 <-
  EEMO_Analysis2 %>%
  filter(`Eemo Version` == "Eemo 2.0")
  

### analyze by upload date
norm_Views <- log(EEMO_Analysis2$Views)
norm_Shares <- log(EEMO_Analysis2$Shares)

hist(norm_Views)
hist(norm_Shares)

ggplot(data = EEMO_Analysis2) +
  geom_point(mapping = aes(x = `Upload Date`, y = Views)) +
  geom_smooth(mapping = aes(x = `Upload Date`, y = Views), se = FALSE)+
  ggtitle("Scatterplot of Upload Date vs. Views")

ggplot(data = EEMO_Analysis2) +
  geom_point(mapping = aes(x = `Upload Date`, y = norm_Views)) +
  geom_smooth(mapping = aes(x = `Upload Date`, y = norm_Views), se = FALSE) +
  ggtitle("Scatterplot of Upload Date vs. Normalized Views")

ggplot(data = EEMO_Analysis2) +
  geom_point(mapping = aes(x = `Upload Date`, y = Shares)) +
  geom_smooth(mapping = aes(x = `Upload Date`, y = Shares), se = FALSE)+
  ggtitle("Scatterplot of Upload Date vs. Shares")

ggplot(data = EEMO_Analysis2) +
  geom_point(mapping = aes(x = `Upload Date`, y = norm_Shares)) +
  geom_smooth(mapping = aes(x = `Upload Date`, y = norm_Shares), se = FALSE) +
  ggtitle("Scatterplot of Upload Date vs. Normalized Shares")

ggplot(data = EEMO_Analysis2) +
  geom_point(mapping = aes(x = `Upload Date`, y = Likes)) +
  geom_smooth(mapping = aes(x = `Upload Date`, y = Likes), se = FALSE)+
  ggtitle("Scatterplot of Upload Date vs. Likes")

### analyze by length of video
ggplot(data = EEMO_Analysis2) +
  geom_point(mapping = aes(x = Length, y = Likes)) +
  ggtitle("Scatterplot of Length of Video vs. Likes")

ggplot(data = EEMO_Analysis2) +
  geom_point(mapping= aes(x = Length, y = Views))+
  ggtitle("Scatterplot of Length of Video vs. Views")

ggplot(data = EEMO_Analysis2) +
  geom_point(mapping= aes(x = Length, y = norm_Views))+
  ggtitle("Scatterplot of Length of Video vs. Normalized Views")

ggplot(data = EEMO_Analysis2) +
  geom_point(mapping= aes(x = Length, y = Shares))+
  ggtitle("Scatterplot of Length of Video vs. Shares")

ggplot(data = EEMO_Analysis2) +
  geom_point(mapping= aes(x = Length, y = norm_Shares))+
  ggtitle("Scatterplot of Length of Video vs. Normalized Shares")

### analyzing the predictive score: "score_v1_5...23"
hist(EEMO_Analysis2$score_v1_5...23)
summary(EEMO_Analysis2$score_v1_5...23)

### analyzing existing  model

fitted_model <-lm(score_v1_5...23 ~ earousal...17 + 
                                    scaled_lift + 
                                    scaledReactionIntensity...21,
                                    data = EEMO_Analysis2)
summary(fitted_model)    

### new model???
model <- glm(score_v1_5...23 ~ Followers +
                               Likes +
                               Comments +
                               Views +
                               Shares +
                               evalance...16+
                               earousal...17 + 
                               scaled_lift + 
                               scaledReactionValence...20 +
                               scaledReactionIntensity...21,
                               data = EEMO_Analysis2)
summary(model)
anova(model, test = "Chisq")

# backwards stepwise model selection (Views has been removed)
step(model) 

reduced_model <- glm(score_v1_5...23 ~ Followers +
                                       Likes +
                                       Comments +
                                       Shares +
                                       evalance...16+
                                       earousal...17 + 
                                       scaled_lift + 
                                       scaledReactionValence...20 +
                                       scaledReactionIntensity...21,
                                       data = EEMO_Analysis2)
summary(reduced_model)

### adding a new column for Days: time video has been on the platform
EEMO_Analysis2$Days <- as.numeric(difftime(EEMO_Analysis2$`Finish Date`, EEMO_Analysis2$`Upload Date`, units = "days"))

# plot by days
ggplot(data = EEMO_Analysis2) +
  geom_point(mapping= aes(x = Days, y = Views))+
  ggtitle("Scatterplot oF Days vs. Views")

ggplot(data = EEMO_Analysis2) +
  geom_point(mapping = aes(x = Days, y = Shares)) +
  ggtitle("Scatterplot of Days vs. Shares")

ggplot(data = EEMO_Analysis2) +
  geom_point(mapping = aes(x = Days, y = score_v1_5...23)) +
  ggtitle("Scatterplot of Days vs. Score")

# remove columns again?
EEMO_Analysis2 <- EEMO_Analysis2[-c(2,5,6,12,21)]

### more regression analysis .0.
regression <- glm( ~ Followers +
                                   Likes +
                                   Comments +
                                   Views +
                                   Shares +
                                   evalance...16+
                                   earousal...17 + 
                                   reactionValance +
                                   reactionIntensity +
                                   scaled_lift + 
                                   scaledReactionValence...20 +
                                   scaledReactionIntensity...21 +
                                   Days,
                                   data = EEMO_Analysis2 )
summary(regression)

step(regression)
reduced_regression <- glm(score_v1_5...23 ~
                    Likes +
                    earousal...17 + 
                    reactionValance +
                    scaled_lift + 
                    scaledReactionIntensity...21 +
                    Days,
                    data = EEMO_Analysis2 )

summary(reduced_regression)

# correlation?
corr_mat <- round(cor(EEMO_Analysis2),2) 
head(corr_mat)

melted_corr_mat <- melt(corr_mat)

ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


