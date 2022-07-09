# load packages
library(readxl)
library(xlsx)

# load the dataset
EEMO_Analysis <- read_excel("~/Downloads/180 EEMO Campaign v1 and v2 - Full Data and Tik-Tok Behavior - for MV Reg Analyses.xlsx", sheet = "All 203 tiktok formulated",  skip = 2)

# 3 SEC LONG
hist(EEMO_Analysis$evalance...26)
summary(EEMO_Analysis$evalance...26)
sd(EEMO_Analysis$evalance...26)
(100-50.360)/18.46968 # = 2.687648 (.0037 = .37%)
(50.360-0)/18.46968 # = 2.726631 (.033 = 3.3%)
.37+3.3

hist(EEMO_Analysis$earousal...27)
summary(EEMO_Analysis$earousal...27)
sd(EEMO_Analysis$earousal...27)
(100-48.48)/18.88779 # = 2.727688 (.033 = 3.3%)
(48.48-0)/18.88779 # = 2.566738 (.0052 = .52%)
3.3+.52

hist(EEMO_Analysis$scaledReactionValence...28)
summary(EEMO_Analysis$scaledReactionValence...28)
sd(EEMO_Analysis$scaledReactionValence...28)
(100-98.2)/39.44233 # = 0.04563625 (.484 = 48.4%)
(98.2-0)/39.44233 # = 2.489711 (.0066 = .66%)
48.4+.66

hist(EEMO_Analysis$scaledReactionIntensity...29)
summary(EEMO_Analysis$scaledReactionIntensity...29)
sd(EEMO_Analysis$scaledReactionIntensity...29)
(100-69.17)/20.58412 # = 1.497757 (.0681 = 6.81%)
(69.17-0)/20.58412 # = 3.360357 (.00039 = .03%)
6.81+.03

# 6 SEC LONG
hist(EEMO_Analysis$evalance...34)
summary(EEMO_Analysis$evalance...34)
sd(EEMO_Analysis$evalance...34)
(100-50.828)/18.80863 # = 2.614332 (.0045 = .45%)
(50.828-0)/18.80863 # = 2.702377 (.0035 = .35%)
.45+.35

hist(EEMO_Analysis$earousal...35)
summary(EEMO_Analysis$earousal...35)
sd(EEMO_Analysis$earousal...35)
(100-50.650)/18.27084 # = 2.701025 (.0035 = .35%)
(50.650-0)/18.27084 # = 2.701025 (.0035 = .35%)
.35+.35

hist(EEMO_Analysis$scaledReactionValence...36)
summary(EEMO_Analysis$scaledReactionValence...36)
sd(EEMO_Analysis$scaledReactionValence...36)
(100-95.79)/33.254 # = 0.1266013 (.4522 = 45.22%)
(95.79-0)/33.254 # = 2.880556 (.0020 = .2%)
45.22+.2

hist(EEMO_Analysis$scaledReactionIntensity...37)
summary(EEMO_Analysis$scaledReactionIntensity...37)
sd(EEMO_Analysis$scaledReactionIntensity...37)
(100-66.11)/16.10127 # = 2.104803 (.0179 = 1.79%)
(66.11-0)/16.10127 # = 4.105887 (0)
1.79

# FULL LENGTH
hist(EEMO_Analysis$evalance...16)
summary(EEMO_Analysis$evalance...16)
sd(EEMO_Analysis$evalance...16)
(100-52.241)/20.84759 # = 2.290864 (.0110 = 1.1%)
(52.241-0)/20.84759 # = 2.505853 (.0062 = .62%)
1.1+.62

hist(EEMO_Analysis$earousal...17)
summary(EEMO_Analysis$earousal...17)
sd(EEMO_Analysis$earousal...17)
(100-55.193)/18.80421 # = 2.382817 (.0087 = .87%)
(55.193-0)/18.80421 # = 2.935141 (.0017 = .17%)
.87+.17

hist(EEMO_Analysis$scaledReactionValence...20)
summary(EEMO_Analysis$scaledReactionValence...20)
sd(EEMO_Analysis$scaledReactionValence...20)
(100-85.97)/23.2854 # = 0.6025235 (.2743 = 27.43%)
(85.97-0)/23.2854 # = 3.692013 (.00011 = .01%)
27.43+.01

hist(EEMO_Analysis$scaledReactionIntensity...21)
summary(EEMO_Analysis$scaledReactionIntensity...21)
sd(EEMO_Analysis$scaledReactionIntensity...21)
(100-53.04)/13.44294 # = 3.493283 (.00024 = .02%)
(53.04-0)/13.44294 # = 3.945565 (.00004 = .004%)
.02+.004

# evalance: 3 to 6 sec, the shape did not change much but to full it did change
# earousal: 3 to 6 sec to full, it did change
# scaledReactionValence: 3 to 6 sec to full, changed
# scaledReactionIntensity: changed throughout

### pairwise scatterplot
pairs(~ EEMO_Analysis$evalance...26 + EEMO_Analysis$evalance...34 + EEMO_Analysis$evalance...16, data = EEMO_Analysis)
pairs(~ EEMO_Analysis$earousal...27 + EEMO_Analysis$earousal...35 + EEMO_Analysis$earousal...17, data = EEMO_Analysis)
pairs(~ EEMO_Analysis$scaledReactionValence...28 + EEMO_Analysis$scaledReactionValence...36 + EEMO_Analysis$scaledReactionValence...20, data = EEMO_Analysis)
pairs(~ EEMO_Analysis$scaledReactionIntensity...29 + EEMO_Analysis$scaledReactionIntensity...37 + EEMO_Analysis$scaledReactionIntensity...21, data = EEMO_Analysis)

pairs(~ EEMO_Analysis$evalance...26 + EEMO_Analysis$earousal...27 + EEMO_Analysis$scaledReactionValence...28 + EEMO_Analysis$scaledReactionIntensity...29, data = EEMO_Analysis)
pairs(~ EEMO_Analysis$evalance...34 + EEMO_Analysis$earousal...35 + EEMO_Analysis$scaledReactionValence...36 + EEMO_Analysis$scaledReactionIntensity...37, data = EEMO_Analysis)
pairs(~ EEMO_Analysis$evalance...16 + EEMO_Analysis$earousal...17 + EEMO_Analysis$scaledReactionValence...20 + EEMO_Analysis$scaledReactionIntensity...21, data = EEMO_Analysis)

### correlation 
# evalance
cor.test(EEMO_Analysis$evalance...26, EEMO_Analysis$evalance...34) # 3v6: 0.9608158
cor.test(EEMO_Analysis$evalance...26, EEMO_Analysis$evalance...16) # 3vfull: 0.8600486
cor.test(EEMO_Analysis$evalance...16, EEMO_Analysis$evalance...34) # fullv6: 0.9249112

# earousal
cor.test( EEMO_Analysis$earousal...27, EEMO_Analysis$earousal...35) # 3v6: 0.9579973
cor.test( EEMO_Analysis$earousal...27, EEMO_Analysis$earousal...17) # 3vfull: 0.8023901
cor.test( EEMO_Analysis$earousal...17, EEMO_Analysis$earousal...35) # fullv6: 0.8551503

# scaledReactionValence
cor.test(EEMO_Analysis$scaledReactionValence...28, EEMO_Analysis$scaledReactionValence...36) # 3v6: 0.8849686
cor.test(EEMO_Analysis$scaledReactionValence...28, EEMO_Analysis$scaledReactionValence...20) # 3vfull: 0.7566702
cor.test(EEMO_Analysis$scaledReactionValence...20, EEMO_Analysis$scaledReactionValence...36) # fullv6: 0.8674394

# scaledReactionIntensity
cor.test(EEMO_Analysis$scaledReactionIntensity...29, EEMO_Analysis$scaledReactionIntensity...37) # 3v6: 0.7395934
cor.test(EEMO_Analysis$scaledReactionIntensity...29, EEMO_Analysis$scaledReactionIntensity...21) # 3vfull: 0.562259
cor.test(EEMO_Analysis$scaledReactionIntensity...21, EEMO_Analysis$scaledReactionIntensity...37) # fullv6: 0.7592148

### views & shares
hist(EEMO_Analysis$Views)
summary(EEMO_Analysis$Views)
sd(EEMO_Analysis$Views)
(100-55276617)/112763435 # = -0.4901989 (1-0.3121 = .6879)
(55276617-0)/112763435 # = 0.4901998 (.3121)
.3121+.6879

hist((EEMO_Analysis$Shares))
summary(EEMO_Analysis$Shares)
sd(EEMO_Analysis$Shares)
(100-614540)/4439027 # = -0.1384177 (1-.4483 = 0.5517)
(614540-0)/4439027 # = 0.1384402 (.4483)
0.5517+.4483

plot(EEMO_Analysis$Views, EEMO_Analysis$Shares)

cor.test(EEMO_Analysis$Views, EEMO_Analysis$Shares)

### normalized views & shares
hist(log(EEMO_Analysis$Views))

hist(log(EEMO_Analysis$Shares))

plot(log(EEMO_Analysis$Views), log(EEMO_Analysis$Shares))

cor.test(log(EEMO_Analysis$Views), log(EEMO_Analysis$Shares))

### view & shares over followers
views_over_followers <- EEMO_Analysis$Views/EEMO_Analysis$Shares
shares_over_followers <- EEMO_Analysis$Shares/EEMO_Analysis$Followers
hist(views_over_followers)
hist(shares_over_followers)

### normalized views & shares over followers
hist(log(views_over_followers))
hist(log(shares_over_followers))

### heatmap??
#cor(EEMO_Analysis[,c('EEMO_Analysis$scaledReactionIntensity...29', 'EEMO_Analysis$scaledReactionIntensity...37', 'EEMO_Analysis$scaledReactionIntensity...21')])
