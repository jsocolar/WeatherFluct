#### BBS Exploratory Analyses in both JAGS and lmer
## May 1st, 2018
## Austin Spence

# load packages
library(R2jags)

# Set working directory and load the data
setwd("C:/Users/Tingley Lab_2/Dropbox/BBS_Project") # Lab 2 Computer directory

lag1_weather <- read.csv("./BBS_Data/lag1_weather.csv")
bbs_data <- read.csv("./BBS_Data/97_16_bbs_weather.csv")


# Make R*
bbs_data$r_star <- bbs_data$n2_total/bbs_data$n1_total
bbs_data$aou <- as.factor(bbs_data$aou)

# Cut it down to the 30 practice birds
bbs_data <- bbs_data[bbs_data$aou %in% c(05070, 04940, 03880, 04610, 04650,
                                          04630, 04670, 04560, 06290, 07250,
                                          07222, 07210, 07550, 07560, 07040,
                                          06390, 06740, 06760, 06410, 06770,
                                          06840, 06540, 06620, 06590, 05630,
                                          05810, 06080, 05950, 05980, 06040),]


#### JAGS Method --------------------------------------------------------------------------------------------
bbs_jags_data <- list(
  r_star = bbs_data$r_star,
  max = bbs_data$max_high_ESu,
  min = bbs_data$min_low_ESu,
  precip = bbs_data$mean_ESu_Precip,
  swe = bbs_data$mean_ESu_SWE,
  mean_max = bbs_data$mean_high_ESu,
  mean_min = bbs_data$mean_low_ESu,
  aou = bbs_data$aou,
  bbs_obs = length(bbs_data$year1),
  aou_obs = length(bbs_data$aou)
)


bbs_jag_model1 <- jags(data = bbs_jags_data,
                       parameters.to.save = c("b.max", "b.min", "b.precip",
                                              "b.swe", "b.mean_max", "b.mean_min"),
                       model.file = "./Code/JAGS/bbs_jags_model1.R",
                       n.chains = 3,
                       n.iter = 10000,
                       n.burnin = 5000)
bbs_jag_model1



######### LME4 -----------------------------------------------------------------------------------------------
library(lmerTest)
bbs_data <- lag
bbs_data$r_star <- bbs_data$n2_total/bbs_data$n1_total
bbs_data$aou <- as.factor(bbs_data$aou)
bbs_data$Z_meanmaxESu <- scale(bbs_data$Z_meanmaxESu)

bbs_data$logR <- log(bbs_data$r_star)

bbs_data$mean_high_ESu <- as.numeric(bbs_data$mean_high_ESu)
bbs_data$t_anom <- scale(bbs_data$mean_high_ESu - bbs_data$overall_meanmaxESu)

## Creating interaction term between mean high summer and zscore between site and bird high temperatures
bbs_data$z_interaction <- bbs_data$mean_high_ESu * bbs_data$Z_meanmaxESu

scta <- bbs_data[which(bbs_data$aou == 05810),]

mod1 <- lm(logR ~ Z_meanmaxESu*t_anom, data=bbs_data)

summary(mod1)

# based on using bbs_data
mod1 <- lmer(logR ~ t_anom*Z_meanmaxESu + (t_anom+Z_meanmaxESu+t_anom:Z_meanmaxESu | aou), data = bbs_data)

mod2 <- lmer(r_star ~ Z_meanmaxESu + 
               (Z_meanmaxESu | aou), data = bbs_data)

# target swe to resident later during winter
z-score  - year - mean(site)/sd(site)
# high z scores indicate hot parts of the range
# 
# currently expecting a negative interaction 





