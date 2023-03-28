# Problem Set 7: Data Imputation Code

# install.packages("mice")
# install.packages("modelsummary")
library(mice)
library(modelsummary)
library(readr)
library(tidyverse)

# load in wage data
wages <- read.csv("ProblemSets/PS7/wages.csv")
head(wages)

wages1 <- filter(wages, !is.na(hgc) & !is.na(tenure))
wages1 <- wages1 %>% mutate(tenure_sq = tenure^2)
dim(wages)
dim(wages1)
head(wages1)

datasummary_skim(wages1)
# 25% of logwages are missing, not sure what type of missingness this is.

# base estimation model - for my own use
est <- lm(logwage ~ hgc + college + tenure + tenure_sq + age + married, 
          data = wages1) 

# listwise deletion for logwage missing values
wages.listwise <- filter(wages1, !is.na(logwage))
est1 <- lm(logwage ~ hgc + college + tenure + tenure_sq + age + married, 
           data = wages.listwise) 
summary(est1)

# mean imputation method
wages.mean <- wages1
wages.mean$logwage[is.na(wages.mean$logwage)] <- 
  mean(wages.mean$logwage, na.rm = TRUE)
est2 <- lm(logwage ~ hgc + college + tenure + tenure_sq + age + married, 
           data = wages.mean) 
summary(est2)

# predicted values - MAR assumption
    # using base data with no adjustments
wages.predicted <- wages1
wages.predicted$logwage[is.na(wages.predicted$logwage)] <- 
  predict(est1, newdata = wages.predicted[is.na(wages.predicted$logwage),])
est3 <- lm(logwage ~ hgc + college + tenure + tenure_sq + age + married, 
           data = wages.predicted) 
summary(est3)

# multiple imputation regression
wages.imputed <- mice(wages1, m=5, method = "pmm", seed = 1234)
est4 <- with(wages.imputed, 
             lm(logwage ~ hgc + college + tenure + tenure_sq + age + married))
summary(est4)

# modelsummary of four models
models <- list(est1, est2, est3, est4)
modelsummary(models, stars = T)

# get outputs for two visualizations in this assignment
datasummary_skim(wages1, histogram = F, output = "wages1summary.tex")
modelsummary(models, stars = T, output = "modelsummmary.tex")
