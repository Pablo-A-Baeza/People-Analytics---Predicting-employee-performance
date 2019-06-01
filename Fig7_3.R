library(tidyverse)
library(psych)
library(QuantPsyc)
library(likert)
library(Hmisc)

survey <- read.table(("Customer sat N2507.csv"), header = T, sep = ",")
attach(survey)


# ----- Data Analysis ----- #

# Understanding our data
str(survey)
names(survey)
nrow(survey)
ncol(survey)
fix(survey)

# ----- Likert data analysis ----- #

likert_sat <- survey[1:4] 
likert_loyalty <- survey[6]
likert_invest <- survey[7]
my_list <- list(likert_sat, likert_loyalty, likert_invest)

# Response frequencies
responses_fr <- lapply(my_list, response.frequencies)
responses_fr

# Sales-person competency (Sat 1,2,3,4)
likert_sat <- likert_sat %>%
  mutate_if(is.numeric, as.factor)

summary(likert_data)

likert_data_results <- likert(likert_sat)
plot(likert_data_results, group.order = c("Sat1", "Sat2", "Sat3", "Sat4"))

#Note. For percent numbers, responses are grouped into "low", "neutral", and "high"

# Total number of complete cases
data_complete <- nrow(na.omit(likert_sat))

# Total number of incomplete cases by variable
colSums(is.na(likert_sat))
data_missing <- sum(colSums(is.na(likert_sat))) # Total number

data_summary <- cbind(data_complete, data_missing)
rownames(data_summary) <- paste("Survey reponses",sep = " ")
data_summary

# Reliability analysis
likert_sat <- likert_sat %>%
  mutate_if(is.factor, as.numeric)

# Cronbach's alpha
cronbachs_alpha <- alpha(likert_sat)
cronbachs_alpha_summary <- cronbachs_alpha$item.stats
cronbachs_alpha_r <- round(cronbachs_alpha$total$raw_alpha, 3) 
cronbachs_alpha_r # Note. Reliability values values are min .7. Our scale is very reliable :D

# Cronbach's alpha if item drop
cronbachs_alpha_drop <- round(cronbachs_alpha$alpha.drop$raw_alpha, 3) 
cronbachs_alpha_drop
cronbachs_alpha_drop_summary <- ifelse(cronbachs_alpha_drop >
                                         cronbachs_alpha_r, "BAD", "OK")

cronbachs_alpha_drop_summary # Note. None of the items yield a higher alpha value if we drop them :D

# Cronbach's alpha for Corrected - Item Total Correlation
cronbachs_alpha_corrected <- round(cronbachs_alpha$item.stats$r.drop, 3)
cronbachs_alpha_corrected
cronbachs_alpha_corrected_summary <- ifelse(cronbachs_alpha_corrected <
                                              0.3, "BAD", "OK")

cronbachs_alpha_corrected_summary # Note. None of items present a a correlation lower than .3 :D


# Customer loyalty (Custoloyalty)
likert_loyalty <- likert_loyalty %>%
  mutate_if(is.numeric, as.factor)

summary(likert_loyalty)

likert_loyalty_results <- likert(likert_loyalty)
plot(likert_loyalty_results)

# Complete cases
data_complete <- nrow(na.omit(likert_loyalty))

# Missing cases
data_missing <- colSums(is.na(likert_loyalty))

# Data summary
cbind(data_complete, data_missing)


#shows data with columns

fix(Custom)

names(Custom)

#[1] "Sat1"             "Sat2"             "Sat3"             "Sat4"            
#[5] "CustSatMean"          "Custloyalty"      "INvestMore"       "SexOfSalesperson"


#make the dataset live
attach(Custom)

#Figure 7.3

modelf7_3=lm(Custloyalty ~ Sat1 + Sat2 + Sat3 + Sat4 + SexOfSalesperson)
modelf7_3
summary(modelf7_3)
coef(modelf7_3)

#for standardised coefficients install and load QuantPsyc package
#install.packages("QuantPsyc") -if not already installed
library(QuantPsyc)
lm.beta(modelf7_3)

