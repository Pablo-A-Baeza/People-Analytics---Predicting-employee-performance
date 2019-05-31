library(tidyverse)
library(psych)
library(QuantPsyc)
library(likert)
library(Hmisc)

customer_data <- read.table(("Customer sat N2507.csv"), header = T, sep = ",")
attach(customer_data)


# ----- Data Analysis ----- #

# Understanding our data
str(customer_data)
names(customer_data)
nrow(customer_data)
ncol(customer_data)
fix(customer_data)

# ----- Likert data analysis ----- #

likert_data <- customer_data[1:4] 

# Response frequencies
r_fr <- response.frequencies(likert_data) %>% round(2) %>% as.data.frame
r_fr

likert_data <- likert_data %>%
  mutate_if(is.numeric, as.factor)

summary(likert_data)

likert_data_results <- likert(likert_data)
plot(likert_data_results, group.order = c("Sat1", "Sat2", "Sat3", "Sat4")) # Note. For percent numbers, responses are grouped into "low", "neutral", and "high"

# Total number of complete cases
data_complete <- nrow(na.omit(likert_data))

# Total number of incomplete cases by variable
colSums(is.na(likert_data))
data_missing <- sum(colSums(is.na(likert_data))) # Total number

data_summary <- cbind(data_complete, data_missing)
rownames(data_summary) <- paste("Survey reponses",sep = " ")
data_summary

# Reliability analysis
likert_data <- likert_data %>%
  mutate_if(is.factor, as.numeric)

# Cronbach's alpha
cronbachs_alpha <- alpha(likert_data)
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

