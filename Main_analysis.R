# Author: Donata Stonkute
###############################################################################
# --------------------------------
# This script includes discrete-time regression models, life table simulation  
# and the Sullivan method for a single country. For illustration, this contains 
# an example of Austria.
# This produces gender- and education-specific outputs.
# 
# --------------------------------

###
# Austria
###

rm(list=ls())
getwd()
setwd("your_directory")
Sys.setenv(LANG = "en")


source('functions_P1.R')


# libraries
library(tidyverse)
library(dplyr)
library(future)
library(furrr)
library(data.table)
library(tictoc)
library(Hmisc)



dat <- fread("Data/mortality_sample.csv")
dat <- dat %>% 
  filter(country == "Austria")


dta_prev <- fread("Data/prevalence.csv")
w5 <- dta_prev %>% 
  mutate(age_gr = cut(
    age,
    breaks = c(seq(50, 86, by = 2), 120),
    labels = seq(50,86,2),
    right = FALSE), 
  ) %>% 
  filter(country=="Austria")

bs_it = 1000 # number of bootstraps

# create empty vectors to store the results
df_start <- c()

tic() # for time monitoring
plan(multisession, workers = 10) # if more cores chosen, process becomes slower
# plan(sequential) # if run locally, instead of servers


df_start <- 1:bs_it %>% furrr::future_map(~ { # ~ everything on the left
  age <- seq(50, 110, 2) 
  
  resampled_data <- long_resample(dat=dat, id= "mergeid", time= "wave")
  

# 1. Mortality ---------------------------------------------------------------
  resampled_men <- resampled_data %>% filter(gender=="man")
  resampled_women <- resampled_data %>% filter(gender=="woman")

# Men ---------------------------------------------------------------------
  # discrete-time models - probabilities of death / survival
  fit_age <-
    glm(dead ~ age + I(age ^ 2) + low + high ,
        data = resampled_men,
        family = binomial(link = "cloglog"))
  
  # predict px given the model 
  surv_low_m <- 1-predict(fit_age,data.frame(age=seq(50,110,2), low=1, high=0),type = "response")
  surv_mid_m <- 1- predict(fit_age,data.frame(age=seq(50,110,2), low=0, high=0),type = "response")
  surv_high_m <- 1- predict(fit_age,data.frame(age=seq(50,110,2), low=0, high=1),type = "response")
  
  # last age group - no immortality 
  surv_low_m[length(surv_low_m)] <- 0
  surv_mid_m[length(surv_mid_m)] <- 0
  surv_high_m[length(surv_high_m)] <- 0
  
  qx_low_m <- 1 - surv_low_m
  qx_mid_m <- 1 - surv_mid_m
  qx_high_m <- 1 - surv_high_m
  
  ### low edu -----------------------------------------------------------------
  
  LT_low_m <- sim_lt(eqx=qx_low_m, eta=age, n=2)
  LT_low_m <- LT_low_m %>% 
    filter(age<=86) %>% 
    mutate(Lx=ifelse(Lx==tail(Lx, 1), yes=Tx, no=Lx))
  
  ### mid edu -----------------------------------------------------------------
  
  LT_mid_m <- sim_lt(eqx=qx_mid_m, eta=age, n=2)
  LT_mid_m <- LT_mid_m %>% 
    filter(age<=86) %>% 
    mutate(Lx=ifelse(Lx==tail(Lx, 1), yes=Tx, no=Lx))

  ### high edu -----------------------------------------------------------------
  
  LT_high_m <- sim_lt(eqx=qx_high_m, eta=age, n=2)
  LT_high_m <- LT_high_m %>% 
    filter(age<=86) %>% 
    mutate(Lx=ifelse(Lx==tail(Lx, 1), yes=Tx, no=Lx))

  # Women ---------------------------------------------------------------------
  # discrete-time models - probabilities of death / survival
  fit_age <-
    glm(dead ~ age + I(age ^ 2) + low + high ,
        data = resampled_women,
        family = binomial(link = "cloglog"))
  
  # predict px given the model 
  surv_low_f <- 1-predict(fit_age,data.frame(age=seq(50,110,2), low=1, high=0),type = "response")
  surv_mid_f <- 1- predict(fit_age,data.frame(age=seq(50,110,2), low=0, high=0),type = "response")
  surv_high_f <- 1- predict(fit_age,data.frame(age=seq(50,110,2), low=0, high=1),type = "response")
  
  # last age group - no immortality 
  surv_low_f[length(surv_low_f)] <- 0
  surv_mid_f[length(surv_mid_f)] <- 0
  surv_high_f[length(surv_high_f)] <- 0
  
  qx_low_f <- 1 - surv_low_f
  qx_mid_f <- 1 - surv_mid_f
  qx_high_f <- 1 - surv_high_f
  
  ### low edu -----------------------------------------------------------------
  
  LT_low_f <- sim_lt(eqx=qx_low_f, eta=age, n=2)
  LT_low_f <- LT_low_f %>% 
    filter(age<=86) %>% 
    mutate(Lx=ifelse(Lx==tail(Lx, 1), yes=Tx, no=Lx))

  ### mid edu -----------------------------------------------------------------
  
  LT_mid_f <- sim_lt(eqx=qx_mid_f, eta=age, n=2)
  LT_mid_f <- LT_mid_f %>% 
    filter(age<=86) %>% 
    mutate(Lx=ifelse(Lx==tail(Lx, 1), yes=Tx, no=Lx))

  ### high edu -----------------------------------------------------------------
  
  LT_high_f <- sim_lt(eqx=qx_high_f, eta=age, n=2)
  LT_high_f <- LT_high_f %>% 
    filter(age<=86) %>% 
    mutate(Lx=ifelse(Lx==tail(Lx, 1), yes=Tx, no=Lx))

  # List of datasets you want to keep
  datasets_to_keep <- c("LT_low_m", "LT_mid_m", "LT_high_m",
                        "LT_low_f", "LT_mid_f", "LT_high_f",
                        "dta_prev", "cross_resample", "perform_ipw",
                        "fit_logistic_ipw", "generate_predicted_df", 
                        "exDF")
  
  all_objects <- ls()
  
  # Identify objects to remove
  objects_to_remove <- setdiff(all_objects, datasets_to_keep)
  
  # Remove unnecessary objects
  rm(list = objects_to_remove)
  
# 2. Prevalence --------------------------------------------------------------

  resampled_prev_dta <- cross_resample(dat=w5)

    threshold <- resampled_prev_dta %>%
    filter(age >= 50, age <= 69) %>% 
    group_by(country, gender) %>%
    summarise(
      #calculate weighted mean
      w_mean = weighted.mean(cog_value, w = wtresp),
      unw_mean = mean(cog_value),
      
      #calculate weighted variance
      w_var = wtd.var(cog_value, wtresp),
      
      #calculate weighted standard deviation
      w_sd = sqrt(w_var),
      
      # Calculate threshold 1.5 standard deviations below the mean
      threshold = w_mean - 1.5 * w_sd
    ) %>% 
    ungroup()
  
    prev <- full_join(resampled_prev_dta, threshold, by = c("country", "gender")) %>% 
      mutate(
        # prevalence estimated only among alive respondents
        cog_imp = ifelse(cog_value < threshold, 1, 0),
        low = ifelse(edu == "low", 1, 0),
        medium = ifelse(edu == "medium", 1, 0),
        high = ifelse(edu == "high", 1, 0))

  country_prev <- perform_ipw(prev, "Austria")
  country_prev <- country_prev %>% mutate(age_gr = factor(age_gr))
        
  fit_prev <- fit_logistic_ipw(country_prev)
  
  df_prev <- generate_predicted_df(country_prev, fit_prev, 
                                  c("man", "woman"), 
                                  c("low", "medium", "high"))
  
  wx_low_m <- df_prev %>% filter(gender == "man", edu == "low") %>% pull(predictions)
  wx_mid_m <- df_prev %>% filter(gender == "man", edu == "medium") %>% pull(predictions)
  wx_high_m <- df_prev %>% filter(gender == "man", edu == "high") %>% pull(predictions)
  
  wx_low_f <- df_prev %>% filter(gender == "woman", edu == "low") %>% pull(predictions)
  wx_mid_f <- df_prev %>% filter(gender == "woman", edu == "medium") %>% pull(predictions)
  wx_high_f <- df_prev %>% filter(gender == "woman", edu == "high") %>% pull(predictions)
  

# 3. Sullivan ----------------------------------------------------------------

## Men ---------------------------------------------------------------------
  LE_low_m <- LT_low_m$ex[LT_low_m$age == 50]
  CIFLE_low_m <- exDF(lx = LT_low_m$lx, wx = wx_low_m, Lx = LT_low_m$Lx)
  CILE_low_m <- LE_low_m - CIFLE_low_m
  
  LE_mid_m <- LT_mid_m$ex[LT_mid_m$age == 50]
  CIFLE_mid_m <- exDF(lx = LT_mid_m$lx, wx = wx_mid_m, Lx = LT_mid_m$Lx)
  CILE_mid_m <- LE_mid_m - CIFLE_mid_m
  
  LE_high_m <- LT_high_m$ex[LT_high_m$age == 50]
  CIFLE_high_m <- exDF(lx = LT_high_m$lx, wx = wx_high_m, Lx = LT_high_m$Lx)
  CILE_high_m <- LE_high_m - CIFLE_high_m
  
## Women ---------------------------------------------------------------------
  LE_low_f <- LT_low_f$ex[LT_low_f$age == 50]
  CIFLE_low_f <- exDF(lx = LT_low_f$lx, wx = wx_low_f, Lx = LT_low_f$Lx)
  CILE_low_f <- LE_low_f - CIFLE_low_f
  
  LE_mid_f <- LT_mid_f$ex[LT_mid_f$age == 50]
  CIFLE_mid_f <- exDF(lx = LT_mid_f$lx, wx = wx_mid_f, Lx = LT_mid_f$Lx)
  CILE_mid_f <- LE_mid_f - CIFLE_mid_f
  
  LE_high_f <- LT_high_f$ex[LT_high_f$age == 50]
  CIFLE_high_f <- exDF(lx = LT_high_f$lx, wx = wx_high_f, Lx = LT_high_f$Lx)
  CILE_high_f <- LE_high_f - CIFLE_high_f  
  
  
# Output --------------------------------------------------------------------
  
  # bind the results of each iteration in the object 
  results <- data.table(LE_low_m, CIFLE_low_m, CILE_low_m, 
                        LE_mid_m, CIFLE_mid_m, CILE_mid_m,
                        LE_high_m, CIFLE_high_m, CILE_high_m,
                        LE_low_f, CIFLE_low_f, CILE_low_f, 
                        LE_mid_f, CIFLE_mid_f, CILE_mid_f,
                        LE_high_f, CIFLE_high_f, CILE_high_f)
  results 
  
}
# keep results fixed between different runs, but different between iterations
, .options = furrr_options(seed = 1234)
)

output_dta <- rbindlist(df_start)
head(output_dta)
toc()

plan(sequential) # closing cores


# Confidence intervals ----------------------------------------------------
## Men ---------------------------------------------------------------------

# low
LE_low_m <- confidence_interval(output_dta$LE_low_m)
CIFLE_low_m <- confidence_interval(output_dta$CIFLE_low_m)
CILE_low_m <- confidence_interval(output_dta$CILE_low_m)


low_m <- as.data.frame(rbind(LE_low_m, CIFLE_low_m, CILE_low_m)) %>% 
  mutate(
    estimate = V1,
    lower = `2.5%`,
    upper = `97.5%`,
    country = "Austria", 
    gender = "Men",
    edu = "low",
    Expectancy = c("Total", "Cognitively Healthy", "Cognitively Impaired")
  ) %>% 
  select(country, gender, edu, Expectancy, estimate, lower, upper)

# mid
LE_mid_m <- confidence_interval(output_dta$LE_mid_m)
CIFLE_mid_m <- confidence_interval(output_dta$CIFLE_mid_m)
CILE_mid_m <- confidence_interval(output_dta$CILE_mid_m)

mid_m <- as.data.frame(rbind(LE_mid_m, CIFLE_mid_m, CILE_mid_m)) %>% 
  mutate(
    estimate = V1,
    lower = `2.5%`,
    upper = `97.5%`,
    country = "Austria", 
    gender = "Men",
    edu = "medium",
    Expectancy = c("Total", "Cognitively Healthy", "Cognitively Impaired")) %>% 
  select(country, gender, edu, Expectancy, estimate, lower, upper)

# high
LE_high_m <- confidence_interval(output_dta$LE_high_m)
CIFLE_high_m <- confidence_interval(output_dta$CIFLE_high_m)
CILE_high_m <- confidence_interval(output_dta$CILE_high_m)

high_m <- as.data.frame(rbind(LE_high_m, CIFLE_high_m, CILE_high_m)) %>% 
  mutate(
    estimate = V1,
    lower = `2.5%`,
    upper = `97.5%`,
    country = "Austria", 
    gender = "Men",
    edu = "high",
    Expectancy = c("Total", "Cognitively Healthy", "Cognitively Impaired")) %>% 
  select(country, gender, edu, Expectancy, estimate, lower, upper)


## Women ---------------------------------------------------------------------

# low
LE_low_f <- confidence_interval(output_dta$LE_low_f)
CIFLE_low_f <- confidence_interval(output_dta$CIFLE_low_f)
CILE_low_f <- confidence_interval(output_dta$CILE_low_f)


low_f <- as.data.frame(rbind(LE_low_f, CIFLE_low_f, CILE_low_f)) %>% 
  mutate(
    estimate = V1,
    lower = `2.5%`,
    upper = `97.5%`,
    country = "Austria", 
    gender = "Women",
    edu = "low",
    Expectancy = c("Total", "Cognitively Healthy", "Cognitively Impaired")
  ) %>% 
  select(country, gender, edu, Expectancy, estimate, lower, upper)

# mid
LE_mid_f <- confidence_interval(output_dta$LE_mid_f)
CIFLE_mid_f <- confidence_interval(output_dta$CIFLE_mid_f)
CILE_mid_f <- confidence_interval(output_dta$CILE_mid_f)

mid_f <- as.data.frame(rbind(LE_mid_f, CIFLE_mid_f, CILE_mid_f)) %>% 
  mutate(
    estimate = V1,
    lower = `2.5%`,
    upper = `97.5%`,
    country = "Austria", 
    gender = "Women",
    edu = "medium",
    Expectancy = c("Total", "Cognitively Healthy", "Cognitively Impaired")) %>% 
  select(country, gender, edu, Expectancy, estimate, lower, upper)

# high
LE_high_f <- confidence_interval(output_dta$LE_high_f)
CIFLE_high_f <- confidence_interval(output_dta$CIFLE_high_f)
CILE_high_f <- confidence_interval(output_dta$CILE_high_f)

high_f <- as.data.frame(rbind(LE_high_f, CIFLE_high_f, CILE_high_f)) %>% 
  mutate(
    estimate = V1,
    lower = `2.5%`,
    upper = `97.5%`,
    country = "Austria", 
    gender = "Women",
    edu = "high",
    Expectancy = c("Total", "Cognitively Healthy", "Cognitively Impaired")) %>% 
  select(country, gender, edu, Expectancy, estimate, lower, upper)


## Combine & save -----------------------------------------------------------------

AUT <- as.data.frame(rbind(low_m, mid_m, high_m, low_f, mid_f, high_f))
head(AUT)


write.csv(AUT, "Results/AUT.csv", row.names = FALSE)






