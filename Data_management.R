# Author: Donata Stonkute
#########################
# This analysis uses data or information from the Harmonized Survey of Health, 
# Ageing and Retirement in Europe dataset and Codebook, Version F as of February 
# 2023 developed by the Gateway to Global Aging Data. The development of the 
# Harmonized Survey of Health, Ageing and Retirement in Europe was funded by the National Institute on Aging 
# (R01 AG030153, RC2 AG036619, 1R03AG043052). For more information, please refer to www.g2aging.org
#########################

Sys.setenv(LANG = "en")
library(tidyverse)
library(data.table)
library(labelled)
library(haven)
# Set working directory
setwd("your_directory")

# Read the Stata file 
## For prior steps in preparation of the data, please go to Stata file "w1-8_keep.do"
mydt_wide <- read_dta("downloaded_data.dta")

# I create this for easier identification of column numbers for variables of interest
col.names <- data.frame(colnames(mydt_wide))


# save variables that are not time-varying
constant_vars <- mydt_wide[,c(1,2,3,26,27,35,36)]

# Age ---------------------------------------------------------------------
# This function helps to identify which columns contain a character string of interest
which(grepl("agey", col.names$colnames.mydt_wide.))

# We retrieve those next to the unique merge ID 
mydt_age <- mydt_wide[,c(1, 28:34)]

# And convert sub-data from wide to long format
mydt_age <- mydt_age %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "age",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, age)

# Next, we add the converted sub-data to the constant variables 
mydt_long <- merge(constant_vars, mydt_age, by="mergeid", all=TRUE)

remove(constant_vars)  
remove(mydt_age)

# Now we will repeat the same steps for all variables of interest


# Interview status -----------------------------------------------------
which(grepl("iwstat", col.names$colnames.mydt_wide.))

mydt_iwstat <- mydt_wide[,c(1, 4:11)]

mydt_iwstat <- mydt_iwstat %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "iwstat",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, iwstat)

mydt_long <- merge(mydt_long, mydt_iwstat, by=c("mergeid","wave"), all=TRUE)

remove(mydt_iwstat)

# Person-level analysis weights -------------------------------------------
which(grepl("wtresp", col.names$colnames.mydt_wide.))
mydt_tresp <- mydt_wide [,c(1, 12:18)]

mydt_tresp <- mydt_tresp %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "wtresp",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, wtresp)

mydt_long <- merge(mydt_long, mydt_tresp, by=c("mergeid","wave"), all=TRUE)

remove(mydt_tresp)


# Proxy -------------------------------------------
which(grepl("proxy", col.names$colnames.mydt_wide.))
mydt_proxy <- mydt_wide [,c(1, 19:25)]

mydt_proxy <- mydt_proxy %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "proxy",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, proxy)

mydt_long <- merge(mydt_long, mydt_proxy, by=c("mergeid","wave"), all=TRUE)

remove(mydt_proxy)

# Self-rated memory -------------------------------------------
which(grepl("slfmem", col.names$colnames.mydt_wide.))
mydt_memory <- mydt_wide [,c(1, 45:49)]

mydt_memory <- mydt_memory %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "slfmem",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, slfmem)

mydt_long <- merge(mydt_long, mydt_memory, by=c("mergeid","wave"), all=TRUE)

remove(mydt_memory)

# Immediate recall -------------------------------------------
which(grepl("imrc", col.names$colnames.mydt_wide.))
mydt_imrc <- mydt_wide [,c(1, 50:56)]

mydt_imrc <- mydt_imrc %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "imrc",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, imrc)

mydt_long <- merge(mydt_long, mydt_imrc, by=c("mergeid","wave"), all=TRUE)

remove(mydt_imrc)


# Delayed recall -------------------------------------------
which(grepl("dlrc", col.names$colnames.mydt_wide.))
mydt_dlrc <- mydt_wide [,c(1, 57:63)]

mydt_dlrc <- mydt_dlrc %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "dlrc",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, dlrc)

mydt_long <- merge(mydt_long, mydt_dlrc, by=c("mergeid","wave"), all=TRUE)

remove(mydt_dlrc)

# Serial 7's -------------------------------------------
which(grepl("ser7", col.names$colnames.mydt_wide.))
mydt_ser7 <- mydt_wide [,c(1, 64:68)]

mydt_ser7 <- mydt_ser7 %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "ser7",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, ser7)

mydt_long <- merge(mydt_long, mydt_ser7, by=c("mergeid","wave"), all=TRUE)

remove(mydt_ser7)


# Orientation -------------------------------------------
which(grepl("orient", col.names$colnames.mydt_wide.))
mydt_orient <- mydt_wide [,c(1, 69:75)]

mydt_orient <- mydt_orient %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "orient",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, orient)

mydt_long <- merge(mydt_long, mydt_orient, by=c("mergeid","wave"), all=TRUE)

remove(mydt_orient)

# Verbal fluency -------------------------------------------
which(grepl("verbf", col.names$colnames.mydt_wide.))
mydt_verbf <- mydt_wide [,c(1, 76:82)]

mydt_verbf <- mydt_verbf %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "verbf",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, verbf)

mydt_long <- merge(mydt_long, mydt_verbf, by=c("mergeid","wave"), all=TRUE)

remove(mydt_verbf)

# Numeric fluency -------------------------------------------
which(grepl("numer_s", col.names$colnames.mydt_wide.))
mydt_numer_s <- mydt_wide [,c(1, 83:89)]

mydt_numer_s <- mydt_numer_s %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "numer_s",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, numer_s)

mydt_long <- merge(mydt_long, mydt_numer_s, by=c("mergeid","wave"), all=TRUE)

remove(mydt_numer_s)

# Recall summary total -------------------------------------------
which(grepl("tr20", col.names$colnames.mydt_wide.))
mydt_tr20 <- mydt_wide [,c(1, 90:96)]

mydt_tr20 <- mydt_tr20 %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "tr20",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, tr20)

mydt_long <- merge(mydt_long, mydt_tr20, by=c("mergeid","wave"), all=TRUE)

remove(mydt_tr20)

# Cognition testing conditions -------------------------------------------
which(grepl("cogimp", col.names$colnames.mydt_wide.))
mydt_cogimp <- mydt_wide [,c(1, 97:103)]

mydt_cogimp <- mydt_cogimp %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "cogimp",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, cogimp)

mydt_long <- merge(mydt_long, mydt_cogimp, by=c("mergeid","wave"), all=TRUE)

remove(mydt_cogimp)

# Other people presence  -------------------------------------------
which(grepl("cogothp", col.names$colnames.mydt_wide.))
mydt_cogothp <- mydt_wide [,c(1, 104:110)]

mydt_cogothp <- mydt_cogothp %>%
  pivot_longer(
    cols = starts_with("r"),
    names_to = "wave_v1",
    names_prefix = "r",
    values_to = "cogothp",
    values_drop_na = FALSE
  ) %>%
  mutate(wave = 
           substr(wave_v1,1,1)) %>%
  select(mergeid, wave, cogothp)

mydt_long <- merge(mydt_long, mydt_cogothp, by=c("mergeid","wave"), all=TRUE)

remove(mydt_cogothp)


# Cosmetics ---------------------------------------------------------------

## Sorting
mydt_long <- mydt_long[order(mydt_long$mergeid, mydt_long$wave),]

mydt_long$country <-    recode(
  mydt_long$country,
  "11" = "Austria",
  "12" = "Germany",
  "13" = "Sweden",
  "14" = "Netherlands",
  "15" = "Spain",
  "16" = "Italy",
  "17" = "France",
  "18" = "Denmark",
  "19" = "Greece",
  "20" = "Switzerland",
  "23" = "Belgium",
  "25" = "Israel",
  "28" = "Czechia",
  "29" = "Poland",
  "30" = "Ireland",
  "31" = "Luxembourg",
  "32" = "Hungary",
  "33" = "Portugal",
  "34" = "Slovenia",
  "35" = "Estonia",
  "47" = "Croatia",
  "48" = "Lithuania",
  "51" = "Bulgaria",
  "53" = "Cyprus",
  "55" = "Finland",
  "57" = "Latvia",
  "59" = "Malta",
  "61" = "Romania",
  "63" = "Slovakia"
)


# Country selection -------------------------------------------------------

mydt_long <- mydt_long %>% 
  filter(country %in% c("Austria", "Belgium", 'Czechia', "Denmark", 
                        "Estonia", "Spain", "France", "Italy", "Sweden", "Slovenia"))

# Interview status --------------------------------------------------------

dt_attrition <- mydt_long %>% 
  group_by(mergeid) %>% 
  mutate(iwstat_lag = lead(iwstat)) %>% 
  ungroup() %>% 
  mutate(drop_outs = ifelse(iwstat_lag == 4 | iwstat_lag == 9, 1, 0),
         drop_outs = ifelse(is.na(drop_outs), 0, drop_outs))


# excluding not yet identified (=0), alive but did not respond (=4), 
# died before last wave (=6), not known (=9)
dt_present <- dt_attrition %>% 
  filter(iwstat %in% c(1,5)) 
nrow(dt_present)
nrow(mydt_long)-nrow(dt_present)


# Alive -------------------------------------------------------------------

## excluding respondents who died between the last interview and the current one
dt_alive <- dt_present %>% 
  filter(iwstat==1) 
nrow(dt_alive)
nrow(dt_present)-nrow(dt_alive)

table1 <- table(dt_alive$country, dt_alive$wave)
table1

length(unique(dt_alive$mergeid))


# Adding death indicator for survival analysis ----------------------------

table(dt_alive$country, dt_alive$radyear)

data.table::setDT(dt_alive)[,dead:=0];
dt_alive[.(dt_alive[!is.na(radyear),
                          unique(mergeid)]),
            on=.(mergeid),
            mult="last", dead:=1]


# Baseline indicator ------------------------------------------------------

data.table::setDT(dt_alive)[,basel:=0];
dt_alive[.(dt_alive[,unique(mergeid)]),
            on=.(mergeid),
            mult="first", basel:=1]

table2 <- table(dt_alive$country)
table2


# Data frame for analysis, renaming variables -----------------------------

unique(dt_alive$ragender)
unique(dt_alive$raeducl)

dt_alive <- dt_alive %>%
  mutate(
    gender = ifelse(ragender == 1, "man", "woman"),
    edu = case_when(raeducl == 1 ~ "low",
                    raeducl == 2 ~ "medium",
                    raeducl == 3 ~ "high"),
    verbf_bi = ifelse(verbf<=15, 0, 1) # dichotomised verbal fluency
  ) 



## COVID-19 period effects ----------------------------------------------------

# ## Reversing deaths observed after wave 8 
## we pretend going back in time when we did not have this data
dtt <- dt_alive %>%
  mutate(covid_dd = ifelse(radyear %in% c(2020, 2021), 
                           yes=1, no=0),
         dead = ifelse(covid_dd==1, 
                       yes=0, no=dead),
         radyear = ifelse(covid_dd==1, 
                          yes=NA, no=radyear), 
         mirtys = 0)


table(dtt$country, dtt$radyear)
table(dtt$dead, dtt$wave)


## Add deaths --------------------------------------------------------------

dd <-
  dtt %>% arrange(mergeid, desc(wave)) %>%
  filter(!is.na(radyear)) %>%
  group_by(mergeid) %>%
  slice_head() %>%
  mutate(wave = as.numeric(wave) + 1,
         age = age + 2,
         mirtys = 99) %>%
  ungroup()

table(dd$wave,dd$country)

dat_dd <- rbind(dtt,dd)
nrow(dat_dd)-nrow(dtt) # number of deaths


# clean death records
duomenys <- dat_dd %>% arrange(mergeid, wave) %>%
  mutate(dead = ifelse(mirtys == 99, 1, 0)) 

# Analytical sample formatting --------------------------------------------

duomenys <- duomenys %>%
  mutate(
    low = ifelse(edu == "low", 1, 0),
    medium = ifelse(edu == "medium", 1, 0),
    high = ifelse(edu == "high", 1, 0),
    edu = factor(edu, levels = c("low", "medium", "high")), 
    year = case_when(
      wave == 1 ~ 2004,
      wave == 2 ~ 2007,
      wave == 3 ~ 2009,
      wave == 4 ~ 2011,
      wave == 5 ~ 2013,
      wave == 6 ~ 2015,
      wave == 7 ~ 2017,
      wave == 8 ~ 2019
    )
  ) 

dtw <- duomenys %>%
  group_by(mergeid) %>%
  mutate(
    wave = as.numeric(wave),
    wave_lead = lead(wave),
    wave_diff = wave_lead - wave) %>%
  ungroup() 


## Non-consecutive waves ---------------------------------------------------

dtf <- dtw # creating a temp df in order not to overwrite

# indicator that tells if difference between waves is more than 1 (non-consecutive)
setDT(dtf)[, grp := cumsum(c(0, diff(wave)) > 1), by = mergeid] 

dtf[, ID := .GRP, by = .(mergeid, grp)]
dtf[, time := .N, by = ID]

# dtf %>% select(mergeid,wave,wave_lead,wave_diff, ID, grp, time,radyear) %>% View()
table(dtf$dead, dtf$time)


# remove both non-consecutive observations and those that participated only in one wave
dat <- as.data.frame(dtf) %>% 
  filter(time>1)
table(dat$country, dat$wave)
nrow(dat)
nrow(dtf)-nrow(dat)

excluded <- dtf %>% anti_join(dat)
length(unique(excluded$mergeid))

# this assures that the only information taken from wave 8 is the deaths (recorded retrosp. in wave 7)
dati <-
  dat %>% mutate(lastw = ifelse(wave > 7 &
                                  dead == 0, 1, 0)) %>% filter(lastw == 0, wave != 9)
table(dati$wave)
head(dati)

nrow(dati)
nrow(dati)-nrow(dat)


dati <- dati %>% 
  select(mergeid, wave, year, country, radyear, dead, 
         gender, age, edu, low, medium, high, 
         ser7, tr20, 
         cogimp, cogothp, proxy, wtresp, drop_outs)

# Age eligibility -------------------------------------------------------

final_mort <- dati %>% 
  filter(age>=50) 
nrow(final_mort)
nrow(dati)-nrow(final_mort)

length(unique(final_mort$mergeid))
write.csv(final_mort, "mortality_sample.csv", row.names = FALSE)



# Omitting missing values -------------------------------------------------

install.packages("naniar")
library(naniar)
install.packages("ComplexUpset")
library(ComplexUpset)

check <- dati %>% 
  mutate(
    proxy = case_when(proxy %in% c(1, 2) ~ NA_real_, TRUE ~ proxy),  # Treat proxy == 1 or 2 as NA
    age = case_when(age < 50 ~ NA_real_, TRUE ~ age)  # Treat age < 50 as NA
  ) %>%
  select(-c(radyear, ser7,cogimp, cogothp, tr20))

check %>% 
  gg_miss_upset() 

dt_full <- dati %>% 
  filter(!is.na(gender) &
           !is.na(edu) &
           !is.na(age))
nrow(dt_full)
nrow(dati)-nrow(dt_full)


# write.csv(dt_full, "analytical_sample.csv", row.names = FALSE)
# write.csv(dt_complete, "prevalence_dta.csv", row.names = FALSE)


# Additional edits --------------------------------------------------------

dat <- read.csv("analytical_sample.csv")
library(tidyverse)
library(purrr)

colnames(dat)

# Create the 'q_num' column, indicating the number of tests taken
your_data <- dt_alive %>%
  filter(wave < 6) %>%
  group_by(mergeid) %>%
  mutate(
    q_num = 1,
    PE = cumsum(q_num),
    test = ifelse(PE == 1, 0, 1)
  ) %>%
  ungroup()  

# Now, for the 'cog_imp' variable
# Filter the data for alive individuals in wave 4 and calculate mean and standard deviation for the relevant age group
w5 <- your_data %>%
  filter(
    wave==5 
         )
library(UpSetR)

# Prepare the dataset with relevant columns (exclude "Presence of other people")
w5_upset <- w5 %>%
  select(
    proxy, cogimp, ser7, tr20  # Exclude "Presence of other people"
  ) %>%
  mutate(
    proxy = ifelse(proxy %in% c(1, 2), NA, proxy),
    cogimp = ifelse(cogimp == 1, NA, cogimp)
  ) %>%
  mutate(across(everything(), ~ as.integer(is.na(.))))  # Convert NA to 1, others to 0

check_missingness <- w5 %>%
  select(
    proxy, cogimp, ser7, tr20  # Exclude "Presence of other people"
  ) %>%
  mutate(
    proxy = ifelse(proxy %in% c(1, 2), NA, proxy),
    cogimp = ifelse(cogimp == 1, NA, cogimp)
  ) %>% 
  filter(
    is.na(proxy) | is.na(cogimp) | is.na(ser7) | is.na(tr20)   # Check for missing in any variable
  ) %>%
  nrow() 


# Rename columns for readability
w5_upset <- w5_upset %>%
  rename(
    "Proxy response" = proxy,
    "Test conditions impairment" = cogimp,
    "Missing Serial 7s test" = ser7,
    "Missing Recall tests" = tr20
  ) %>%
  as.data.frame()  # Ensure it's a dataframe

# Verify column names to ensure the correct selection
print(colnames(w5_upset))

# Save as JPEG
jpeg("U:/A3_CHE/Figs/Exclusion_w5.jpg", width = 1200, height = 800, res = 150)
upset(
  w5_upset,
  sets = colnames(w5_upset),
  sets.bar.color = "steelblue",
  main.bar.color = "steelblue",
  mainbar.y.label = "Number of Missing Cases",
  sets.x.label = "Missing Variables",
  order.by = "freq"
)
dev.off()  # Close the file


w5_filtered <- w5 %>%
  filter(
    !is.na(tr20), 
    !is.na(ser7), 
    proxy == 0,
    cogimp != 1)

w5_filtered <- w5_filtered %>% 
  filter(!is.na(wtresp))


# weighted mean and threshold ---------------------------------------------
library(Hmisc)


result <- w5_filtered %>%
  filter(age %in% 50:69) %>% 
  mutate(cog_value= tr20 + ser7) %>% 
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

#### ---- try finding weighted SD function ----

write.csv(result, "1.5SD by country_gender.csv")

# Create the 'cog_imp' variable using the calculated value
dat2 <- full_join(w5_filtered, result, by = c("country", "gender")) %>% 
  mutate(
    cog_value = tr20 + ser7,
    # prevalence estimated only among alive respondents
    cog_imp = ifelse(dead == 0 & cog_value < threshold, 1, 0),
    low = ifelse(edu == "low", 1, 0),
    medium = ifelse(edu == "medium", 1, 0),
    high = ifelse(edu == "high", 1, 0),
    edu = factor(edu, levels = c("low", "medium", "high")), 
  ) %>% 
  select(c(mergeid, wave, country, radyear, dead,
           gender, age, edu, low, medium, high,  
           tr20, ser7, cog_value, cog_imp, 
           wtresp, q_num, PE, test))

check <- dat2 %>% filter(is.na(wtresp))

write.csv(dat2, "prevalence.csv", row.names = FALSE)



















