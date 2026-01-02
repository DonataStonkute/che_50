# 2024 March
# FUNCTIONS AND INFORMATION FOR CHE paper
# SHARE 

# re-sampling for bootstraps
## data needs to be in data.table format
long_resample <- function(dat, id, time) {
  
  id = substitute(id)
  time = substitute(time)
  
  tmp <- split(dat, by = id)
  
  idx <- unique(dat[[id]])
  
  resample_id <- sample(idx, length(idx), replace = TRUE)
  
  resample_dat <- lapply(seq_along(resample_id), function(x) {
    res_dat <- tmp[[as.character(resample_id[x])]]
    res_dat$new_id <- x
    return(res_dat)
  })
  
  resample_dat <- do.call(rbind, resample_dat)
  
  resample_dat
}


cross_resample <- function(dat) {
  n <- nrow(dat)
  
  # Resample rows with replacement
  sample_indices <- sample(1:n, n, replace = TRUE)
  
  # Create resampled dataset
  resample_dat <- dat[sample_indices, , drop = FALSE]
  
  return(resample_dat)
}

# life table simulation
sim_lt <- function(eqx,eta,n){
  
# period life table starting quantities  
  dat <- data.frame(
    age = eta,
    qx = eqx,
    nr = length(eqx),
    ax = c(rep(n/2,length(eta)-1), NA),
    lx = c(500000,rep(0,(length(eta)-1))),
    px = 1 - eqx,
    Lx = NA)
  
# simulate lifetimes (elimination by death)  
  for (i in 1:(nrow(dat)-1)) {
    ## draw random prob for each individual from the cohort
    threshold <- runif(dat$lx[i])
    ## compare with age-spec prob from the models
    lx.test <- as.numeric(dat$qx[i] > threshold)
    ## survived to the next age interval is the subtraction of deaths
    dat$lx[i + 1] <- dat$lx[i] - sum(lx.test)
    ## Number of person-years lived between ages x and x+n
    dat$Lx[i] <- n*(dat$lx[i + 1]) + dat$ax[i] * sum(lx.test)
    
  }  
  
  dat$Lx[nrow(dat)] <- 0
  dat$Tx <-  rev(cumsum(rev(dat$Lx)))
  dat$ex <-  dat$Tx/dat$lx
  
  
  return(dat)
  
}

# function to convert LT by 2years to by 4years
# abridge_LT_4y <- function(input_data) {
#   result <- input_data %>%
#     mutate(age_4 = case_when(
#       age %% 4 == 0 ~ age - 2,
#       TRUE ~ age + (2 - age %% 4)
#     )) %>%
#     group_by(age_4) %>%
#     slice_min(order_by = age) %>% 
#     ungroup() %>%
#     mutate(Lx = Tx - lead(Tx), 
#            dx = lx - lead(lx), 
#            qx = dx / lx) %>%
#     select(age_4, lx, Lx, Tx) %>% 
#     filter(age_4 <= 86) %>%
#     mutate(Lx = ifelse(Lx == tail(Lx, 1), yes = Tx, no = Lx),
#            Lx = ifelse(Lx == tail(Lx, 1), Tx, Lx),
#            Tx = rev(cumsum(rev(Lx))),
#            ex = Tx/lx)
#   
#   return(result)
# }


# Function to perform inverse probability weighting
perform_ipw <- function(data, country_name) {
  country_data <- data %>% filter(country == country_name)
  
  fit_ipw <- glm(test ~ age_gr + gender*edu, data = country_data, family = binomial)
  country_data$PS <- predict(fit_ipw, type = "response")
  country_data <- country_data %>% mutate(IPW = ifelse(test == 1, 1/PS, 1/(1-PS)))
  
  return(country_data)
}


# Function to fit logistic regression with IPW
fit_logistic_ipw <- function(data) {
  fit <- glm(
    cog_imp ~ age_gr + gender * edu,
    data = data,
    family = binomial,
    weights = IPW * (wtresp / sum(wtresp))
  )
  return(fit)
}


# Function to generate predicted dataframe
generate_predicted_df <- function(country_data, fit_model, gender_labels, edu_labels) {
  # Predictions for each gender and education level
  predictions <- list()
  for (gender_label in gender_labels) {
    for (edu_label in edu_labels) {
      predictions[[paste(gender_label, edu_label, sep="_")]] <- predict(
        fit_model,
        newdata = data.frame(
          gender = gender_label,
          edu = edu_label,
          age_gr = as.factor(seq(50, 86, 2))
        ),
        type = "response"
      )
    }
  }
  

# Create a dataframe for predicted values
  predicted_df <- data.frame(
    country = unique(country_data$country),
    gender = c(rep("man", 57), rep("woman", 57)),  
    edu = rep(c(rep("low", 19), rep("medium", 19), rep("high", 19)), times = 2),  
    age_gr = rep(as.factor(seq(50, 86, 2)), times = 6)
  )
  
  # Add predicted values to the dataframe
  predicted_df$predictions <- unlist(predictions)
  
  return(predicted_df)
}


# DFLE formula, given Sullivan method
exDF <- function(lx, wx, Lx){
  dfle <- 1/lx[1]*sum((1-wx)*Lx)
  return(dfle)
}


# confidence intervals 
confidence_interval <- function(vector) {
  estimate <- mean(vector)
  ci <- quantile(vector, probs = c(.025,.975),type=8)
  result <- c(estimate, ci)
  return(result)
}


# data handling
reshape_data <- function(df, country_name, df_long_name) {
  df_long <- df %>%
    pivot_longer(
      cols = everything(), 
      names_to = "variable", 
      values_to = "estimate"
    ) %>%
    mutate(
      type = gsub("_.*", "", variable),                # Extract type (capitalized letters)
      edu = case_when(                                    # Extract education level
        grepl("low", variable) ~ "low",
        grepl("mid", variable) ~ "medium",
        grepl("high", variable) ~ "high",
        TRUE ~ NA_character_
      ),
      gender = case_when(                                 # Extract gender
        grepl("_m$", variable) ~ "m",
        grepl("_f$", variable) ~ "f",
        TRUE ~ NA_character_
      ),
      country = country_name                             # Assign country name
    ) %>%
    select(country, gender, edu, type, estimate)         # Select relevant columns
  
  assign(df_long_name, df_long, envir = .GlobalEnv)
}

