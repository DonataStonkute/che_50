# Visualization -----------------------------------------------------------
Sys.setenv(LANG = "en")

rm(list=ls())

getwd()
setwd("your_directory")
dat <- read.csv("Results/combined_country_results.csv")

## libraries
library(tidyverse)
library(tidyr)
library(viridis)
library(forcats)


## ggplot parameters
theme_graph <- function (base_size = 12, base_family = "sans") {
  theme(plot.title = element_text(size = 14, face = "bold", hjust=0.5, margin = margin(20, 0, 5, 0)),
        plot.subtitle = element_text(colour = "#000000", size = 12,hjust=0.5, margin = margin(0, 0, 10, 0)),
        plot.caption = element_text(colour = "#000000", size = 10, hjust=1, margin = margin(10, 0, 20, 0)),
        plot.background = element_rect(fill = "#FFFFFF"), 
        panel.background = element_rect(fill = "#FFFFFF", colour = "#000000", linetype = "solid"), 
        panel.grid.major.x = element_line(colour = "#C9C9C9", linetype = "dotted"),
        panel.grid.major.y = element_line(colour = "#C9C9C9", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14, colour = "#000000", hjust=0.5, face = "bold", margin = margin(10, 0, 10, 0)), 
        axis.title.y = element_blank(), axis.text = element_text(size = 14, colour = "#000000"),
        axis.line.y = element_line(colour = "#000000"),
        axis.line.x = element_line(colour = "#000000"),
        axis.ticks = element_line(colour = "#000000", size = 1),   
        strip.text.x = element_text(size = 12),
        legend.text = element_text(size = 14, colour = "#000000"),
        legend.background = element_rect(fill = "#FFFFFF", colour = "white", size = 0.3, linetype = "blank"), 
        legend.key = element_rect(fill = NA, linetype = "blank"), 
        legend.position = "bottom",
        legend.direction = "horizontal")
}

## order data
dat <- dat %>% 
  mutate(
    country = factor(country, levels = c("Czechia", "Estonia", "Slovenia",
                                              "Austria", "Belgium", "Denmark",
                                              "Sweden", "France", "Italy", "Spain")),
         Education = factor(edu, levels = c("low", "medium", "high")), 
         Gender = gender)
unique(dat$Expectancy)

CIFLE <- dat %>% filter(type=="CIFLE")
LE <- dat %>%   filter(type=="LE") 
CILE <- dat %>%   filter(type=="CILE") 


head(CIFLE)
# Main Plots --------------------------------------------------------------


## CIFLE -------------------------------------------------------------------

###---- CIFLE at 50 by edu 

min <- min(CIFLE$lower)
max <- max(CIFLE$upper)


dfle_plot <- ggplot(CIFLE, aes(x = p_est, y = country, color = Education)) +
  geom_point(size = 4, shape = 16, alpha = 0.8) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2, size = 0.9) +
  scale_y_discrete(limits = rev(levels(CIFLE$country)), name = "Country") +
  labs(x = "Years", y = NULL, color = "Education Level") +
  scale_x_continuous(limits = range(CIFLE$lower, CIFLE$upper), breaks = seq(16, 44, 4)) +
  facet_grid(. ~ Gender) +
  theme_minimal(base_size = 12) +  # Adjusted base size
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 10, face = "bold"),  # Smaller legend title
    legend.text = element_text(size = 9),  # Reduce legend text size
    legend.key.size = unit(0.8, "lines"),  # Smaller legend key size
    legend.box.margin = margin(-5, 0, 0, 0),  
    legend.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold"), 
    axis.title.y = element_blank(), # Adjust facet titles
    legend.position = "bottom"
  ) +
  scale_color_viridis_d(option = "C", end = 0.8)

dfle_plot




library(dplyr)
library(forcats)

# Step 1: Calculate the education gap (High - Low) for each country and gender
edu_gap <- CIFLE %>%
  filter(Education %in% c("low", "high")) %>%
  select(country, Gender, Education, p_est) %>%
  pivot_wider(names_from = Education, values_from = p_est) %>%
  mutate(gap = high - low)

# Step 2: Reorder countries by gap (descending) within each gender
CIFLE <- CIFLE %>%
  left_join(edu_gap %>% select(country, Gender, gap), by = c("country", "Gender")) %>%
  group_by(Gender) %>%
  mutate(country = fct_reorder(country, gap, .desc = TRUE)) %>%
  ungroup()


ggsave("Figs/cifle_by_edu.png", dpi = 350, width = 2244 / 350, height = 1260 / 350, units = "in")

###---- difference between high and low

Diff <- CIFLE %>% 
  select(country, gender, Education, p_est) %>% 
  pivot_wider(values_from = "p_est", names_from = "Education") %>% 
  mutate(Edu_gap = high-low, 
         Gender = factor(gender, levels = c("Women", "Men")),
         country = factor(country, levels = c("Estonia", "Czechia", "Slovenia", 
                                              "Austria", "Belgium",  "Denmark", 
                                              "Sweden", "France", "Spain", "Italy")))

diff_plot <- ggplot(Diff) +
  geom_bar(aes(x = country, y = Edu_gap, fill = gender), 
                      stat = "identity", position = position_dodge(), width = 0.8) + 
  coord_flip(ylim=c(0,20)) +
  theme(panel.spacing = unit(2, "lines"), 
        legend.title = element_text(size = 18)) +
  # facet_wrap(.~gender, nrow = 1) +
  ylab("Years") +
  theme_graph() +
  scale_fill_viridis(discrete = T, option = "C", end = 0.8, direction = 1) 

diff_plot

ggsave("Figs/edu_diff_by_gender.png", dpi = 350)

###---- CIFLE at 50 by sex

min <- min(CIFLE$lower)
max <- max(CIFLE$upper)

ggplot(CIFLE,aes(x = p_est, y = country, color=Gender)) +
  geom_point(size=3.5)+
  geom_errorbar(aes(xmin=lower, xmax=upper), width=0.4, size = 0.6) +
  scale_y_discrete(limits=rev,name= "Country")+
  labs(x="Years")+
  scale_x_continuous(
    limits = c(min, max),
    breaks = c(seq(16, 44, 8))) +
  facet_grid(.~Education) +
  theme_graph() +
  # theme(strip.placement = "outside") +
  scale_color_viridis(discrete = T, option = "C", end = 0.8)  

ggsave("Figs/cifle_by_gender.png", dpi = 350)


## CILE --------------------------------------------------------------------

###---- CILE at 50 by edu 

min <- min(CILE$lower)
max <- max(CILE$upper)

dle_plot <- ggplot(CILE,aes(x = p_est, y = country, color=Education)) +
  geom_point(size=3.5)+
  geom_errorbar(aes(xmin=lower, xmax=upper), width=0.3, size = 0.6) +
  scale_y_discrete(limits=rev,name= "Country")+
  labs(x="Years")+
  scale_x_continuous(
    limits = c(min, max),
    breaks = c(seq(0, 15, 5))) +
  facet_grid(.~Gender) +
  theme_graph() +
  theme(legend.box.margin=margin(-15,0,0,0),
        legend.background = element_rect(linetype = "blank"),
        legend.title=element_text(size=16)
  ) +
  scale_color_viridis(discrete = T, option = "C", end = 0.8)  

dle_plot

ggsave("Figs/cile_by_edu.png", dpi = 350)


## LE  ---------------------------------------------------------------------

###---- LE at 50 by edu
min <- min(LE$lower)
max <- max(LE$upper)

ggplot(LE,aes(x = p_est, y = country, color=Education)) +
  geom_point(size=3.5)+
  geom_errorbar(aes(xmin=lower, xmax=upper), width=0.4, size = 0.5) +
  scale_y_discrete(limits=rev,name= "Country")+
  labs(
       x="Years")+
  facet_grid(.~Gender) +
  scale_x_continuous(
    limits = c(min, max),
    breaks = c(seq(22, 46, 4)),
    sec.axis = dup_axis(name = "")) +
  theme_graph() +
  theme(strip.placement = "outside") +
  scale_color_viridis(discrete = T, option = "C", end = 0.8) 

ggsave("Figs/LE_by_edu.png", dpi = 350)


###---- by sex
ggplot(LE,aes(x = p_est, y = country, color=Gender)) +
  geom_point(size=3.5)+
  geom_errorbar(aes(xmin=lower, xmax=upper), width=0.4, size = 0.6) +
  scale_y_discrete(limits=rev,name= "Country")+
  scale_x_continuous(
    limits = c(min, max),
    breaks = c(seq(22, 46, 8))) +
  facet_grid(.~Education) +
  theme_graph() +
  xlab("Years") +
  theme(axis.title.y = element_blank(),
        strip.text.x = element_text(size = 14)) +
  scale_color_viridis(discrete = T, option = "C", end = 0.8) 

ggsave("Figs/LE_by_sex_edu.png")



# Relative ----------------------------------------------------------------


# Share of life spent disabled

prop_dat <- dat %>% 
  group_by(country, Gender, edu) %>% 
  mutate(disab = lead(p_est), 
         prop_healthy = disab/p_est*100) %>% 
  ungroup() %>% 
  filter(type == "LE") %>% 
  select(country, Gender, edu, prop_healthy)
  
prop_dat <- prop_dat %>% 
  mutate(Gender = factor(Gender, levels = c("Men", "Women")),
         country = factor(country, levels = c("Czechia", "Estonia", "Slovenia",
                                              "Austria", "Belgium", "Denmark",
                                              "Sweden", "France", "Italy", "Spain")),
         Education = factor(edu, levels = c("low", "medium", "high")))

prop_wide <- prop_dat %>% 
  select(-edu) %>% 
  pivot_wider(names_from = "Education", values_from = "prop_healthy") %>% 
  mutate(prcnt_diff = high - low)
  
  
write.csv(prop_dat, "Results/prop_CIFLE.csv", row.names = FALSE)

prop_plot <- ggplot(prop_dat) +
  geom_bar(aes(x = country, y = prop_healthy, fill = Education), 
           stat = "identity", position = position_dodge(), width = 0.8) + 
  scale_x_discrete(limits=rev) +
  coord_flip(ylim=c(50,100)) +
  facet_wrap(.~Gender, nrow = 1) +
  ylab("%") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),  # Smaller legend title
    legend.text = element_text(size = 9),  # Reduce legend text size
    legend.key.size = unit(0.8, "lines"),  # Smaller legend key size
    legend.box.margin = margin(-5, 0, 0, 0),  
    legend.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold"), 
    axis.title.y = element_blank()# Adjust facet titles
  ) +
  scale_fill_viridis(discrete = TRUE, option = "C", end = 0.8, direction = 1) 


prop_plot
ggsave("Figs/prop_of_LE_edu.png",  dpi = 350, width = 2244 / 350, height = 1260 / 350, units = "in")


library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(viridis)

# Step 1: Prepare prop_healthy
prop_dat <- dat %>% 
  group_by(country, Gender, edu) %>% 
  mutate(disab = lead(p_est), 
         prop_healthy = disab / p_est * 100) %>% 
  ungroup() %>% 
  filter(type == "LE") %>% 
  select(country, Gender, edu, prop_healthy)

# Step 2: Set factor levels
prop_dat <- prop_dat %>% 
  mutate(
    Gender = factor(Gender, levels = c("Men", "Women")),
    Education = factor(edu, levels = c("low", "medium", "high"))
  )

# Step 3: Get ordering based on lowest prop_healthy for low-educated women
ordering <- prop_dat %>%
  filter(Gender == "Women", Education == "low") %>%
  arrange(prop_healthy) %>%
  pull(country)

# Step 4: Apply the same ordering to all data
prop_dat <- prop_dat %>%
  mutate(country = factor(country, levels = rev(ordering)))

# Step 5: Plot
prop_plot <- ggplot(prop_dat) +
  geom_bar(aes(x = country, y = prop_healthy, fill = Education), 
           stat = "identity", position = position_dodge(), width = 0.8) + 
  coord_flip(ylim = c(50, 100)) +
  facet_wrap(. ~ Gender, nrow = 1) +
  ylab("%") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.8, "lines"),
    legend.box.margin = margin(-5, 0, 0, 0),
    legend.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank()
  ) +
  scale_fill_viridis(discrete = TRUE, option = "C", end = 0.8, direction = 1) 

prop_plot
ggsave("Figs/prop_of_LE_edu.png",  dpi = 350, width = 2244 / 350, height = 1260 / 350, units = "in")







ggplot(prop_dat) +
  geom_bar(aes(x = country, y = prop_healthy, fill = Gender), 
           stat = "identity", position = position_dodge(), width = 0.8) + 
  coord_flip(ylim=c(40,100)) +
  facet_wrap(.~Education, nrow = 1) +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines")) +
  ylab("%") +
  scale_fill_viridis(discrete = T, option = "C", end = 0.8, direction = -1) +
  guides(fill = guide_legend(reverse = TRUE)) 

ggsave("Figs/prop_by_sex.png")


prop_dat_gender <- prop_dat %>% 
  select(-edu) %>% 
  pivot_wider(names_from = "Education", values_from = "prop_healthy") %>% 
  mutate(edu_gap = high - low) %>% 
  select(-c(low, medium, high)) %>% 
  pivot_wider(names_from = "Gender", values_from = "edu_gap") %>% 
  mutate(gender_dif_in_diff = Women - Men)
  

ggplot(prop_dat_gender) +
  geom_bar(aes(x = country, y = gender_dif_in_diff), 
           stat = "identity", position = position_dodge(), width = 0.8) + 
  coord_flip() +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines")) +
  ylab("%") +
  scale_fill_viridis(discrete = T, option = "C", end = 0.8, direction = -1) +
  guides(fill = guide_legend(reverse = TRUE)) 

# Sensitivity tests -------------------------------------------------------


## Practice effects  -------------------------------------------------------

dat_noPE <- read.csv("Results/Sensitivity/No PE/final_noPE.csv")
dat_noPE <- dat_noPE %>% 
  mutate(Adj = "Not adjusted for PE")
dat_PE <- dat %>% 
  mutate(Adj = "Adjusted for PE") %>% 
  select(-c(Gender, Education))

dat_sens1 <- rbind(dat_noPE, dat_PE)

## order data
dat_sens1 <- dat_sens1 %>% 
  mutate(country = factor(country, levels = c("Czechia", "Estonia", "Slovenia",
                                              "Austria", "Belgium", "Denmark",
                                              "Sweden", "France", "Italy", "Spain")),
         Education = factor(edu, levels = c("low", "medium", "high")),
         Gender = gender)

CIFLE <- dat_sens1 %>% filter(type=="CIFLE")
LE <- dat_sens1 %>%   filter(type=="LE") 
CILE <- dat_sens1 %>%   filter(type=="CILE") 

Diff <- CIFLE %>% 
  select(country, gender, edu, p_est, Adj) %>% 
  pivot_wider(names_from = "Adj", values_from = "p_est") %>% 
  mutate(difference = `Adjusted for PE` - `Not adjusted for PE`,
         edu = factor(edu, levels = c("low", "medium", "high"))) %>% 
  arrange(country, gender, edu)

###---- CIFLE at 50 by edu 

min <- min(CIFLE$lower)
max <- max(CIFLE$upper)

ggplot(data = CIFLE) +
  geom_point(data = CIFLE %>% filter(Adj == "Adjusted for PE"),
             aes(x = p_est, y = country, color = "Adjusted for PE"), size = 3) +
  geom_point(data = CIFLE %>% filter(Adj == "Not adjusted for PE"),
             aes(x = p_est, y = country, color = ifelse(p_est > CIFLE$p_est[CIFLE$Adj == "Adjusted for PE"], "green", "red")), 
             size = 3, alpha = 0.35) +  # Adjust alpha as needed
  scale_y_discrete(limits = rev) +
  labs(x = "Years") +
  facet_grid(Education ~ Gender) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box.margin = margin(-12,0,0,0),
        legend.background = element_rect(linetype = "blank"),
        legend.title = element_text(size = 12),
        axis.title.y = element_blank()) +
  scale_color_manual(values = c("Adjusted for PE" = viridis(1)[1],
                                "green" = "green",
                                "red" = "red"),
                     labels = c("Adjusted for PE", "Greater than Adjusted", "Less than Adjusted"),
                     name = "Model Specification")

ggsave("Figs/sens_PE.png", dpi = 300)



## 1.3 SD ------------------------------------------------------------------

dat_1.3SD <- read.csv("Results/Sensitivity/1.3SD/final_1.3SD.csv")
dat_1.3SD <- dat_1.3SD %>% 
  mutate(Threshold = "1.3 SD")
dat_1.5SD <- dat %>% 
  mutate(Threshold = "1.5 SD") 
  # select(-c(Gender, Education))

dat_sens2 <- rbind(dat_1.3SD, dat_1.5SD)

## order data
dat_sens2 <- dat_sens2 %>% 
  mutate(country = factor(country, levels = c("Estonia", "Czechia", "Slovenia", 
                                              "Austria", "Denmark", "Belgium",
                                              "Sweden", "France", "Spain", "Italy")),
         Education = factor(edu, levels = c("low", "medium", "high")),
         Gender = gender)

CIFLE <- dat_sens2 %>% filter(type=="CIFLE")
LE <- dat_sens2 %>%   filter(type=="LE") 
CILE <- dat_sens2 %>%   filter(type=="CILE") 


Diff <- CIFLE %>% 
  select(country, gender, edu, p_est, Threshold) %>% 
  pivot_wider(names_from = "Threshold", values_from = "p_est") %>% 
  mutate(difference = `1.5 SD` - `1.3 SD`,
         edu = factor(edu, levels = c("low", "medium", "high"))) %>% 
  arrange(country, gender, edu)

###---- CIFLE at 50 by edu 

min <- min(CIFLE$lower)
max <- max(CIFLE$upper)


ggplot(data = CIFLE) +
  geom_point(data = CIFLE %>% filter(Threshold == "1.5 SD"),
             aes(x = p_est, y = country, color = "1.5 SD"), size = 3) +
  geom_point(data = CIFLE %>% filter(Threshold == "1.3 SD"),
             aes(x = p_est, y = country, color = "1.3 SD"), size = 3,
             alpha = 0.35) +
  scale_y_discrete(limits=rev) +
  labs(x="Years") +
  facet_grid(Education ~ Gender) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box.margin=margin(-12,0,0,0),
        legend.background = element_rect(linetype = "blank"),
        legend.title=element_text(size=12),
        axis.title.y = element_blank()) +
  scale_color_manual(values = c("1.5 SD" = viridis(1)[1],
                                "1.3 SD" = viridis(1, alpha = 0.35)[1]),
                     labels = c("1.3 SD", "1.5 SD"),
                     name = "Threshold Specification")

ggsave("Figs/sens_SD.png", dpi = 350)



## Attrition ---------------------------------------------------------------

dat_unadjusted <- read.csv("Results/FEB/final_edu.csv")
dat_adjusted <- read.csv("Results/Sensitivity/Attrition/attr_final_edu.csv")

dat_unadjusted <- dat_unadjusted %>% 
  mutate(Model = "Unadjusted",
         estimate = p_est) %>% 
  select(-p_est)

dat_adjusted <- dat_adjusted %>% 
  mutate(Model = "Adjusted") 

# Combine the two data frames
combined_df <- bind_rows(dat_unadjusted, dat_adjusted)

compare_models <- combined_df %>% 
  filter(type == "LE") %>% 
  select(-c(lower, upper)) %>% 
  pivot_wider(names_from = "Model", values_from = "estimate") %>% 
  mutate(difference = Unadjusted - Adjusted,
         country = factor(country, levels = c(
           "Czechia", "Estonia", "Slovenia", 
           "Austria", "Belgium", 
           "Denmark", "Sweden", 
           "France", "Italy", "Spain"
         )),
         edu = factor(edu, levels = c("low", "medium", "high")))

ggplot(compare_models, aes(x = country, y = difference, fill = edu)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
  scale_x_discrete(limits = rev(levels(compare_models$country))) +
  coord_flip() +  # Adjust y-limits for better visualization
  facet_wrap(~ gender, nrow = 1) +  # Facet by gender
  theme_minimal(base_size = 12) +  # Adjusted base size
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 10, face = "bold"),  # Smaller legend title
    legend.text = element_text(size = 9),  # Reduce legend text size
    legend.key.size = unit(0.8, "lines"),  # Smaller legend key size
    strip.text = element_text(size = 12, face = "bold"),  # Bold facet labels
    axis.title.y = element_blank()  # Remove y-axis title for clarity
  ) +
  scale_fill_viridis_d(option = "C", end = 0.8) + # Color palette for education levels
  ylab("Difference in Life Expectancy, Years") + # Label y-axis
  labs(fill = "Education Level")
  

ggsave("Figs/Attrition_LE_diff.png", dpi = 350, width = 2244 / 350, height = 1260 / 350, units = "in")



## Belgium as a benchmark --------------------------------------------------

Sys.setenv(LANG = "en")

rm(list=ls())

setwd("U:/A3_CHE")

dat <- read.csv("Results/FEB/final_edu.csv")
belgium <- read.csv("Sensitivity/Benchmark_country/Results/Combined_edu.csv")

dat <- dat %>% 
  rename(estimate = p_est,
         Expectancy = type) %>% 
  mutate(Type = "country_specific")

belgium <- belgium %>% 
  mutate(Type = "fixed_country", 
         Expectancy = ifelse(Expectancy == "Total", 
                             yes = "LE", no = ifelse(
                               Expectancy == "Cognitively Healthy", 
                               yes = "CIFLE", no = "CILE"
                             )
         ))

# Combine the two data frames
combined_df <- bind_rows(dat, belgium)

compare_models <- combined_df %>% 
  filter(Expectancy == "CIFLE") %>% 
  select(-c(lower, upper)) %>% 
  pivot_wider(names_from = "Type", values_from = "estimate") %>% 
  mutate(difference = country_specific - fixed_country,
         country = factor(country, levels = c(
           "Czechia", "Estonia", "Slovenia", 
           "Austria", "Belgium", 
           "Denmark", "Sweden", 
           "France", "Italy", "Spain"
         )),
         edu = factor(edu, levels = c("low", "medium", "high")))

ggplot(compare_models, aes(x = country, y = difference, fill = edu)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
  scale_x_discrete(limits = rev(levels(compare_models$country))) +
  coord_flip() +  # Adjust y-limits for better visualization
  facet_wrap(~ gender, nrow = 1) +  # Facet by gender
  theme_minimal(base_size = 12) +  # Adjusted base size
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 10, face = "bold"),  # Smaller legend title
    legend.text = element_text(size = 9),  # Reduce legend text size
    legend.key.size = unit(0.8, "lines"),  # Smaller legend key size
    strip.text = element_text(size = 12, face = "bold"),  # Bold facet labels
    axis.title.y = element_blank()  # Remove y-axis title for clarity
  ) +
  scale_fill_viridis_d(option = "C", end = 0.8) + # Color palette for education levels
  ylab("Difference in Cognitive Health Expectancy, Years") + # Label y-axis
  labs(fill = "Education Level")

ggsave("Figs/Benchmark_country.png", dpi = 350, width = 2244 / 350, height = 1260 / 350, units = "in")


## No simulation -----------------------------------------------------------

setwd("U:/A3_CHE")

simulation <- read.csv("Results/FEB/final_edu.csv")
simple <- read.csv("Sensitivity/No_simulation/Results/Combined_edu.csv")

simulation <- simulation %>% 
  rename(estimate = p_est,
         Expectancy = type) %>% 
  mutate(Type = "Simulation")

simple <- simple %>% 
  mutate(Type = "Direct", 
         Expectancy = ifelse(Expectancy == "Total", 
                             yes = "LE", no = ifelse(
                               Expectancy == "Cognitively Healthy", 
                               yes = "CIFLE", no = "CILE"
                             )
         ))

# Combine the two data frames
combined_df <- bind_rows(simulation, simple)

compare_models <- combined_df %>% 
  filter(Expectancy == "LE") %>% 
  select(-c(lower, upper)) %>% 
  pivot_wider(names_from = "Type", values_from = "estimate") %>% 
  mutate(difference = Simulation - Direct,
         country = factor(country, levels = c(
           "Czechia", "Estonia", "Slovenia", 
           "Austria", "Belgium", 
           "Denmark", "Sweden", 
           "France", "Italy", "Spain"
         )),
         edu = factor(edu, levels = c("low", "medium", "high")))

ggplot(compare_models, aes(x = country, y = difference, fill = edu)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
  scale_x_discrete(limits = rev(levels(compare_models$country))) +
  coord_flip() +  # Adjust y-limits for better visualization
  facet_wrap(~ gender, nrow = 1) +  # Facet by gender
  theme_minimal(base_size = 12) +  # Adjusted base size
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 10, face = "bold"),  # Smaller legend title
    legend.text = element_text(size = 9),  # Reduce legend text size
    legend.key.size = unit(0.8, "lines"),  # Smaller legend key size
    strip.text = element_text(size = 12, face = "bold"),  # Bold facet labels
    axis.title.y = element_blank()  # Remove y-axis title for clarity
  ) +
  scale_fill_viridis_d(option = "C", end = 0.8) + # Color palette for education levels
  ylab("Difference in Life Expectancy, Years") + # Label y-axis
  labs(fill = "Education Level")

ggsave("Figs/Simulation_vs_direct.png", dpi = 350, width = 2244 / 350, height = 1260 / 350, units = "in")


## Interaction -----------------------------------------------------------

setwd("U:/A3_CHE")

original <- read.csv("Results/FEB/final_edu.csv")
interaction <- read.csv("Sensitivity/Interaction/Results/Combined_edu.csv")

original <- original %>% 
  rename(estimate = p_est,
         Expectancy = type) %>% 
  mutate(Type = "Original")

interaction <- interaction %>% 
  mutate(Type = "Interaction", 
         Expectancy = ifelse(Expectancy == "Total", 
                             yes = "LE", no = ifelse(
                               Expectancy == "Cognitively Healthy", 
                               yes = "CIFLE", no = "CILE"
                             )
         ))

# Combine the two data frames
combined_df <- bind_rows(original, interaction)

compare_models <-
  combined_df %>% 
  filter(Expectancy == "LE") %>% 
  select(-c(lower, upper)) %>% 
  pivot_wider(names_from = "Type", values_from = "estimate") %>% 
  mutate(difference = Original - Interaction,
         country = factor(country, levels = c(
           "Czechia", "Estonia", "Slovenia", 
           "Austria", "Belgium", 
           "Denmark", "Sweden", 
           "France", "Italy", "Spain"
         )),
         edu = factor(edu, levels = c("low", "medium", "high")))

ggplot(compare_models, aes(x = country, y = difference, fill = edu)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) + 
  scale_x_discrete(limits = rev(levels(compare_models$country))) +
  coord_flip() +  # Adjust y-limits for better visualization
  facet_wrap(~ gender, nrow = 1) +  # Facet by gender
  theme_minimal(base_size = 12) +  # Adjusted base size
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 10, face = "bold"),  # Smaller legend title
    legend.text = element_text(size = 9),  # Reduce legend text size
    legend.key.size = unit(0.8, "lines"),  # Smaller legend key size
    strip.text = element_text(size = 12, face = "bold"),  # Bold facet labels
    axis.title.y = element_blank()  # Remove y-axis title for clarity
  ) +
  scale_fill_viridis_d(option = "C", end = 0.8) + # Color palette for education levels
  ylab("Difference in Life Expectancy at Age 50, Years") + # Label y-axis
  labs(fill = "Education Level")

ggsave("Figs/Interaction.png", dpi = 350, width = 2244 / 350, height = 1260 / 350, units = "in")


