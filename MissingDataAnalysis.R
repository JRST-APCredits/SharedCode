#### Setup ####
# Load libraries
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse",           # Data wrangling and descriptive stats
               "naniar",              # Little's MCAR test
               "psych")               # Correlations                                                     

# Load clean dataset
df_clean <- read.csv("YOUR CLEAN FILE HERE")

## Filter for student level inclusion/exclusion criteria ####
df_clean <- df_clean %>%
  # Include
  filter(transfer == 0) %>%
  filter(tookcourse_2 == 1) %>%
  filter(cohort >= 2013 & cohort <= 2018) %>%
  # Exclude
  filter(international == 0) 

#### Missing Data Analysis ####
# Examine Missing Variables
graphics.off() 
gg_miss_var(df_clean)
colSums(is.na(df_clean))

# Vector of variables with missing data
missVar <- c("apscore", "hsgpa", "mathsr", "englsr", "enrl_from_cohort", "cohort", "numgrade", "numgrade_2")

# Check for MCAR (Little's Test)
# Full dataset
LittlesTest_full <- df_clean %>%
  dplyr::select(missVar) %>%
  mcar_test(.)
print(LittlesTest_full) # If p-value < .05, data is NOT missing completely at random (MCAR)

# Check for MAR (test for association between observed variables and missingness indicators)
df_clean_missing <- df_clean %>% 
  #changing missingness variables into 0/1 values
  dplyr::mutate(across(everything(), ~ +(is.na(.)), .names = 'miss_{col}')) %>%
  dplyr::select(firstgen, urm, female, lowincomeflag, transfer, international, ell, us_hs, 
                cohort_2013:cohort_2018, miss_aptaker:miss_apyear)

# Check that all variables are numeric for correlation plot
sapply(df_clean_missing, class)

graphics.off() 
cor.plot(df_clean_missing, stars = T, xlas = 2)
