
# Data cleaning -------------------------------------------------------------

# Load packages -----------------------------------------------------------
library(readr)
library(tidyverse)
library(dplyr)
library(psych)
library(janitor)

# Read in the data and save it to appropriate folder ----------------------
LED_raw <- read.csv("Life Expectancy Data.csv")
View(LED_raw)
write_csv(LED_raw, "data/Unprocessed/LED_raw.csv")
read_csv("data/Unprocessed/LED_raw.csv")

# Get preliminary information to inform cleaning --------------------------
skimr::skim(LED_raw)

# Clean data --------------------------------------------------------------

LED_clean <- LED_raw %>% 
  rename(thinness_10_to_19_years = thinness..1.19.years, thinness_5_to_9_years = thinness.5.9.years,
         under_five_deaths = under.five.deaths, income_composition_of_resources = Income.composition.of.resources, 
         life_expectancy = Life.expectancy, adult_mortality = Adult.Mortality, infant_deaths = infant.deaths, 
         total_expenditure = Total.expenditure, percentage_expenditure = percentage.expenditure) %>%
  mutate(infant_deaths = ifelse(infant_deaths == 0, NA, infant_deaths)) %>%
  mutate(Population = ifelse(Population < 1000, NA, Population)) %>%
  mutate(percentage_expenditure = ifelse(percentage_expenditure == 0, NA, percentage_expenditure)) %>%
  clean_names()
# rename columns so they are 1. more uniform in naming and 2. revalue data points that should be NA 

sum(is.na(LED_clean$infant_deaths))
sum(is.na(LED_clean$Population))
#check that the numbers of NAs have increased since cleaning
  
View(LED_clean)

sum(is.na(LED_raw))
sum(is.na(LED_clean))
#check that the number of NAs across the dataframe have increased as compared to the untidy version

skimr::skim(LED_clean)

# Save an RDS  ------------------------------------------------------------
write_rds(LED_clean, "data/Processed/LED.rds")
read_rds("data/LED.rds")

# Save final cleaned product to Processed folder --------------------------
write_csv(LED_clean, "data/Processed/LED_cleaned.csv")
read_csv("data/Processed/LED_cleaned.csv")


describe(LED_clean)




