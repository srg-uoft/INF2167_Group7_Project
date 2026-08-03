################################### Preamble ###################################
# Purpose: A space for cleaning data and using that data to run various models,
#           in order to analyze temporal traffic collision trends
#           within the Greater Toronto Area
# Author: Stella Gregorski, Yiyang Qin
# Date: May-August 2026
# Contact: stella.gregorski@mail.utoronto.ca
# License: MIT
# Pre-requisites: please install the Tidyverse, Here, and Janitor packages on your local 
#                 machine ahead of time, as well as load the project GitHub repository 
#                 so the relative filepaths will function as intended.

########################### Import Required Libraries ##########################

library(tidyverse)
library(here)
library(janitor)
library(kableExtra)
library(marginaleffects)
library(broom)
library(modelsummary)
library(tinytable)

########################### Read In the Dataset ################################
collisions <- read_csv(here("data/02 - analysis data", "working-data-2020-2026.csv"))

############################### Establish Goals ################################

## models
# 5 versions of the same model
# one for each type of transportation 
# predictors are month, day of week, hour of day

####################### Cleaning & Prepping the Data ###########################

## rename some of the columns to make them easier to understand
collisions_cleaned <- collisions |>
  rename(INJURIES = INJURY_COLLISIONS) |>
  rename(FAILURE_TO_REMAIN = FTR_COLLISIONS) |>
  rename(PROPERTY_DAMAGE_OVER_2000 = PD_COLLISIONS) |>
  rename(NEIGHBOURHOOD = NEIGHBOURHOOD_158)

## changing column names to snake_case using janitor
collisions_cleaned <- collisions_cleaned |>
  clean_names()

## dropping N/R and NA values
collisions_modeling <- collisions_cleaned |>
  filter((automobile == "YES") | (automobile == "NO"))

######################### Recode the Binary Columns ############################

collisions_modeling <- collisions_modeling |>
  mutate("automobile" = if_else(automobile == "YES", 1, 0)) |>
  mutate("bicycle" = if_else(automobile == "YES", 1, 0)) |>
  mutate("motorcycle" = if_else(automobile == "YES", 1, 0)) |>
  mutate("passenger" = if_else(automobile == "YES", 1, 0)) |>
  mutate("pedestrian" = if_else(automobile == "YES", 1, 0))

############################ Estimate the Models ###############################
car_model <- glm(automobile ~ occ_dow, data = collisions_modeling, family = binomial, maxit = 100)
car_predictions <- predictions(car_model) |>
  as_tibble()
head(car_predictions)

slopes(car_model, newdata = "median") |>
  select(term, contrast, estimate) |>
  tt(digits = 1)







