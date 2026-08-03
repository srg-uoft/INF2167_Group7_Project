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

## make a long version of the data to pivot mode of transportation
collisions_long <- collisions_cleaned |>
  pivot_longer(
    cols = c(automobile, motorcycle, pedestrian, bicycle, passenger), 
    names_to = "vehicle_type", 
    values_to = "involved_yesno"
  ) |>
  filter(involved_yesno == "YES")

########################## Prepare the Dataframes ##############################
overall_collisions <- collisions_long |>
  group_by(neighbourhood, occ_month, occ_dow, occ_hour) |>
  count() |>
  rename(number_of_collisions = n)

automobile_collisions <- collisions_long |>
  filter(vehicle_type == "automobile") |>
  group_by(neighbourhood, occ_month, occ_dow, occ_hour) |>
  count() |>
  rename(number_of_collisions = n)

bicycle_collisions <- collisions_long |>
  filter(vehicle_type == "bicycle") |>
  group_by(neighbourhood, occ_month, occ_dow, occ_hour) |>
  count() |>
  rename(number_of_collisions = n)

motorcycle_collisions <- collisions_long |>
  filter(vehicle_type == "motorcycle") |>
  group_by(neighbourhood, occ_month, occ_dow, occ_hour) |>
  count() |>
  rename(number_of_collisions = n)

pedestrian_collisions <- collisions_long |>
  filter(vehicle_type == "pedestrian") |>
  group_by(neighbourhood, occ_month, occ_dow, occ_hour) |>
  count() |>
  rename(number_of_collisions = n)

passenger_collisions <- collisions_long |>
  filter(vehicle_type == "passenger") |>
  group_by(neighbourhood, occ_month, occ_dow, occ_hour) |>
  count() |>
  rename(number_of_collisions = n)

############################ Estimate the Models ###############################

overall_model <- lm(number_of_collisions ~ neighbourhood + occ_month + occ_dow + occ_hour, data = overall_collisions)
summary(overall_model)

automobile_model <- lm(number_of_collisions ~ neighbourhood + occ_month + occ_dow + occ_hour, data = automobile_collisions)
summary(automobile_model)

bicycle_model <- lm(number_of_collisions ~ neighbourhood + occ_month + occ_dow + occ_hour, data = bicycle_collisions)
summary(bicycle_model)

pedestrian_model <- lm(number_of_collisions ~ neighbourhood + occ_month + occ_dow + occ_hour, data = pedestrian_collisions)
summary(pedestrian_model)

passenger_model <- lm(number_of_collisions ~ neighbourhood + occ_month + occ_dow + occ_hour, data = passenger_collisions)
summary(passenger_model)

motorcycle_model <- lm(number_of_collisions ~ neighbourhood + occ_month + occ_dow + occ_hour, data = motorcycle_collisions)
summary(motorcycle_model)

################################################################################
################################################################################
################################################################################
################################################################################
######## Failed Linear Model Process, Keeping for Records but Not Using ########
################################################################################
################################################################################
################################################################################
################################################################################

## make a summary table for collisions by neighbourhood
collisions_sum <- collisions_cleaned |>
  group_by(neighbourhood) |>
  count() |>
  rename(Total_Collisions = n)

## make a wide dataframe by month
collisions_wide_month <- collisions_long |>
  group_by(neighbourhood) |>
  count(occ_month) |>
  ungroup() |>
  pivot_wider(names_from = occ_month, values_from = n)

## make a wide dataframe by day of week
collisions_wide_dow <- collisions_long |>
  group_by(neighbourhood) |>
  count(occ_dow) |>
  ungroup() |>
  pivot_wider(names_from = occ_dow, values_from = n)

## make a wide dataframe by hour
collisions_wide_hour <- collisions_long |>
  group_by(neighbourhood) |>
  count(occ_hour) |>
  ungroup() |>
  pivot_wider(names_from = occ_hour, values_from = n)

## make a wide dataframe by vehicle type
collisions_wide_vt <- collisions_long |>
  group_by(neighbourhood) |>
  count(vehicle_type) |>
  ungroup() |>
  pivot_wider(names_from = vehicle_type, values_from = n)

## join the wide tables together
collisions_wide_all <- collisions_sum |>
  left_join(collisions_wide_month, by = c("neighbourhood" = "neighbourhood")) |>
  left_join(collisions_wide_dow, by = c("neighbourhood" = "neighbourhood")) |>
  left_join(collisions_wide_hour, by = c("neighbourhood" = "neighbourhood")) |>
  left_join(collisions_wide_vt, by = c("neighbourhood" = "neighbourhood")) |>
  clean_names()

# model for if all else fails
# too-high R^2, because "vehicles involved in collisions" and "total number of collisions" are too similar
test_model <- lm(total_collisions ~ monday + tuesday + wednesday + thursday + friday + saturday + sunday, data = collisions_wide_all)
summary(test_model)

################################################################################
################################################################################
################################################################################
################################################################################
####### Failed Logistic Model Process, Keeping for Records but Not Using #######
################################################################################
################################################################################
################################################################################
################################################################################

## dropping N/R and NA values
collisions_modeling <- collisions_cleaned |>
  filter((automobile == "YES") | (automobile == "NO"))

## recoding the binary variables
collisions_modeling <- collisions_modeling |>
  mutate("automobile" = if_else(automobile == "YES", 1, 0)) |>
  mutate("bicycle" = if_else(automobile == "YES", 1, 0)) |>
  mutate("motorcycle" = if_else(automobile == "YES", 1, 0)) |>
  mutate("pedestrian" = if_else(automobile == "YES", 1, 0)) |>
  mutate("passenger" = if_else(automobile == "YES", 1, 0))

# trying to predict whether a car will be involved in a crash using temporal data, not working very well (probably since almost all data points have cars)
# not enough negative for it to learn from
car_model <- glm(automobile ~ occ_dow, data = collisions_modeling, family = binomial, maxit = 100)
car_predictions <- predictions(car_model) |>
  as_tibble()
head(car_predictions)

slopes(car_model, newdata = "median") |>
  select(term, contrast, estimate) |>
  tt(digits = 1)
  
