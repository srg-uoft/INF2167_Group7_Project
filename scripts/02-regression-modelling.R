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
library(broom)
library(modelsummary)
library(patchwork)

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
  group_by(neighbourhood, occ_month, occ_dow, occ_hour, vehicle_type) |>
  count() |>
  rename(number_of_collisions = n)

overall_collisions_minus_vt <- collisions_long |>
  group_by(neighbourhood, occ_month, occ_dow, occ_hour) |>
  count() |>
  rename(number_of_collisions = n)

############################ Estimate the Models ###############################

overall_model <- lm(number_of_collisions ~ neighbourhood + occ_month + occ_dow + occ_hour + vehicle_type, data = overall_collisions)
summary(overall_model)
# R^2 = 0.3241

minus_vt_model <- lm(number_of_collisions ~ neighbourhood + occ_month + occ_dow + occ_hour, data = overall_collisions_minus_vt)
summary(minus_vt_model)
# R^2 = 0.5806

######################### Make Figures from Models #############################

## make both residuals plots
plot1 <- augment(overall_model) |>
  ggplot(aes(.fitted, .resid)) +
  geom_point(colour = "#661100", alpha = .7) +
  geom_hline(yintercept = 0, colour = "#FF4434", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Residuals from Modelling Collision Frequency \nwith Spatial, Temporal, and Vehicle Type Variables",
    subtitle = "Vehicle Type Included in Predictors",
    x = "Predicted Values",
    y = "Residuals") +
  theme(
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.text = element_text(size = 9),
    title = element_text(size = 16),
    plot.subtitle = element_text(size = 12, face = "bold"),
    panel.spacing.x = unit(2.63, "lines")
  )

plot2 <- augment(minus_vt_model) |>
  ggplot(aes(.fitted, .resid)) +
  geom_point(colour = "#117733", alpha = .7) +
  geom_hline(yintercept = 0, colour = "#00FF00", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Residuals from Modelling Collision Frequency \nwith Spatial and Temporal Variables",
    subtitle = "Vehicle Type Excluded from Predictors",
    x = "Predicted Values",
    y = "Residuals") +
  theme(
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.text = element_text(size = 9),
    title = element_text(size = 16),
    plot.subtitle = element_text(size = 12, face = "bold"),
    panel.spacing.x = unit(2.63, "lines")
  )

## combine into one visual
combined_plot <- plot1 + plot2 + plot_layout(ncol = 1)
combined_plot

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
  
