################################### Preamble ###################################

# Purpose: A space for creating visuals for use in this project's
#           report and presentation files.
# Author: Stella Gregorski, Yiyang Qin
# Date: May-August 2026
# Contact: stella.gregorski@mail.utoronto.ca
# License: MIT
# Pre-requisites: please install the Tidyverse, Here, Paletteer, and Janitor packages 
#                 on your local machine ahead of time, as well as load the project 
#                 GitHub repository so the relative filepaths will function as intended.

########################### Import Required Libraries ##########################

library(tidyverse)
library(here)
library(janitor)
library(kableExtra)

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

## add a new column for month number
months_df = data.frame(name = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), number = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

collisions_cleaned <- collisions_cleaned |>
  left_join(months_df, by = c("occ_month" = "name")) |>
  rename(occ_month_num = number)

## add a new column for day number
dow_df = data.frame(name = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), number = c(1, 2, 3, 4, 5, 6, 7))

collisions_cleaned <- collisions_cleaned |>
  left_join(dow_df, by = c("occ_dow" = "name")) |>
  rename(occ_dow_num = number)

## make a long version of the data to pivot mode of transportation
collisions_long <- collisions_cleaned |>
  pivot_longer(
    cols = c(automobile, motorcycle, passenger, bicycle, pedestrian), 
    names_to = "vehicle_type", 
    values_to = "involved_yesno"
  )

########## Figure 1: Accidents Per Month by Mode of Transportation #############

## create a separate dataframe that removes vehicle types not involved in crashes, then counts per vehicle type by month
vehicle_counts_by_month <- collisions_long |>
  filter(involved_yesno == "YES") |>
  group_by(occ_month_num) |>
  count(vehicle_type)

## make a faceted line plot to show temporal trends for each vehicle type
vehicle_counts_by_month |>
  ggplot(
    aes(x = occ_month_num, y = n, color = vehicle_type)
    ) +
  geom_line(linewidth = 1.5
    ) +
  geom_point(size = 3
    ) + 
  facet_wrap(
    ~vehicle_type, scales = "free",
    labeller = labeller(vehicle_type = c("automobile" = "Automobile", "bicycle" = "Bicycle", "motorcycle" = "Motorcycle", "passenger" = "Public Transit Vehicle", "pedestrian" = "Pedestrian"))
    ) +
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.abb
    ) +
  theme(
    axis.text.x = element_text(angle = 55, vjust = 0.9, hjust=1),
    strip.text.x = element_text(size = 14),
    strip.background = element_rect(colour = "black", fill = "lightgrey"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11),
    title = element_text(size = 22),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 14),
    panel.spacing.x = unit(2.63, "lines")
    ) +
  guides(
    color = "none"
    ) +
  labs(
    x = "Month",
    y = "Number of Collisions", 
    title = "Number of Collisions Responded to by Month",
    subtitle = "Data Collected from 2020-2026",
  )

########### Figure 2: Accidents Per Hour by Mode of Transportation #############

## create a separate dataframe that removes vehicle types not involved in crashes, then counts per vehicle type by hour
vehicle_counts_by_hour <- collisions_long |>
  filter(involved_yesno == "YES") |>
  group_by(occ_hour) |>
  count(vehicle_type)

## make a faceted line plot to show temporal trends for each vehicle type
vehicle_counts_by_hour |>
  ggplot(
    aes(x = occ_hour, y = n, color = vehicle_type)
  ) +
  geom_line(linewidth = 1.5
  ) +
  geom_point(size = 3
  ) + 
  facet_wrap(
    ~vehicle_type, scales = "free",
    labeller = labeller(vehicle_type = c("automobile" = "Automobile", "bicycle" = "Bicycle", "motorcycle" = "Motorcycle", "passenger" = "Public Transit Vehicle", "pedestrian" = "Pedestrian"))
  ) +
  theme(
    axis.text.x = element_text(angle = 55, vjust = 0.9, hjust=1),
    strip.text.x = element_text(size = 14),
    strip.background = element_rect(colour = "black", fill = "lightgrey"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 9),
    title = element_text(size = 22),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 14),
    panel.spacing.x = unit(2.63, "lines")
  ) +
  guides(
    color = "none"
  ) +
  scale_x_continuous(
    breaks = seq(0, 24, by = 1)
    ) +
  labs(
    x = "Hour of Day Collision Occurred (24-hour Time)",
    y = "Number of Collisions", 
    title = "Number of Collisions Responded to by Hour",
    subtitle = "Data Collected from 2020-2026",
  )

##### Figure 3: Accidents Per Day of the Week by Mode of Transportation ########

## create a separate dataframe that removes vehicle types not involved in crashes, then counts per vehicle type by day of the week
vehicle_counts_by_weekday <- collisions_long |>
  filter(involved_yesno == "YES") |>
  group_by(occ_dow_num) |>
  count(vehicle_type)


## make a faceted line plot to show temporal trends for each vehicle type
vehicle_counts_by_weekday |>
  ggplot(
    aes(x = occ_dow_num, y = n, color = vehicle_type)
  ) +
  geom_line(linewidth = 1.5
  ) +
  geom_point(size = 3
  ) + 
  facet_wrap(
    ~vehicle_type, scales = "free",
    labeller = labeller(vehicle_type = c("automobile" = "Automobile", "bicycle" = "Bicycle", "motorcycle" = "Motorcycle", "passenger" = "Public Transit Vehicle", "pedestrian" = "Pedestrian"))
  ) +
  theme(
    axis.text.x = element_text(angle = 55, vjust = 0.9, hjust=1),
    strip.text.x = element_text(size = 14),
    strip.background = element_rect(colour = "black", fill = "lightgrey"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11),
    title = element_text(size = 22),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 14),
    panel.spacing.x = unit(2.63, "lines")
  ) +
  guides(
    color = "none"
  ) +
  scale_x_continuous(
    breaks = seq(1, 7, by = 1),
    labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  ) +
  labs(
    x = "Day of Week Collision Occured",
    y = "Number of Collisions", 
    title = "Number of Collisions Responded to by Day of Week",
    subtitle = "Data Collected from 2020-2026",
  )

################# Figure 4: Summary Table of Riskiest Times ####################

# summary table for worst weekday for each mode of transportation
sum_vehicle_counts_by_weekday <- vehicle_counts_by_weekday |>
  group_by(vehicle_type) |>
  summarize(max_weekday = occ_dow_num[which(n == max(n))]) |>
  mutate("max_weekday" = case_when(
    max_weekday == 1 ~ "Sunday",
    max_weekday == 2 ~ "Monday",
    max_weekday == 3 ~ "Tuesday",
    max_weekday == 4 ~ "Wednesday",
    max_weekday == 5 ~ "Thursday",
    max_weekday == 6 ~ "Friday",
    max_weekday == 7 ~ "Saturday")) |>
  mutate("vehicle_type" = case_when(
    vehicle_type == "automobile" ~ "Automobile",
    vehicle_type == "bicycle" ~ "Bicycle",
    vehicle_type == "motorcycle" ~ "Motorcycle",
    vehicle_type == "passenger" ~ "Public Transit Vehicle",
    vehicle_type == "pedestrian" ~ "Pedestrian"))

# summary table for worst month for each mode of transportation
sum_vehicle_counts_by_month <- vehicle_counts_by_month |>
  group_by(vehicle_type) |>
  summarize(max_month = occ_month_num[which(n == max(n))]) |>
  mutate("max_month" = case_when(
    max_month == 1 ~ "January",
    max_month == 2 ~ "February",
    max_month == 3 ~ "March",
    max_month == 4 ~ "April",
    max_month == 5 ~ "May",
    max_month == 6 ~ "June",
    max_month == 7 ~ "July",
    max_month == 8 ~ "August",
    max_month == 9 ~ "September",
    max_month == 10 ~ "October",
    max_month == 11 ~ "November",
    max_month == 12 ~ "December")) |>
  mutate("vehicle_type" = case_when(
    vehicle_type == "automobile" ~ "Automobile",
    vehicle_type == "bicycle" ~ "Bicycle",
    vehicle_type == "motorcycle" ~ "Motorcycle",
    vehicle_type == "passenger" ~ "Public Transit Vehicle",
    vehicle_type == "pedestrian" ~ "Pedestrian"))

# summary table for worst hour for each mode of transportation
sum_vehicle_counts_by_hour <- vehicle_counts_by_hour |>
  group_by(vehicle_type) |>
  summarize(max_hour = occ_hour[which(n == max(n))]) |>
  mutate("vehicle_type" = case_when(
    vehicle_type == "automobile" ~ "Automobile",
    vehicle_type == "bicycle" ~ "Bicycle",
    vehicle_type == "motorcycle" ~ "Motorcycle",
    vehicle_type == "passenger" ~ "Public Transit Vehicle",
    vehicle_type == "pedestrian" ~ "Pedestrian"))

# summary table for worst neighbourhood for each mode of transportation
vehicle_counts_by_neighbourhood <- collisions_long |>
  filter(involved_yesno == "YES") |>
  filter(neighbourhood != "NSA") |>
  group_by(neighbourhood) |>
  count(vehicle_type)

sum_vehicle_counts_by_neighbourhood <- vehicle_counts_by_neighbourhood |>
  group_by(vehicle_type) |>
  summarize(max_neighbourhood = first(neighbourhood[which(n == max(n))])) |>
  mutate("vehicle_type" = case_when(
    vehicle_type == "automobile" ~ "Automobile",
    vehicle_type == "bicycle" ~ "Bicycle",
    vehicle_type == "motorcycle" ~ "Motorcycle",
    vehicle_type == "passenger" ~ "Public Transit Vehicle",
    vehicle_type == "pedestrian" ~ "Pedestrian"))

# join the summary tables together
overall_max_summary <- sum_vehicle_counts_by_hour |>
  left_join(sum_vehicle_counts_by_weekday, by = c("vehicle_type" = "vehicle_type")) |>
  left_join(sum_vehicle_counts_by_month, by = c("vehicle_type" = "vehicle_type")) |>
  left_join(sum_vehicle_counts_by_neighbourhood, by = c("vehicle_type" = "vehicle_type")) |>
  rename('Vehicle Type' = vehicle_type, 
         'Hour with Highest Number of Recorded Collisions' = max_hour,
         'Day of Week with Highest Number of Recorded Collisions' = max_weekday,
         'Month with Highest Number of Recorded Collisions' = max_month,
         'Neighbourhood with Highest Number of Recorded Collisions' = max_neighbourhood)

# print them nicely
kable(overall_max_summary) |>
  kable_styling(latex_options = "striped")
