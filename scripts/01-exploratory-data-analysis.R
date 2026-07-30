################################### Preamble ###################################
# Purpose: A space for generating initial summary statistics and visuals for our  
#           INF2167 project, as a draft space before deciding which blocks of
#           code to run in the ultimate proposal/report
# Author: Stella Gregorski, Yiying Qin
# Date: May-August 2026
# Contact: stella.gregorski@mail.utoronto.ca
# License: MIT
# Pre-requisites: please install the Tidyverse, Here, and Janitor packages on your local 
#                 machine ahead of time, as well as load the project GitHub repository 
#                 as an R project so the relative filepaths will function as intended.

########################### Import Required Libraries ##########################
library(tidyverse)
library(here)
library(janitor)
library(paletteer)

########################### Read In the Dataset ################################
collisions <- read_csv(here("data/02 - analysis data", "working-data-2020-2026.csv"))

############################### Establish Goals ################################
# establish goals: see what months, specific days, specific hours, or neighborhoods, have the most accidents
## also can facet that by mode of transportation to see what that might look like for different modes
# establish goals: in general, see how many accidents there are per mode of transportation

########################## Viewing & Cleaning Data #############################
## classes
glimpse(collisions)
# looks like all columns are either numeric doubles or strings of characters
# doubles include the year, hour, and number of fatalities
# character strings include month name, day of the week, and many different "yes/no" variables
# modes of transportation being separate columns might be tricky, will have to pivot to use mode of transportation as a facet variable

## rename some of the columns to make them easier to understand
collisions_cleaned <- collisions |>
  rename(INJURIES = INJURY_COLLISIONS) |>
  rename(FAILURE_TO_REMAIN = FTR_COLLISIONS) |>
  rename(PROPERTY_DAMAGE_OVER_2000 = PD_COLLISIONS) |>
  rename(NEIGHBOURHOOD = NEIGHBOURHOOD_158)

## changing column names to snake_case using janitor
collisions_cleaned <- collisions_cleaned |>
  clean_names()
head(collisions_cleaned)

## add a new column for month number
months_df = data.frame(name = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), number = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

collisions_cleaned <- collisions_cleaned |>
  left_join(months_df, by = c("occ_month" = "name")) |>
  rename(occ_month_num = number)

## values
unique(collisions_cleaned$occ_month)
unique(collisions$occ_dow)
# unique values of months and days of the week seem to make sense

unique(collisions_cleaned$occ_hour)
# collisions have occurred in every possible hour of the day

table(collisions_cleaned$fatalities)
# a vast majority of collisions had no fatalities
# 2 fatalities or above could be considered outliers, total of 7 collisions of that caliber in 370000

################ Space for Drafting Proposal Statistics/Tables #################
## collisions by neighborhood, descending order
collisions_cleaned |>
  group_by(neighbourhood) |>
  count() |>
  arrange(desc(n)) |>
  ungroup() |>
  mutate(percentage = round(n / sum(n) * 100, 2)) |>
  head(10)
# looks like there are many entries where the location was not recorded
# around 17% of entries do not have a location recorded, will need to decide what to do with this
# otherwise, given that the next 

## collisions by month, descending order
collisions_cleaned |>
  group_by(occ_month) |>
  count() |>
  arrange(desc(n)) |>
  ungroup() |>
  mutate(percentage = round(n / sum(n) * 100, 2)) |>
  head(6)
# the first three months of the year and the last three months of the year make up the 6 months with the most collisions
# winter weather perhaps?
# all percentages are fairly close, there isn't one clear runaway month

## collisions by hour, descending order
collisions_cleaned |>
  group_by(occ_hour) |>
  count() |>
  arrange(desc(n)) |>
  ungroup() |>
  mutate(percentage = round(n / sum(n) * 100, 2)) |>
  head(6)
# a standard time that has 1-12 + AM/PM, rather than 24-hour time, might be a good addition here
# looks like most accidents occur in the afternoon/evening, not overnight or in the morning

## are there specific dates that have the most accidents
## collisions by month, descending order
collisions_cleaned |>
  group_by(occ_date) |>
  count() |>
  arrange(desc(n)) |>
  ungroup() |>
  mutate(percentage = round(n / sum(n) * 100, 2)) |>
  head(10)
# percentages are all small but there were nearly twice the number of accidents on day #1 as on day #10 so that's something?

## make a long version of the data to pivot mode of transportation
collisions_long <- collisions_cleaned |>
  pivot_longer(
    cols = c(automobile, motorcycle, passenger, bicycle, pedestrian), 
    names_to = "vehicle_type", 
    values_to = "involved_yesno"
)

## use the long data to group accidents by vehicle involved
collisions_long |>
  group_by(vehicle_type) |>
  count(involved_yesno) |>
  filter(involved_yesno == "YES") |>
  arrange(desc(n))
# far more cars involved in accidents than any other type of vehicle, to be expected
# motorcycles in not that many accidents (surprising)

#################### Space for Drafting Proposal Visuals #######################
## bar chart for how many vehicles of each type were involved in collisions
# make a data frame to hold that summary data
counts_by_vehicle_type <- collisions_long |>
  group_by(vehicle_type) |>
  count(involved_yesno)

# trying a faceted bar chart
counts_by_vehicle_type |>
  ggplot(aes(x = involved_yesno, y = n)) +
  geom_col() +
  facet_wrap(vars(vehicle_type))

# trying another faceted bar chart
counts_by_vehicle_type |>
  filter((involved_yesno == "YES") | (involved_yesno == "NO")) |>
  ggplot(aes(x = vehicle_type, y = n)) +
  geom_col() +
  facet_wrap(vars(involved_yesno))

# trying a side by stacked bar chart
counts_by_vehicle_type |>
  ggplot(aes(x = involved_yesno, y = n, fill = vehicle_type)) +
  geom_col()

# trying another stacked bar
counts_by_vehicle_type |>
  filter((involved_yesno == "YES") | (involved_yesno == "NO")) |>
  ggplot(aes(x = vehicle_type, y = n, fill = involved_yesno)) +
  geom_col()+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.9, hjust=1))+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_taylor_d(album = "Midnights", labels = c('Yes', 'No'))+
  labs(
    x = "Vehicle Type",
    y = "Number of Accidents",
    title = "Whether Each Vehicle Type was in Each Reported Accident",
    fill = "Involved in Crash"
  )

## bar charts for crashes per month, per hour, per year, etc
# crashes in each month, by month number
collisions_cleaned |> # use the regular wide data since we want individual crashes not individual vehicles involved in crashes
  ggplot(aes(x = factor(occ_month_num), fill = as.factor(occ_month_num)))+
  geom_bar()+
  scale_fill_paletteer_d("ggthemes::Classic_Cyclic", guide = "none")+
  labs(
    x = "Month",
    y = "Number of Accidents Responded To", 
    title = "Accidents Per Month from 2020-2026"
  )

# crashes in each month, by month name
collisions_cleaned |> # use the regular wide data since we want individual crashes not individual vehicles involved in crashes
  ggplot(aes(x = factor(occ_month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")), fill = as.factor(occ_month)))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 55, vjust = 0.9, hjust=1))+
  scale_fill_paletteer_d("ggthemes::Classic_Cyclic", guide = "none")+
  labs(
    x = "Month",
    y = "Number of Accidents Responded To", 
    title = "Accidents Per Month from 2020-2026"
  )

# figure as modified in class week 11
# changes to visual 1
##### none, ran out of time
collisions_cleaned |> # use the regular wide data since we want individual crashes not individual vehicles involved in crashes
  ggplot(aes(x = factor(occ_month, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")), fill = as.factor(occ_month)))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 55, vjust = 0.9, hjust=1))+
  scale_fill_manual(values = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"), guide = "none")+
  labs(
    x = "Month",
    y = "Number of Accidents Responded To", 
    title = "Accidents Per Month from 2020-2026"
  )