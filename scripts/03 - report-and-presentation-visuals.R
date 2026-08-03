################################### Preamble ###################################
# Purpose: A space for bringing together the work done in scripts 01 and the models
#           generated in script 02 to generate visuals for use in this project's
#           report and presentation files.
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
library(paletteer)

########################### Read In the Dataset ################################
collisions <- read_csv(here("data/02 - analysis data", "working-data-2020-2026.csv"))

# landing page for working on steps for the project

## visuals (4-6 minimum)
## raw data visuals
# 3x facet wrapped line graphs of accidents per month/day/hour (thus 3 figures) based on type of transportation
# 1x summary table listing the highest risk month, day, and hour for each type of transportation

## model visuals
# 3x line graphs - predicted probabilities from models, time unit (3 of these, so 3 graphs) on x axis, predicted probability on y axis, type of transportation (aka model) as legend
# 1x table with the transportation modes/models as columns, time units as rows, model accuracy as values. will involve grouping responses into an estimated binary to compare with the actual binary

## models
# 5 versions of the same model
# one for each type of transportation 
# predictors are month, day of week, hour of day






