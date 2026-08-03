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
library(paletteer)

########################### Read In the Dataset ################################
collisions <- read_csv(here("data/02 - analysis data", "working-data-2020-2026.csv"))

############################### Establish Goals ################################