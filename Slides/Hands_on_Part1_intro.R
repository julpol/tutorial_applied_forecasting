##################
# Load packages
###################

library(here)  # Works only inside of a project. Set the pathway to find your files in the project.
library(rlang)
library(fabletools) #  Provides tools, helpers and data structures for developing models and time series functions for 'fable' and extension packages.
library(fable) # provides common forecasting methods for tsibble, such as ARIMA and ETS. 
library(tsibble) # provides a data infrastructure for tidy temporal data with wrangling tools.
library(feasts) # provides support for visualizing data and extracting time series features.
library(slider) 
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra) # for arranging multi-panel plots
library(readr)  # for saving csv
library(scales) # for changing time x-axis format


######################################
sessionInfo()  # to see which package version is loaded
#####################################

########################################
########################################
# Hands 0n 
########################################
########################################

DepartmentStoreSales <- read.csv("Data/DepartmentStoreSales.csv")
DepartmentStoreSales <- read.csv("C:/Dropbox/UoM/Talks/Wombat2024/WorkshopSlides/Rcode/Data/DepartmentStoreSales.csv")


DepartmentStoreSales <- DepartmentStoreSales |>
  mutate(Quarter = 	yearquarter(as.character(DepartmentStoreSales$Quarter))) |>
  as_tsibble(index = Quarter)

DepartmentStoreSales |>
  autoplot(Sales) +
  xlab("Quarter") + ylab("Sales ($)") +
  ggtitle("Department Store Sales (Quarterly)")











