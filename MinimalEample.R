##########
# Minimal example
##########

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
#library(stringr) # for replacing strings in text (e.g., Problem 9 Ch 5)
#library(GGally)  # for scatter plot matrix of many time series 
#library(ROCR)    # area under the curve in classification models

######################################
sessionInfo()  # to see which package version is loaded
#####################################

Amtrak.data <- read.csv("Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character( Amtrak.data$Month)) ) |>
  as_tsibble(index = Month)

lag_data <- ridership |>     mutate(dif1 =  Ridership - lag(Ridership, 1),    
                                    dif12 =  Ridership - lag(Ridership, 12),           
                                    dif12_1 =  dif12 - lag(dif12, 1))

train.ridership <- lag_data |> filter_index("1992 Feb" ~ "2001 Mar") 
valid.ridership <- lag_data |> filter_index("2001 Apr" ~ .)

fit.ets <- train.ridership |>     model(ets =  ETS(dif12_1 ~ error("A") + trend("N", alpha = 0.2) + season("N")))

fc.ets <- fit.ets |> forecast(h=dim(valid.ridership)[1])

fc.ets |>     
  autoplot(train.ridership, level = NULL, size = 1, linetype = "dashed") +    
  geom_line(aes(y = dif12_1), data = lag_data, size = 0.5) +     
  geom_line(aes(y = .fitted), data = fitted.values(fit.ets), size = 1, colour = "blue1") +  
  xlab("Year") + ylab("Twice-Differenced")


report(fit.ets)
fitted.values(fit.ets)
