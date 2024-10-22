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
#library(stringr) # for replacing strings in text (e.g., Problem 9 Ch 5)
#library(GGally)  # for scatter plot matrix of many time series 
#library(ROCR)    # area under the curve in classification models

######################################
sessionInfo()  # to see which package version is loaded
#####################################



#########################
########################
# Hands on
########################
########################

#### Hands- on 3.1  ####
# Recreate the plot of quadratic trend


train.lm.poly.trend <- train.ridership |>  model( quadratic = TSLM(Ridership ~ trend() + I(trend()^2))) 

report(train.lm.poly.trend)

fc_quadratic <- train.lm.poly.trend |> forecast(h = 36)

ridership |>  autoplot(Ridership) +  
  geom_line(data = fitted(train.lm.poly.trend), aes(y = .fitted, colour = .model, linetype = .model) , size = 1.2) +  	
  autolayer(fc_quadratic, level = NULL, linetype = "dashed", size = 1.2) +  
  labs(x = "Time") +  
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size=0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, 
                   xend = yearmonth("2004-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color = "grey55")+  
  annotate(geom = "text", x = yearmonth("2002-Aug"), y=2290, label = "Validation", color = "grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, 
                   xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55")+  
  annotate(geom="text", x=yearmonth("1996-Aug"), y = 2290, label = "Training", color = "grey37")






######### Hands-on 3.2 ############

ToysRUsRevenues <- read.csv("Data/ToysRUsRevenues.csv")
ToysRUsRevenues <- read.csv("C:/Dropbox/UoM/Talks/Wombat2024/WorkshopSlides/Rcode/Data/ToysRUsRevenues.csv")


ToysRUsRevenues <- ToysRUsRevenues |>
  dplyr::mutate(Quarter = yearquarter(as.character( ToysRUsRevenues$Quarter)) ) %>%
  as_tsibble(index = Quarter)

# a - regression model
ToysRUs_train <- ToysRUsRevenues |> filter_index( ~ "1995 Q2")
ToysRUs_test  <- ToysRUsRevenues |> filter_index("1995 Q3" ~ .)

ToysRUs.reg <- ToysRUs_train |> model(TSLM( Revenues ~ season() + trend()))

report(ToysRUs.reg)

fc.ToysRUs.reg <- ToysRUs.reg  |> forecast(h = nrow(ToysRUs_test))

accuracy( ToysRUs.reg)
accuracy( fc.ToysRUs.reg, ToysRUsRevenues) 



