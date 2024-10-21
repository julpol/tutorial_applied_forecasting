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


Amtrak.data <- read.csv("Data/Amtrak data.csv")
Amtrak.data <- read.csv("C:/Dropbox/UoM/Talks/Wombat2024/WorkshopSlides/Rcode/Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character( Amtrak.data$Month)) ) |>
  as_tsibble(index = Month)

ridership |>  autoplot(Ridership) +  
  xlab("Time") + ylab("Ridership") 



# p.25
p.ridership <- ridership |>
  autoplot(Ridership) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) + 
  xlab("Time") + ylab("Ridership") 

p.ridership.zoom <- ridership |>
  filter_index("1997 Jan" ~ "2000 Dec") |>
  autoplot(Ridership) +
  xlab("Time") + ylab("Ridership")

grid.arrange(p.ridership, p.ridership.zoom, ncol = 1)




# p.32
ridership |>
  autoplot(Ridership) +
  xlab("Time") + ylab("Ridership")  +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size=0.6)+
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, 
                   xend = yearmonth("2004-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , size = 0.3, color = "grey55")+ 
  annotate(geom = "text", x = yearmonth("2002-Aug"), y = 2280, label = "Validation", color = "grey37") +
  
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, 
                   xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), size = 0.3, color = "grey55")+ 
  annotate(geom="text", x = yearmonth("1996-Aug"), y = 2280, label = "Training", color = "grey37")+
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")













