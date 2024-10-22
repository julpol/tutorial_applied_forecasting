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




######################
######################
# Hands on activity # 2.1
######################
######################


AustralianWines <- read.csv("Data/AustralianWines.csv")
AustralianWines <- read.csv("C:/Dropbox/UoM/Talks/Wombat2024/WorkshopSlides/Rcode/Data/AustralianWines.csv")

AustralianWines <- AustralianWines |>
  dplyr::mutate(Month = yearmonth(as.character(AustralianWines$Month)) ) |>
  as_tsibble(index = Month)

AustralianWines.train <- AustralianWines |> filter_index( ~ "1993 Dec") 
AustralianWines.test  <- AustralianWines |> filter_index( "1994 Jan" ~ .)



# Fit moving average to fortified wine: windoe =5 and window = 12
fit_wine_MA <- AustralianWines.train |> 
                  mutate(MAw12_12 = slide_dbl(Fortified, mean, .before = 5, .after = 6, 
                                              .complete = TRUE),     
                         MAw12_2x12 = slide_dbl(MAw12_12 , mean, .before = 1, .after = 0, 
                                                .complete = TRUE)) |>    #Centered  
                  mutate(MAw7 = slide_dbl(Fortified, mean, .before = 2, .after = 2, .complete = TRUE)) |> 
                  select(Month, Fortified, MAw12_2x12, MAw7)


colnames(fit_wine_MA)[c(3,4)] <- c("MA12", "MA5")

fit_wine_MA <- fit_wine_MA |> gather(model, value, Fortified:MA5)

fit_wine_MA |> ggplot(aes(x = Month, y = value)) +   
  geom_line(aes(color = model, linetype = model, size = model) ) +  
  scale_size_manual(values = c(0.8 , 0.8, 0.8)) +   
  scale_linetype_manual(values = c("dotdash", "solid", "longdash")) +  
  theme(legend.position = "bottom") +  
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +  
  labs(x = "Time", y = "Fortified")









######################
######################
# 2.2
######################
######################

# Apply Holt-Winter’s exponential smoothing (with multiplicative seasonality) to sales.
fit_wine_HW <- AustralianWines.train |> 
  model(ets =  ETS(Fortified ~ season("M")) ) 
report(fit_wine_HW)


















