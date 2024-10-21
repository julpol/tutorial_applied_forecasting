
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

Amtrak.data <- read.csv("Data/Amtrak data.csv")
Amtrak.data <- read.csv("C:/Dropbox/UoM/Talks/Wombat2024/WorkshopSlides/Rcode/Data/Amtrak data.csv")

ridership <- Amtrak.data |>
  mutate(Month = yearmonth(as.character( Amtrak.data$Month)) ) |>
  as_tsibble(index = Month)




# p.7
ridership.24 <- ridership |> filter_index(~ "1992 Dec") 

ridership.24 |> ACF(Ridership, lag_max = 18) |>  
  autoplot() +   
  xlab("Lag") +   
  scale_x_continuous(breaks = seq(0, 18, by = 1)) 

ridership.24 |> autoplot()


# p.9
train.lm.trend.season <- train.ridership |> 
  model(TSLM( Ridership ~ trend() + I(trend()^2) + season()))

train.lm.trend.season |> residuals() |> ACF(.resid, lag_max = 18) |> 
  autoplot() + 
  xlab("Lag")




###############
# Souvenir Sales example, p.18
################

SouvenirSales <- read.csv("Data/SouvenirSales.csv")
SouvenirSales <- read.csv("C:/Dropbox/UoM/Talks/Wombat2024/WorkshopSlides/Rcode/Data/SouvenirSales.csv")

SouvenirSales <- SouvenirSales |>
  dplyr::mutate(Month = yearmonth(as.character(SouvenirSales$Month)) ) |>
  as_tsibble(index = Month)

SouvenirSales.train <- SouvenirSales |> filter_index(~ "2000 Dec") 
SouvenirSales.valid  <- SouvenirSales |> filter_index("2001 Jan" ~ .)

### Plotting

SouvenirSales |> autoplot()

## ARIMA models
# run auto ARIMA() on Sales
fit <- SouvenirSales.train |>
  model(
    model.arima = ARIMA(Sales),
    model.arima.log = ARIMA(log(Sales))
  )

fit |> select(model.arima) |> report()

fit |> select(model.arima.log) |> report()

fc <- fit |>  forecast(h=12) 

fc |> View()


# accuracy (on validation period)
accuracy(fc, SouvenirSales.valid)


# Ploting

# Compute validation forecast errors
# Add errors column to fc2
fc2 <- fc |> group_by(.model) |>
  left_join(SouvenirSales.valid, by = "Month") |>
  mutate(fc.error = Sales.y - .mean)

# Plot 1: actual and forecasts
p1 <- autoplot(SouvenirSales, Sales) +
  autolayer(fitted(fit), .fitted, alpha = 0.7) +
  autolayer(fc2, .mean, linetype = "dashed", level = NULL) +
  labs(title = "Sales and Forecasts", x = "Month", y = "Sales")

# Plot 2: errors
p2 <- autoplot(fc2, series="fc.error", linetype = "dashed") +
  autolayer(resid(fit), .resid) +
  labs(title = "Errors", x = "Month", y = "Error")

grid.arrange(p1, p2 , nrow = 2)






