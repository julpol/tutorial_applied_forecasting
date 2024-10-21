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


# P.7
train.ridership <- ridership |> filter_index(~ "2001 Mar") 
valid.ridership <- ridership |> filter_index("2001 Apr" ~ .)


## Linear trend
train.lm <- train.ridership |> model( TSLM( Ridership ~ trend() ) )

train.ridership |>  
  autoplot(Ridership) +  
  autolayer(fitted.values(train.lm), colour = "blue1", size = 1.2) +  
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") + 
  labs(x = "Time")

fc.lm <- train.lm |> forecast(h = 36)

train.ridership |>  
  autoplot(Ridership) +  
  autolayer(fitted.values(train.lm), colour = "blue1", size = 1.2) +  
  geom_line(aes(y = .mean), data = fc.lm, colour = "blue1", linetype = "dashed", size = 1.2) +  	scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +  
  labs(x = "Time") +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size=0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, 
                   xend = yearmonth("2004-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color = "grey55")+  
  annotate(geom = "text", x = yearmonth("2002-Aug"), y=2290, label = "Validation", color = "grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, 
                   xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "grey55")+  
  annotate(geom="text", x=yearmonth("1996-Aug"), y = 2290, label = "Training", color = "grey37")
  

report(train.lm)











#p.11
###Exponential trend
train.lm <- train.ridership |>  
  model(    linear = TSLM( Ridership ~ trend() ),    
            exponential = TSLM( log(Ridership) ~ trend() )    
  )
fc_trends <- train.lm |> forecast(h = 36)

ridership |>  autoplot(Ridership) +  
  geom_line(data = fitted(train.lm), aes(y = .fitted, colour = .model, linetype = .model)) +  	autolayer(fc_trends, level = NULL, linetype = "dashed") +  
  labs(x = "Time") +  
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")




#p. 13
### quadratic (U-shate) trend 

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



# p.19 - Model with seasonality

train.lm.season <- train.ridership |> model(TSLM(Ridership ~ season()))
report(train.lm.season)


pred.values.train.season <- fitted.values(train.lm.season) # forecasts in training
fc.lm.season <- train.lm.season %>% forecast(h=36)    # forecasts in validation
fc.resid.season <- valid.ridership$Ridership - fc.lm.season$.mean  # errors in validation

fc.resid.season <- data.frame(valid.ridership$Month, fc.resid.season) 
colnames(fc.resid.season)[1] <- "Month"
fc.resid.season <- fc.resid.season %>%
  as_tsibble(index = Month)


p.model <- ridership  %>%
  autoplot(Ridership) + 
  geom_line(aes(y=.mean), data = fc.lm.season,  colour="blue1", linetype = "dashed", size = 1) +
  autolayer( pred.values.train.season,  level = NULL, colour="blue1", size = 1) +
  xlab("Time") + ylab("Ridership")  +
  theme(legend.position = "none") +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size=0.6) +
  geom_segment(aes(x = yearmonth("2001-May"), y = 2250, 
                   xend = yearmonth("2004-May"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both") , color="grey55")+  
  annotate(geom="text", x=yearmonth("2003-Jan"), y=2300, label="Validation", color="grey37") +
  geom_segment(aes(x = yearmonth("1991-Jan"), y = 2250, 
                   xend = yearmonth("2001-Mar"), yend = 2250),
               arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color="grey55")+  
  annotate(geom="text", x=yearmonth("1995-Aug"), y=2300, label="Training", color="grey37") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")

p.resid <- train.lm.season %>% augment() %>% 
  autoplot(.resid) +
  autolayer(fc.resid.season ,  level = NULL)+
  xlab("Time") + ylab("Residuals")  +
  theme(legend.position = "none") +
  geom_vline(xintercept= as.numeric(as.Date(yearmonth("2001-April"))), linetype="solid", color = "grey55", size=0.6)+
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")


grid.arrange(p.model, p.resid , nrow = 2)




# P. 23
train.lm.trend.season <- train.ridership |>        
  model(TSLM(Ridership ~ trend() + I(trend()^2) + season()))

report(train.lm.trend.season)






