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

#p.12
# Centered moving average and trailing moving average with window width w=12
############################################################################
ma.ridership <- ridership |>
  mutate(MAw12_12 = slide_dbl(Ridership, mean, .before = 5, .after = 6, .complete = TRUE),
         MAw12_2x12 = slide_dbl(MAw12_12, mean, .before = 1, .after = 0, .complete = TRUE))  |> # Centered moving average for even window
  mutate(MATw12 = slide_dbl(Ridership, mean,.before = 11, .after = 0, .complete = TRUE)) |>  # Trailing MA
  select(Month, Ridership, MAw12_2x12, MATw12)

colnames(ma.ridership)[c(3,4)] <- c("Centered Moving Average", "Trailing Moving Average")

# Create long table
ma.ridership <- ma.ridership |> gather(model, value, Ridership:`Trailing Moving Average`)

ma.ridership |> ggplot(aes(x=Month, y=value)) + 
  geom_line(aes(color = model, linetype = model, size = model) ) +
  scale_size_manual(values=c(1.2 , 0.4, 1.2)) + 
  scale_linetype_manual(values=c("dotdash", "solid","longdash"))+
  scale_color_manual(values=c( "blue1", "black","coral"))+
  theme(legend.position="bottom") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +  
  labs(x = "Time", y = "Ridership")


#p.16
#Trailing moving average forecaster with w=12 applied to Amtrak ridership series.
############################################################################
 
train.ridership <- ridership |> filter_index(~ "2001 Mar") 
valid.ridership <- ridership |> filter_index("2001 Apr" ~ .)

ma.trailing <- train.ridership |>  
  mutate(MATw12 = slide_dbl(Ridership, mean, .before = 11, .complete = TRUE))

last.ma <- tail(ma.trailing, 1)
ma.pred <- bind_cols(valid.ridership, rep(last.ma$MATw12, nrow(valid.ridership))) |>     
  select(-"Ridership")
names(ma.pred)[2] <- 'fitted'

ridership |>  autoplot(Ridership) +  
	geom_line(aes(y = MATw12), data = ma.trailing, size = 1.0, colour = "blue1") +  
	autolayer(ma.pred, alpha = 0.5, level = NULL, linetype = "dashed", size = 1.25, colour = "blue1") + 
	 xlab("Time") + ylab("Ridership")  +  
	theme(legend.position = "none")





#p.22
# Differences of Ridership time series
lag_data <- ridership |> mutate(dif1=  Ridership - lag(Ridership,1),
                                dif12 =  Ridership - lag(Ridership,12),
                                dif12_1 =  dif12 - lag(dif12,1))


p1 <- lag_data  |>
  autoplot(Ridership) +
  xlab("Time") + ylab("Ridership") + labs(subtitle = "Ridership")+ #+ theme(plot.title = element_text(hjust = 0.5, size=8)))
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")

p2 <- lag_data  |>
  autoplot(dif12) +
  xlab("Time") + ylab("Ridership") + labs(subtitle = "Lag-12 Difference") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")


p3 <- lag_data  |>
  autoplot(dif1) +
  xlab("Time") + ylab("Ridership") + labs(subtitle = "Lag-1 Difference") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")


p4 <- lag_data  |>
  autoplot(dif12_1) +
  xlab("Time") + ylab("Ridership") + labs(subtitle = "Twice-Differenced (Lag-12, Lag-1)") +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")

grid.arrange(p1, p2, p3, p4, 
             ncol = 2, nrow = 2)











#p.30
#Exp Smoothing (α=0.2) applied to twice-differenced Amtrak series
############################################################################
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

# Extras not on the slide
report(fit.ets)
pred.values.ets <- fitted.values(fit.ets)
View(pred.values.ets)
fc.ets



#p.43
# ETS on ridership data 
############################################################################
train.ridership <- ridership |> filter_index( ~ "2001 Mar") 
valid.ridership <- ridership |> filter_index( "2001 Apr" ~ .)

fit.ets.HW <- train.ridership |> model(ets =  ETS(Ridership ~ error("M") + trend("A") + season("A")) ) 
report(fit.ets.HW)
pred.values.ets.HW <- fitted.values(fit.ets.HW)
View(pred.values.ets.HW)


fc.ets.HW <- fit.ets.HW |> forecast(h=dim(valid.ridership)[1])

ridership  |>
  autoplot(Ridership) +
  geom_line(aes(y=.mean), data = fc.ets.HW,  colour="blue1", linetype = "dashed") +
  autolayer(pred.values.ets.HW, alpha = 0.5, level = NULL, colour = "blue1") +
  xlab("Time") + ylab("Ridership")  +
  theme(legend.position = "none") +
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


n <- nrow(components(fit.ets.HW))
components(fit.ets.HW)[n, c("level", "slope")] # level and trend

t(components(fit.ets.HW)[(n-11):n, c("season")])  # s1 to s12            







# p.50
# automated ETS
fit.ets.auto <- train.ridership |> model(ets =  ETS(Ridership)) 
pred.values.ets.auto <- fitted.values(fit.ets.auto)
fc.ets.auto <- fit.ets.auto |> forecast(h=dim(valid.ridership)[1])
report(fit.ets.auto)

ridership  |>
  autoplot(Ridership) +
  geom_line(aes(y=.mean), data = fc.ets.auto,  colour="blue1", linetype = "dashed") +
  autolayer(pred.values.ets.auto, alpha = 0.5, level = NULL, colour = "blue1") +
  xlab("Time") + ylab("Ridership")  +
  theme(legend.position = "none") +
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




#p.56
######## Accuracy examination on RADIRSHIP
fit.ets.HW <- train.ridership |>     
  model(ets = ETS(Ridership ~ error("M") + trend("A") + season("A")))

fc.ets.HW <- fit.ets.HW |> forecast(h = nrow(valid.ridership))

accuracy(fit.ets.HW)
accuracy( fc.ets.HW, ridership) 












