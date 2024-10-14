# Wombat 2024 tutorial: Applied forecasting

Tutorial on a practical approach to time-series forecasting with R. 

Time: Oct 22 1:30-5:00

Location: [Monash University (downtown), 750 Collins St, Melbourne](https://www.google.com/maps/place/Monash+College+City+Campus/@-37.8200186,144.9493095,15z/data=!4m6!3m5!1s0x6ad65d4dbf52da3d:0xe25a5625211a931e!8m2!3d-37.8200186!4d144.9493095!16s%2Fm%2F0kg1lss?entry=ttu&g_ep=EgoyMDI0MTAwOS4wIKXMDSoASAFQAw%3D%3D)


## Tutorial details

We will take a practical approach to time-series forecasting. We will discuss each forecasting process step, including exploring and visualising time series, and we will also discuss practical approaches to evaluating the performance of forecasting solutions. The tutorial will cover popular forecasting methods, including smoothing algorithms, regression models, and ARIMA. Finally, we will discuss automated forecasting and the advantages of assembly methods. The material covered in this tutorial is inspired by the book Practical Time Series Forecasting with R: A Hands-On Guide.

Background: Participants should have a basic working knowledge of R and a little experience with ggplot2. This tutorial has many ‘hands-on’ activities. It is strongly recommended that you attend with your own laptop. Please ensure you have installed Rstudio, R and the recommended list of packages before the tutorial starts. If you are unfamiliar with writing R code or with using Rstudio, consider working through the first two chapters of the [learnr](https://learnr.numbat.space/) materials.

## Course Schedule
**Time**	   **Topic**

1:30-2:30  	Introduction and you need to know about time series analysis [Part 1 slides]() 

2:30-3:00 	Exponential smoothing [Part 2 slides]() 

3:00-3:30 	BREAK

3:30-4:00	  Exponential smoothing [Part 2 slides]() 

4:00-4:30   Regression models [Part 3 slides]() 

4:30-5:00	ARIMA [Part 4 slides]() 


## Material
- Slides part 1
- Slides part 2
- Slides part 3
- Slides part 4
- Zip of material for hands-on activities

## Getting started
1. You should have a reasonably up-to-date R and R Studio version. In addition, please install the following packages and their dependencies *before the tutorial*.
```
install.packages(here, dependencies=c("Depends", "Imports"))         # Works only inside of a project. Set the pathway to find your files in the project.
install.packages(rlang, dependencies=c("Depends", "Imports"))
install.packages(fabletools, dependencies=c("Depends", "Imports"))   # Provides tools, helpers and data structures for developing models and time series functions for 'fable' and extension packages.
install.packages(fable, dependencies=c("Depends", "Imports"))        # Provides common forecasting methods for tsibble, such as ARIMA and ETS. 
install.packages(tsibble, dependencies=c("Depends", "Imports"))      # Provides a data infrastructure for tidy temporal data with wrangling tools.
install.packages(feasts, dependencies=c("Depends", "Imports"))       # Provides support for visualizing data and extracting time series features.
install.packages(slider, dependencies=c("Depends", "Imports")) 
install.packages(dplyr, dependencies=c("Depends", "Imports"))
install.packages(tidyr, dependencies=c("Depends", "Imports"))
install.packages(lubridate, dependencies=c("Depends", "Imports"))
install.packages(ggplot2, dependencies=c("Depends", "Imports"))
install.packages(gridExtra, dependencies=c("Depends", "Imports"))   # For arranging multi-panel plots
install.packages(readr, dependencies=c("Depends", "Imports"))       # For saving csv
install.packages(scales, dependencies=c("Depends", "Imports"))      # For changing time x-axis format
```


2. Run the test code to ensure you are ready for the tutorial with your working environment.
3. Download the Zip file with all the materials to your laptop and unzip it in the dedicated folder on your computer. You will have a few folders, like *Data* and *Rcode*, with some files required for the tutorial. Please keep this folder structure.
   

 








