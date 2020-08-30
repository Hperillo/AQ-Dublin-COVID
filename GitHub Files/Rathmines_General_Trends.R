rm(list = ls())

# Load Packages
library(dplyr)
library(plyr)
library(readr)
library(Hmisc)
library(psych)
library(rmweather)
library(ranger)
library(hexbin)
library(tidyr)
library(stringr)
library(ggplot2)
library(data.table)
library(lubridate)
library(Rmisc)
library(knitr)

setwd("~/Desktop/Research_Project/Data/AQ/Final_Data")
Rathmines <- read_csv('Rath_19_20.csv')

Rathmines$date <- dmy_hm(Rathmines$date)

Rathmines <- Rathmines %>%
  gather("PM", "NO2", "O3", key = 'Pollutant', value = 'value')

# Create Year, Month, Date, and Hour columns.
Rathmines <- Rathmines %>%
  mutate(Year = str_sub(date,3,4),
         Month = str_sub(date,6,7),
         Date = str_sub(date,9,10),
         Hour = str_sub (date, 12, 13))

Rathmines <- Rathmines %>%
  filter(Month <= '06')

Rathmines <- Rathmines %>%
  mutate(value = as.numeric(value))

Rathmines <- Rathmines[!is.na(Rathmines$value), ]

# Remove negative values
Rathmines <- Rathmines %>%
  filter(value >= 0)

# Reformat dataframe
Rathmines <- within(Rathmines, Date_1 <- paste(Month,Date,sep='-'))

Rathmines$Date_1 <- as.Date(Rathmines$Date_1, tryFormats = c("%m-%d"))

Rathmines$Year <- as.factor(Rathmines$Year)

Daily_Avgs_Rath <- summarySE(Rathmines, measurevar= "value", groupvars=c("Pollutant", "Date_1", "Year"))

# Line plot for weeks
Daily_Rathmines_plot <- ggplot(data = Daily_Avgs_Rath, aes(x = Date_1, y = value)) +
  geom_line(aes(color = Year)) +
  labs(x = 'Date',
       y = 'Daily Avg Concentration (µg/m^3)',
       color = "Year",
       title = "Daily Air Pollution Patterns at Rathmines",
       subtitle = "2019 and 2020")+
  facet_wrap(~ Pollutant, dir = 'v')+
  theme_bw()+
  theme(
    strip.text.x = element_text(margin = margin(2, 0, 2, 0), size = 12)
  )+
  geom_vline(xintercept=as.numeric(Daily_Avgs_Rath$Date_1[c(141)]),
             linetype=2, colour="black")
Daily_Rathmines_plot
