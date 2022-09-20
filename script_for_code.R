# Set working directory to Source File Location
# The CSV files from my desktop

# Load libraries
library(tidyverse)  # dealing with tabular data
library(lubridate) # handling dates
library(dplyr) # data manipulation
library(tidyr) # tidy data
library(data.table) # table formated result
library(ggplot2) # visualization
library(scales)# graphical scaling

# Importing my .csv data files to the Global Environment
eui_data_2019 = read_csv("eui2019_ake.csv")
br_data_2019 = read_csv("buildings_and_renos_2021_ake.csv")

eui_data_joined = left_join(eui_data_2019, br_data_2019, by="Building_number")

view(eui_data_joined) # contains 252 entries

#Filtering to remove rows with NAs
# Removing the rows of Year_occupied containing NAs
eui_data_n1 <- filter(eui_data_joined, !is.na(eui_data_joined$Year_occupied))
view(eui_data_n1) #gives 236 entries

# Removing the rows of column Latest_reno containing NAs
eui_data_n2 <- filter(eui_data_n1, !is.na(eui_data_n1$Latest_reno))
view(eui_data_n2) #gives 117 entries


#Plotting
ggplot()+
  geom_point(data = eui_data_n2, mapping = aes(x=Year_occupied, y=Latest_reno))
  # make correlation line later


#make box plot for steam, EUI and Chilled water. Do group_by property type