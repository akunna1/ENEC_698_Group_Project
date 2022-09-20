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
  geom_point(data = eui_data_n2, se=FALSE, color='black', mapping = aes(x=Year_occupied, y=Latest_reno))+
  geom_smooth(method = 'lm', data = eui_data_n2, mapping = aes(x=Year_occupied, y=Latest_reno))+
  labs(x='Year Occupied', y='Year of Latest Renovation', title='Latest Renovation Year Vs. Occupancy Year')


#Make box plot for steam, EUI and Chilled water. Do group_by property type

#Box Plot for steam
#removing NAs in Steam_in_klb column
eui_data_steam <- filter(eui_data_joined, !is.na(eui_data_joined$Steam_in_klb))
view(eui_data_steam) #gives 147 entries

#Grouping joined data (unfiltered) by property type
property_type_count <- eui_data_joined%>%
  group_by(Property_type) %>%
  summarise(Total = n()) # grouping the steam data into property type by count
view(property_type_count)

#Making tables for each property type- doing steam
# filter each data by property types- then from each table find the outliers.