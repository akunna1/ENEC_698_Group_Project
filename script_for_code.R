# Set working directory to Source File Location
# The CSV files from my desktop

# Load libraries
library(tidyverse)  # dealing with tabular data
library(lubridate) # handling dates
library(dplyr) # data manipulation
library(tidyr) # tidy data
library(data.table) # table formatted result
library(ggplot2) # visualization
library(scales)# graphical scaling

# Importing my .csv data files to the Global Environment
eui_data_2019 = read_csv("eui2019_ake.csv")
br_data_2019 = read_csv("buildings_and_renos_2021_ake.csv")

eui_data_joined = left_join(eui_data_2019, br_data_2019, by="Building_number")

view(eui_data_joined) # contains 252 entries

#removing NAs in Steam_in_klb, Chilled_water_ton_hr and EUI_in_mmbtu_per_sqft columns
eui_data_steam <- filter(eui_data_joined, !is.na(eui_data_joined$Steam_in_klb))
eui_data_sc <- filter(eui_data_steam, !is.na(eui_data_steam$Chilled_water_ton_hr))
eui_data_sce <- filter(eui_data_sc, !is.na(eui_data_sc$EUI_in_mmbtu_per_sqft))
view(eui_data_sce) #gives 126 entries

#Grouping joined data (unfiltered) by property type and doing a count
property_type_count <- eui_data_joined%>%
  group_by(Property_type) %>%
  summarise(Total = n()) # grouping the joined data into property type by count
view(property_type_count)

#Grouping sce data  by property type and doing a count
#sce- steam,chilled water and eui
property_type_count_sce <- eui_data_sce%>%
  group_by(Property_type) %>%
  summarise(Total = n()) # grouping the steam data into property type by count
view(property_type_count_sce)

#Boxplots
#boxplot1: Steam
qplot(x=eui_data_sce$Property_type, y=eui_data_sce$Steam_in_klb, geom="boxplot", main="Boxplot of Steam (klb) by Property type", ylab="Steam(klb)", xlab="Property Type", col=I('black'), fill=eui_data_sce$Property_type) + 
  coord_flip()+
  theme(legend.position="none") #to remove legend
  
#boxplot2: Chilled Water
qplot(x=eui_data_sce$Property_type, y=eui_data_sce$Chilled_water_ton_hr, geom="boxplot", main="Boxplot of Chilled Water (ton-hr) by Property type", ylab="Chilled Water (ton-hr)", xlab="Property Type", col=I('black'), fill=eui_data_sce$Property_type) + 
  coord_flip()+
  theme(legend.position="none") #to remove legend

#boxplot3: EUI
qplot(x=eui_data_sce$Property_type, y=eui_data_sce$EUI_in_mmbtu_per_sqft, geom="boxplot", main="Boxplot of EUI (MMBTU/Sqft) by Property type", ylab="EUI (MMBTU/Sqft)", xlab="Property Type", col=I('black'), fill=eui_data_sce$Property_type) + 
  coord_flip()+
  theme(legend.position="none") #to remove legend

#_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________
#Line Graph for Year Occupied Vs. Year of Latest Renovation
#Filtering to remove rows with NAs for columns Year_occupied and Latest_reno
eui_data_n1 <- filter(eui_data_sce, !is.na(eui_data_sce$Year_occupied))
view(eui_data_n1) #gives 123 entries

eui_data_n2 <- filter(eui_data_n1, !is.na(eui_data_n1$Latest_reno))
view(eui_data_n2) #gives 74 entries

#Plotting
ggplot()+
  geom_point(data = eui_data_n2, se=FALSE, color='black', mapping = aes(x=Year_occupied, y=Latest_reno))+
  labs(x='Year Occupied', y='Year of Latest Renovation', title='Latest Renovation Year Vs. Occupancy Year')
#_____________________________________________________________________________________________________________________________________________________________________________________________________________________________________
