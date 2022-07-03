git config --global user.email "ncsu.nickmo@gmail.com"
git config --global user.name "Nick"

#theme for rmarkdown
install.packages("prettydoc")
library(prettydoc) 

#Essential library kits
library(pacman)
library(geosphere)
library(xlsx)
library(magrittr)
library(ggpol)
library(lubridate)
library(janitor)
library(dplyr)
library(GGally)
library(ggplot2)
library(ggthemes)
library(rmarkdown)
library(shiny)
library(stringr)
library(tidyr)
library(skimr)
library(scales)
library(reshape2)
library(ggthemes)

#setting directory to import files
print(getwd())
#i had to flip all of the dashes 
('C:/Users/Siggi/Downloads/BikeShareGit')


April2021 <- read.csv('202103_Apr_21.csv')
May2021 <- read.csv('202104_May_21.csv')
June2021 <- read.csv('202105_Jun_21.csv')
July2021 <- read.csv('202106_Jul_21.csv')
August2021 <- read.csv('202107_Aug_21.csv')
September021 <- read.csv('202108_Sep_21.csv')
October2021 <- read.csv('202109_Oct_21.csv')
November2021 <- read.csv('202110_Nov_21.csv')
December2021 <- read.csv('202111_Dec_21.csv')
January2022 <- read.csv('202112_Jan_22.csv')
February2022 <- read.csv('202201_Feb_22.csv')
March2022 <- read.csv('202202_Mar_22.csv')


bike_data <- bind_rows(April2021, May2021, June2021, July2021, August2021, September021, October2021, November2021, December2021, January2022, February2022, March2022)
head(bike_data)

#newly learned, saving df to compress it
#this was still too large to open when compressed :/
getwd()
save(bike_data, file = "C:/Users/Siggi/Downloads/BikeShareGit")

#mutating columns to be the right data type
bike_data <- mutate(bike_data, end_station_id = as.character(end_station_id),
   start_station_id = as.character(start_station_id))
 
#viewing class type of single column     
class(bike_data$ride_id)
#viewing all variable class types   
str(bike_data)
          
#making a new weekday column WITHOUT time         
bike_data$weekday <- as.Date(bike_data$started_at)

#This code snippet will add new columns to the code
#(ride duration, month of the year, day of the week, hour of day)
bike_data_v2<- bike_data %>%
  mutate(ride_duration = as.numeric(difftime(ended_at, started_at, unit="mins"))) %>%
  mutate(ride_duration_hours = as.numeric(difftime(ended_at, started_at, unit="hours"))) %>% 
  mutate(ride_distance = distHaversine(cbind(start_lng, start_lat), cbind(end_lng, end_lat))) %>%
  mutate(ride_year = year(started_at)) %>%
  mutate(ride_month = month(started_at, label = TRUE)) %>%
  mutate(day_of_week = weekdays(weekday)) %>%   
  mutate(hour_of_day = hour(started_at))

#to calculate number of NA values
sum(is.na(bike_data_v2))


#deleting all NA values
bike_data_v3 <- na.omit(bike_data_v2)

#checking that NA values are now zero
sum(is.na(bike_data_v3))




#with janitor removing blank cells (I think this worked best for numeric)
bike_data_v3 %>% remove_empty("rows")
#this got rid of pesky empty cells in station columns
bike_data_v3 <- with(bike_data_v3, bike_data_v3[!(start_station_id == "" | is.na(start_station_id)), ])
bike_data_v3 <- with(bike_data_v3, bike_data_v3[!(end_station_id == "" | is.na(end_station_id)), ])


#BUT THIS IS HOW TO DELETE ROWS WITH CONDITIONS
#I deleted numeric values in a numeric column under 15 seconds (.25min)!
bike_data_v4 <- bike_data_v3[!(bike_data_v3$ride_duration<0.25),]
skim(bike_data_v4)

#I did not delete the ride_distance of 0 because those could have been people who left and returned to the same station

#general summary info
summary(bike_data_v4$ride_duration)

#I run this before making my graph. This will prevent the y-axis from showing scientific notation numbers
options(scipen=5)

#applying the font type I would like for my figures
windowsFonts(A = windowsFont("Times New Roman"))

#FIGURE 1 #bar graph for casual vs. member weekday ride amounts YAY DONE
ggplot(data=bike_data_v4)+
  geom_bar(mapping = aes(x=day_of_week,fill=member_casual))+
  scale_x_discrete(limits = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),
                   labels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))+ #the discrete lines show how I rearranges the x-axis labels to go from sun-sat and be renamed to shorter text
  facet_wrap(~member_casual)+ 
  theme(strip.text = element_blank())+
  labs(title="Weekly Ride Amounts by Cyclist Type", subtitle="Casual Riders vs. Members",
       x = "Day of the Week",
       y = "Number of Overall Rides",
       caption="Data Provided by Google Capstone Project")+
  scale_y_continuous(limits=c(0,500000), labels = scales::comma)+ #this combined scale_y_continuous(limits=c(0,500000)  AND scale_y_continuous(labels=comma)
  theme_fivethirtyeight(base_size = 12, base_family = "A")+
  labs(fill='Rider Type')+
  theme(axis.title = element_text(face="bold"), strip.text = element_blank())

#FIGURE 2 #bar graph for casual vs. member monthly ride amounts YAY DONE
ggplot(data=bike_data_v4)+
  geom_bar(mapping = aes(x=ride_month,fill=member_casual))+
  scale_x_discrete(limits = c("Mar","Apr","May","Jun","Jul","Aug","Sep", "Oct", "Nov", "Dec", "Jan", "Feb"),
                   labels = c("Mar21","Apr21","May21","Jun21","Jul21","Aug21","Sep21", "Oct21", "Nov21", "Dec21", "Jan22", "Feb22"))+ #the discrete lines show how I rearranges the x-axis labels to go from sun-sat and be renamed to shorter text
  facet_wrap(~member_casual)+ 
  theme(strip.text = element_blank())+
  labs(title="Monthly Ride Amounts by Cyclist Type", subtitle="Casual Riders vs. Members",
       x = "Month",
       y = "Number of Overall Rides",
       caption="Data Provided by Google Capstone Project")+
  scale_y_continuous(limits=c(0,400000), labels = scales::comma)+ #this combined scale_y_continuous(limits=c(0,500000)  AND scale_y_continuous(labels=comma)
  theme_fivethirtyeight(base_size = 12, base_family = "A")+
  labs(fill='Rider Type')+
  theme(axis.title = element_text(face="bold"), strip.text = element_blank())






#for figure 3
#maybe i need to add these to bike_data_v4 as new column?
mean(bike_data_v4[bike_data_v4$member_casual == 'member', 'ride_duration'])

#for figure 3
#maybe i need to add these to bike_data_v4 as new column?
mean(bike_data_v4[bike_data_v4$member_casual == 'casual', 'ride_duration'])

#WORKING
#FIGURE 3 (i want to find avg ride duration for member vs casual)
ggplot(data=bike_data_v4, aes(x = member_casual, y = ride_duration)) + 
  stat_summary(geom = "bar", fun.y = "mean")+
  coord_flip()+
  scale_x_discrete(limits = c("member","casual"),
                   labels = c("Member", "Casual"))+
  labs(title="Average Ride Duration by Cyclist Type", subtitle="Casual Riders vs. Members",
       x = "Rider Type",
       y = "Average Ride Duration (in minutes)",
       caption="Data Provided by Google Capstone Project")+
  theme_fivethirtyeight(base_size = 12, base_family = "A")+
  theme(axis.title = element_text(face="bold"), strip.text = element_blank())

#WORKING
bike_data_test %>% 
  group_by(member_casual) %>% 
  summarise(mean = mean(ride_duration)) %>% 
  ggplot(aes(x = member_casual, y = mean)) + 
  geom_col(fill = "cornflowerblue")+
  theme(panel.grid.major.x=element_blank())



options(scipen=6)
#testing
ggplot(data = bike_data_v4, aes(x = member_casual, fill = rideable_type)) +
  geom_bar(position = position_dodge()) +
  labs(title="Usage of Different Bike Types by Riders",
       x = "Rider Type", y = "Count") +
  scale_y_continuous(breaks = c(0,500000, 1000000, 1500000, 2000000, 2500000), labels = scales::comma)+
  
  
  
#good to know this for future reference graphs (show y-axis as unit in millions)
scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))
  
#testing
ggplot(data = bike_data_v4, aes(x = member_casual, y = ride_duration, fill = member_casual)) +
    geom_bar(stat = "summary", fun = mean)

#testing
ggplot(data = bike_data_v4, aes(x = day_of_week, y = ride_duration, fill = member_casual)) +
  geom_bar(stat = "summary", fun = mean, position = "dodge")+
  scale_x_discrete(limits = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),
                  labels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))


#testing
ggplot(data = bike_data_v4, aes(x = ride_month, y = ride_duration, fill = member_casual)) +
  geom_bar(stat = "summary", fun = mean, position = "dodge")+
  scale_x_discrete(limits = c("Mar","Apr","May","Jun","Jul","Aug","Sep", "Oct", "Nov", "Dec", "Jan", "Feb"),
                   labels = c("Mar21","Apr21","May21","Jun21","Jul21","Aug21","Sep21", "Oct21", "Nov21", "Dec21", "Jan22", "Feb22"))






#new errors?
#bar graph for casual vs. member monthly ride amounts (i want to just make simple count of monthly rides for casual vs. members)
ggplot(data=bike_data_v4)+
  geom_bar(mapping = aes(x = ride_month, fill = member_casual))+
  scale_x_discrete(limits = c("Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov","Dec","Jan","Feb","Mar"),
                   labels = c("Apr21","May21","Jun21","Jul21","Aug21","Sep21","Oct21", "Nov21","Dec21","Jan22","Feb22","Mar22"))+
  theme(strip.text = element_blank())+
  labs(title="Monthly Ride Amounts by Cyclist Type", subtitle="April 2021 - March 2022",
       x = "Month",
       y = "Number of Overall Rides",
       caption="Data Provided by Google Capstone Project")+
  scale_y_continuous(breaks = c(0,100000, 200000, 300000, 400000, 500000, 600000, 700000), labels = scales::comma)+ #this combined scale_y_continuous(limits=c(0,500000)  AND scale_y_continuous(labels=comma)
  theme_fivethirtyeight(base_size = 12, base_family = "Times")+
  labs(fill='Rider Type')+
  theme(axis.title = element_text(face="bold"), strip.text = element_blank())









#removing columns that are unecessary for graph
bike_data_removedcols <- select(bike_data_v4, -c(ride_id:end_lng, weekday, ride_distance, ride_year, day_of_week, hour_of_day, ride_duration))

#grouping the y and x axis's & then putting the sum of ride_duration next to those two groupings                 
bike_data_removedcols_V2 <- bike_data_removedcols %>% 
  group_by(member_casual, ride_month) %>%
  summarise(ride_duration_sum=sum(ride_duration_hours))

#yay done                
ggplot(data = bike_data_removedcols_V2, aes(ride_month, ride_duration_sum, fill=member_casual, group = member_casual))+
  geom_col(position = position_dodge())+
  scale_x_discrete(limits = c("Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov","Dec","Jan","Feb","Mar"),
                   labels = c("Apr21","May21","Jun21","Jul21","Aug21","Sep21","Oct21", "Nov21","Dec21","Jan22","Feb22","Mar22"))+
  theme(strip.text = element_blank())+
  labs(title="Total Monthly Ride Durations by Cyclist Type", subtitle="April 2021 - March 2022",
       x = "Month & Year",
       y = "Overall Ride Duration (in hours)",
       caption="Data Provided by Google Capstone Project")+
  scale_y_continuous(breaks = c(0,40000,80000,120000,160000,200000), labels = scales::comma)+ #this combined scale_y_continuous(limits=c(0,500000)  AND scale_y_continuous(labels=comma)
  theme_fivethirtyeight(base_size = 12, base_family = "A")+
  labs(fill='Rider Type')+
  theme(axis.title = element_text(face="bold"), strip.text = element_blank())























#treemap. not very pretty, but for future reference
install.packages("treemap")
library(treemap)
treemap(dtf = bike_data_v4,
        index=c("start_station_name"),
        n=5,
        vSize="ride_duration",
        vColor="ride_distance",
        palette="Spectral",
        type="value",
        border.col=c("grey70", "grey90"),
        fontsize.title = 18,
        algorithm="pivotSize",
        title ="Treemap of the top 15 NZ's most populous cities",
        title.legend="ride_distance")



#back-to-back bar graph fail
ggplot(bike_data_v4, aes(x = ride_month, y = ride_duration, fill = member_casual)) +
  geom_bar(stat = "identity") + 
  facet_share(~member_casual, dir = "h", scales = "free", reverse_num = TRUE) +
  coord_flip() +
  theme_minimal()