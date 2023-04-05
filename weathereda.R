library(tidyverse)  # data wrangling
weatherdata <- read.csv("Weather Data.csv")


#1)  Find all the unique 'Wind Speed' values in the data.
colnames(weatherdata)
unique(weatherdata$Wind.Speed_km.h)


# 2) Find the number of times when the 'Weather is exactly Clear'.
length(weatherdata[str_detect(weatherdata$Weather,"Clear"),])
unique(weatherdata$Weather)


# 3) Find the number of times when the 'Wind Speed was exactly 4 km/h'.
length(weatherdata[weatherdata$Wind.Speed_km.h==4,])


# 4) Find out all the Null Values in the data.
weatherdata %>% 
  filter(if_any(everything(), is.na))

# 5) Rename the column name 'Weather' of the dataframe to 'Weather Condition'.
colnames(weatherdata)[8]="WeatherCondition"
colnames(weatherdata)


# 6) What is the mean 'Visibility' ?
mean(weatherdata$Visibility_km)

# 7) What is the Standard Deviation of 'Pressure'  in this data?
sd(weatherdata$Press_kPa,na.rm=TRUE)

#8) What is the Variance of 'Relative Humidity' in this data ?
var(weatherdata$Rel.Hum_., na.rm = TRUE)

# 9) Find all instances when 'Snow' was recorded.
weatherdata[str_detect(weatherdata$Weather,"Snow"),]

#10) Find all instances when 'Wind Speed is above 24' and 'Visibility is 25'.
weatherdata[weatherdata$Wind.Speed_km.h>24 & weatherdata$Visibility_km==25,]

# 11) What is the Mean value of each column against each 'Weather Condition ?
# Group by mean of multiple columns
df2 <- weatherdata %>% group_by(WeatherCondition) %>% 
  summarise(mean_speed=mean(Wind.Speed_km.h),
            mean_visibility= mean(Visibility_km),
            mean_pressure= mean(Press_kPa),
            mean_temp= mean(Temp_C),
            mean_dew= mean(Dew.Point.Temp_C),
            mean_hum= mean(Rel.Hum_.),
            .groups = 'drop') %>%
  as.data.frame()
df2

# 12) What is the Minimum & Maximum value of each column against each 'Weather Condition ?
df3 <- weatherdata %>% group_by(WeatherCondition) %>% 
  summarise(max_speed=max(Wind.Speed_km.h),
            max_visibility= max(Visibility_km),
            max_pressure= max(Press_kPa),
            max_temp= max(Temp_C),
            max_dew= max(Dew.Point.Temp_C),
            max_hum= max(Rel.Hum_.),
            .groups = 'drop') %>%
  as.data.frame()
df3
df4 <- weatherdata %>% group_by(WeatherCondition) %>% 
  summarise(min_speed=min(Wind.Speed_km.h),
            min_visibility= min(Visibility_km),
            min_pressure= min(Press_kPa),
            min_temp= min(Temp_C),
            min_dew= min(Dew.Point.Temp_C),
            min_hum= min(Rel.Hum_.),
            .groups = 'drop') %>%
  as.data.frame()
df4
#13) Show all the Records where Weather Condition is Fog.
weatherdata[weatherdata$WeatherCondition=="Fog",]
# 14) Find all instances when 'Weather is Clear' or 'Visibility is above 40'.
weatherdata[weatherdata$WeatherCondition=="Clear" & weatherdata$Visibility_km>40,]

#15) Find all instances when :
# 'Weather is Clear' and 'Relative Humidity is greater than 50' or
# 'Visibility is above 40'
weatherdata[(weatherdata$WeatherCondition=="Clear" & weatherdata$Rel.Hum_.>50) | weatherdata$Visibility_km>40,]
