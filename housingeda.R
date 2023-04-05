

library(tidyverse)  # data wrangling
datayr <- read.csv("housing_in_london_yearly_variables.csv")
datamonth <- read.csv("housing_in_london_monthly_variables.csv")

#Housing data:
# 1) Convert the Datatype of 'Date' column to Date-Time format.

datayr$date<- as.Date(datayr$date)
class(datayr$date)

datamonth$date<- as.Date(datamonth$date)
class(datamonth$date)

datamonth$month <- format(datamonth$date, "%m")
datamonth$year <- format(datamonth$date, "%Y")
head(datamonth$month)
head(datamonth$year)


# 2) Add a new column ''year'' in the dataframe, which contains years only.(B.2) Add a new column ''month'' as 2nd column in the dataframe, which contains month only.
datayr$month <- format(datayr$date, "%m")
datayr$year <- format(datayr$date, "%Y")
head(datayr$month)
head(datayr$year)
datayr$month <- month.name[as.numeric(datayr$month)]
head(datayr$month)


# 3) Remove the null columns '
#datayr<- datayr[ , colSums(is.na(datayr))==0]
#datamonth<- datamonth[ , colSums(is.na(datamonth))==0]
head(datayr)
head(datamonth)
colnames(datayr)
colnames(datamonth)

#4) Show all the records where 'No. of Crimes' is 0. And, how many such records are there ?
datamonth[datamonth$no_of_crimes==0,]
length(datamonth[datamonth$no_of_crimes==0,])

#5) What is the maximum & minimum 'average_price' per year in england ?
# aggregate
aggregate(datamonth$average_price, by = list(datamonth$year), max)
aggregate(average_price ~ year, data = datamonth, min)

# 6) What is the Maximum & Minimum No. of Crimes recorded per area ?
aggregate(datamonth$no_of_crimes, by = list(datamonth$area), max)
aggregate(no_of_crimes ~ area, data = datamonth, min)
head(datamonth$no_of_crimes)

# 7) Show the total count of records of each area, where average price is less than 100000.
length(unique(datamonth$area[datamonth$average_price<100000]))

# 8)year and month least crime
min(datamonth$no_of_crimes)
datamonth$year[(datamonth$no_of_crimes== min(datamonth$no_of_crimes)) && datamonth$no_of_crimes != NA]
unique(datamonth$month[datamonth$no_of_crimes== min(datamonth$no_of_crimes)])
unique(datamonth$month[datamonth$no_of_crimes==min(datamonth[,6], na.rm=T)])

library(dplyr)
crimebyyear<-datamonth %>%
  group_by(year) %>%
  summarise(
    total = sum(
      no_of_crimes, na.rm = TRUE),
    .groups = "drop")
crimebyyear$year[crimebyyear$total == min(crimebyyear$total)]

crimebymonth <-datamonth %>%
 group_by(month) %>%
 summarise(
    total = sum(
      no_of_crimes, na.rm = TRUE),
    .groups = "drop")
crimebymonth$month[crimebymonth$total == min(crimebymonth$total)]
   

