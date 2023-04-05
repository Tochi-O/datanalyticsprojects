library(tidyverse)  # data wrangling
udemydata <- read.csv("udemy_courses.csv")

colnames(udemydata)
# 1) What are all different subjects for which Udemy is offering courses ?
unique(udemydata$subject)

# 2) Which subject has the maximum number of courses.
names(which.max(table(udemydata$subject)))


# 3) Show all the courses which are Free of Cost.
unique(udemydata$price)
udemydata$course_title[udemydata$price==0]


#4) Show all the courses which are Paid.
udemydata$course_title[udemydata$price>0]




#5) Which are Top Selling Courses ?
# sorting the data by the column
# required in descending order
data_sorted <- udemydata[order(udemydata$num_subscribers,
                                decreasing = TRUE), ]
head(data_sorted$course_title,10)
head(data_sorted$num_subscribers,10)


# 6) Which are Least Selling Courses ?
tail(data_sorted$course_title,10)
tail(data_sorted$num_subscribers,10)

#7) Show all courses of Graphic Design where the price is below 100 ?
udemydata$course_title[udemydata$subject=='Graphic Design' & udemydata$price<100]
class(udemydata$price)


#8) List out all the courses that are related to 'Python'.
library("stringr")   
udemydata$course_title[str_detect(udemydata$course_title, "Python")]

#9) What are courses that were published in the year 2015 ?
class(udemydata$published_timestamp)
head(udemydata$published_timestamp,10)
udemydata$published_timestamp<- as.Date(udemydata$published_timestamp)
udemydata$year <- format(udemydata$published_timestamp, "%Y")
class(udemydata$year)
class(udemydata$published_timestamp)
udemydata$course_title[udemydata$year=="2015"]
head(udemydata$year)

# 10) What is the Max. Number of Subscribers for Each Level of courses ?
library(dplyr)
aggregate(udemydata$num_subscribers, by = list(udemydata$level), max)
aggregate(num_subscribers ~ level, data = udemydata, max)
  