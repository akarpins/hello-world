#Bikeshare R project - last updated on August 10, 2019 - AK


#read the working files*/
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

names(ny)

#add summary statistics - AK on Aug 10, 2019

summary(ny)

#format variables
ny$Birth.Year <- as.integer(ny$Birth.Year)
ny$User.Type   <- as.factor(ny$User.Type)
# Gender (Zero=unknown; 1=male; 2=female)
ny$Gender     <- as.factor(ny$Gender)
levels(ny$Gender) <- c("UNKNOWN", "MALE", "FEMALE")

#plotting the histogram of the birth year, the count and proportion of user types, and the proportion of gender
#(which we also defined as factor with three levels: unknown, make, and female).
library(ggplot2)
ggplot(ny) + geom_histogram(aes(Birth.Year)) +  ggtitle("Graph 1a) Histogtam of the Birth Year in NY")

#Get the summary statistics for NY data
summary(ny)

#frequency table for Birth Year
library(dplyr)
library(tidyr)
library(knitr)
ny%>%
  group_by(Birth.Year)%>%
  summarize(n=n())%>%
  mutate(prop=n/sum(n))%>%   # our new proportion variable
  kable() 


#Plot the proportion of Gender distribution in NY data
ggplot(ny) + geom_bar(aes(Gender, y = (..count..)/sum(..count..)), fill="pink") + ylab("Proportion") + theme_bw() +  ggtitle("Graph 1b) Distribution of Gender in NY")

#Summary Statistic for Gender in NY
ny%>%
  group_by(Gender)%>%
  summarize(n=n())%>%
  mutate(prop=n/sum(n))%>%   # our new proportion variable
  kable() 

require(lubridate)
library(dplyr)

head(ny)
require(lubridate)

# create the weekday and hour variable with the day and hour of trip start time
#data <- mutate(data, weekday = wday(starttime, label=TRUE), hour=as.factor(hour(starttime))) 
ny$weekday <- wday(ny$Start.Time, label=TRUE)
ny$hour    <- as.factor(hour(ny$Start.Time))


# plot the percentage of trips in each day 
#(note y=(..count..)/sum(..count..) is used to plot the fraction of the total in each day, otherwise the total count of trips in each day would be produced)
library(ggplot2)
ggplot(ny) + geom_bar(aes(x=weekday, y=(..count..)/sum(..count..)), fill="darkorange") + theme_bw() + ylab("Frequency of trips") + ggtitle("Graph 2a) Percentage of Trips in NY per week")


#Summary Statistic for Trips in NY
ny%>%
  group_by(hour)%>%
  summarize(n=n())%>%
  mutate(prop=n/sum(n))%>%   # our new proportion variable
  kable() 

# Your solution code goes here

library(dplyr)

chi$birth.year <- as.integer(chi$Birth.Year)
chi$usertype   <- as.factor(chi$User.Type)
# Gender (Zero=unknown; 1=male; 2=female)
chi$Gender     <- as.factor(chi$Gender)
levels(chi$Gender) <- c("UNKNOWN", "MALE", "FEMALE")
chi %>% filter(Gender != "UNKNOWN") %>% ggplot(.)  + geom_density(aes(Birth.Year,group=Gender, fill=Gender, colour=Gender), adjust=3, alpha=0.1) + ggtitle("Graph 3a) The density of Gender distribution in Chicago")


#Summary Statistic for Trips in NY
chi%>%
  group_by(Gender, Birth.Year)%>%
  summarize(n=n())%>%
  mutate(prop=n/sum(n))%>%   # our new proportion variable
  kable() 

system('python -m nbconvert Explore_bikeshare_data.ipynb')
