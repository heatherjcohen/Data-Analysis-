library(dplyr)
library(pastecs)
library(doBy)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(scales)
library(car)
library(plotly)
recode <- car::recode
##############################################################################
# Read
##############################################################################
#Windows
#Heather updated to change to newest file and fixed syntax
data<-read.csv(file="C:\\Users\\span\\Desktop\\Heather Misc\\Personal\\DadWeight.csv", header = TRUE, sep = ",")
#
####Remove NAs

dad <- data[c(1:3336),c(1:3,7:10)]
dad<-dad[complete.cases(dad), ]

###### Correct Data Types
dad$Estimated.Lean.Body.Weight<-abs(as.numeric(as.character(dad$Estimated.Lean.Body.Weight)))
dad$Estimated.Body.Fat.Weight<-abs(as.numeric(as.character(dad$Estimated.Body.Fat.Weight)))
dad$Estimated.Body.Fat.Percentage<-abs(as.numeric(as.character(dad$Estimated.Body.Fat.Percentage)))

##### Format Date

colnames(data)[1] <- "Weekday"
dad$WeekdayStr<-ifelse(dad$Weekday==1,"Monday",
                    ifelse(dad$Weekday==2,"Tuesday",   
                           ifelse(dad$Weekday==3,"Wednesday",
                                  ifelse(dad$Weekday==4,"Thursday",
                                         ifelse(dad$Weekday==5,"Friday",
                                                ifelse(dad$Weekday==6,"Saturday",
                                                       ifelse(dad$Weekday==7,"Sunday",99)))))))

dad$WeekdayStr <- factor(dad$WeekdayStr, levels= c( "Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
dad$Date<-as.character(dad$Date)
dad$Date<-as.Date(dad$Date, "%A, %B %d,%Y")
monthOrder <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
dad$Month <- factor(format(dad$Date, "%b"), levels = monthOrder)

#################################################   PLOTS ##########################################
ggplot(dad, aes(x=Date, y=Weight..pounds.)) + 
  geom_point() + 
  theme(axis.text.x=element_text(angle=90))  + geom_smooth(method='lm')

ggplot(dad, aes(x=Date, y=Estimated.Body.Fat.Percentage)) + 
  geom_point() + 
  theme(axis.text.x=element_text(angle=90))  

ggplot(dad, aes(x=Weekday, y=Weight..pounds., color=Date)) + 
  geom_point() +  theme(axis.text.x=element_text(angle=90)) + scale_x_discrete(limits=c("Monday","Tuesday", "Wednesday", 'Thursday', "Friday", "Saturday", "Sunday"))

ggplot(data = dad,
       mapping = aes(x = Date, y = Weight..pounds.,  colour = WeekdayStr)) +
  geom_point() +
  geom_line() +
  facet_grid(facets = WeekdayStr ~ .) + guides(fill=FALSE)


dad1<- dad %>%
  select(Weight..pounds., Date, Weekday, WeekdayStr) %>%
  filter(Date >= as.Date("2017-01-01"))


ggplot(data = dad1,
       mapping = aes(x = Date, y = Weight..pounds.,  colour = WeekdayStr)) +
  geom_point() +
  geom_line() +
  facet_grid(facets = WeekdayStr ~ .) + guides(fill=FALSE)


ggplot(dad, aes(Month, Weight..pounds.,fill = Month)) + geom_boxplot() + stat_boxplot(geom ='errorbar') + ggtitle("Weight Funcuations by Month")


ggplot(dad, aes(WeekdayStr, Weight..pounds.,fill = WeekdayStr)) + geom_boxplot() + stat_boxplot(geom ='errorbar') + ggtitle("Weight Funcuations by Day of the Week")


ggplot(dad, aes(Date, Weight..pounds.)) + geom_line(linetype="longdash") + geom_point() + ggtitle("Weight Over Time")




#https://www.clarusft.com/exploring-seasonality-in-a-time-series-with-rs-ggplot2/












