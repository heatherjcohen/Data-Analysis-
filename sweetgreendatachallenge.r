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
orders<-read.csv(file="C:\\Users\\span\\Desktop\\Heather Misc\\Personal\\sweetgreen analytics exercise 20180205\\orders 20180205.csv", header = TRUE, sep = ",")
locations<-read.csv(file="C:\\Users\\span\\Desktop\\Heather Misc\\Personal\\sweetgreen analytics exercise 20180205\\locations 20180111.csv", header = TRUE, sep = ",")

orders$location_id <- as.numeric(gsub(",", "", orders$location_id))
orders$spend_amount <- as.numeric(gsub(",", "", orders$spend_amount))
orders$order_id <- as.numeric(gsub(",", "", orders$order_id))
orders$user_id  <- as.numeric(gsub(",", "", orders$user_id))
orders$earn_amount <- as.numeric(gsub(",", "", orders$earn_amount))
orders$created_at<-as.POSIXct(orders$created_at, ("%Y-%m-%d %H:%M:%S"))

locations$location_id <- as.numeric(gsub(",", "", locations$location_id))

summary(orders)

total <- merge(orders,locations,by="location_id")
total$time<-format(as.POSIXct(total$created_at,format='%m/%d/%Y %H:%M:%S'),format='%H:%M:%S')
total$date <- format(as.POSIXct(total$created_at,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
total$month <- format(as.POSIXct(total$created_at,format='%m/%d/%Y %H:%M:%S'),format='%b')

AmountByName<-aggregate(total$spend_amount, by=list(Category=total$name), FUN=sum)
colnames(AmountByName) <- c("Location Name", "Total Amount Spent")

AmountByCity<-aggregate(total$spend_amount, by=list(Category=total$city), FUN=sum)
colnames(AmountByCity) <- c("City", "Total Amount Spent")

AmountByState<-aggregate(total$spend_amount, by=list(Category=total$state), FUN=sum)
colnames(AmountByState) <- c("State", "Total Amount Spent")

AmountByUser<-aggregate(total$spend_amount, by=list(Category=total$user_id), FUN=sum)
colnames(AmountByUser) <- c("User_Id", "Total Amount Spent")

ByUserByLocation <- aggregate(total$spend_amount, by = list(total$user_id, total$name), FUN=sum)
colnames(ByUserByLocation) <- c("User_Id", "Location Name", "Total Amount Spent")

AmountByDate <- aggregate(total$spend_amount, by = list(total$created_at), FUN=sum)
colnames(AmountByDate) <- c("Created At", "Total Amount Spent")

AmountByJustDate <- aggregate(total$spend_amount, by = list(total$date), FUN=sum)
colnames(AmountByJustDate) <- c("Date", "Total Amount Spent")

ggplot(total) + geom_boxplot(aes(x=month, y=spend_amount))

ggplot(total, aes(x=month, y=spend_amount, color=month)) + 
  geom_point() +  theme(axis.text.x=element_text(angle=90)) + scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
