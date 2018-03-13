library(dplyr)
library(pastecs)
library(doBy)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(scales)
library(car)
library(plotly)
library(plyr)
library(scales)
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

total$date <- as.Date(total$date, "%m/%d/%Y")

NumberOfOrdersByID<-tally(group_by(total, user_id))
colnames(NumberOfOrdersByID) <- c("User_id", "# of orders")

NumberOfOrdersByID2<-aggregate(order_id ~ user_id, total, function(x) length(unique(x)))

AmountByName<-aggregate(total$spend_amount, by=list(Category=total$name), FUN=sum)
colnames(AmountByName) <- c("Location Name", "Total Amount Spent")

AmountByCity<-aggregate(total$spend_amount, by=list(Category=total$city), FUN=sum)
colnames(AmountByCity) <- c("City", "Total Amount Spent")

AmountByState<-aggregate(total$spend_amount, by=list(Category=total$state), FUN=sum)
colnames(AmountByState) <- c("State", "Total Amount Spent")

AmountByUser<-aggregate(total$spend_amount, by=list(Category=total$user_id), FUN=sum)
colnames(AmountByUser) <- c("User_Id", "Total Amount Spent")

AveAmountByUser<-aggregate(total$spend_amount, by=list(Category=total$user_id), FUN=mean)
colnames(AveAmountByUser) <- c("User_Id", "Average Amount Spent")

ByUserByLocation <- aggregate(total$spend_amount, by = list(total$user_id, total$name), FUN=sum)
colnames(ByUserByLocation) <- c("User_Id", "Location Name", "Total Amount Spent")

AmountByDate <- aggregate(total$spend_amount, by = list(total$created_at), FUN=sum)
colnames(AmountByDate) <- c("Created At", "Total Amount Spent")

AmountByJustDate <- aggregate(total$spend_amount, by = list(total$date), FUN=sum)
colnames(AmountByJustDate) <- c("Date", "Total Amount Spent")

AmountByMonth <- aggregate(total$spend_amount, by = list(total$month), FUN=sum)
colnames(AmountByMonth) <- c("Month", "Total Amount Spent")

AveAmountByMonth <- aggregate(total$spend_amount, by = list(total$month), FUN=mean)
colnames(AveAmountByMonth) <- c("Month", "Average Amount Spent")

#############################################Subsegment high value 
#Highest spending per user
orderedData = AmountByUser[order(AmountByUser[,2], decreasing = TRUE),]
topUser = orderedData$User_Id[1:20]
topAllData = total[total$user_id %in% topUser,]
#Greatest number of orders
orderedData2 = NumberOfOrdersByID2[order(NumberOfOrdersByID2[,2], decreasing = TRUE),]
topUser2 =orderedData2$user_id[1:20]
topAllData2 = total[total$user_id %in% topUser2,]
#Highest Average Order Amount
orderedData3 = AveAmountByUser[order(AveAmountByUser[,2], decreasing = TRUE),]
topUser3 = orderedData3$User_Id[1:20]
topAllData3 = total[total$user_id %in% topUser3,]

total <- rbind(topAllData, topAllData2)
total2<-rbind(total, topAllData3)
#####This is the dataframe that holds all the high value user_ids and their order_ids
total3<-total2[!duplicated(total2), ]
#################################################################Plots####################



ggplot(aes(x=date, y=spend_amount), data=sample) + geom_line() +stat_smooth(colour='blue', span=0.2)

ggplot(data = sample, aes(x = date, y = spend_amount))+
  geom_line(color = "#00AFBB")




ggplot(sample, aes(date, spend_amount)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_smooth(method=lm)


ggplot(total, aes(date, spend_amount)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_smooth(method=lm)


ggplot(sample, aes(x=date, y=spend_amount))+ geom_boxplot(aes(group=month))