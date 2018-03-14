library(dplyr)
library(pastecs)
library(doBy)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(scales)
library(car)
library(plotly)
library(chron)
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

################################################  Inspect for trends

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
topAllData = totals[totals$user_id %in% topUser,]
#Greatest number of orders
orderedData2 = NumberOfOrdersByID2[order(NumberOfOrdersByID2[,2], decreasing = TRUE),]
topUser2 =orderedData2$user_id[1:20]
topAllData2 = totals[totals$user_id %in% topUser2,]
#Highest Average Order Amount
orderedData3 = AveAmountByUser[order(AveAmountByUser[,2], decreasing = TRUE),]
topUser3 = orderedData3$User_Id[1:20]
topAllData3 = totals[totals$user_id %in% topUser3,]

totals <- rbind(topAllData, topAllData2)
total2<-rbind(totals, topAllData3)
#####This is the dataframe that holds all the high value user_ids and their order_ids
total3<-total2[!duplicated(total2), ]
################################################################# Characterize High Value ####################

total3$time <- as.POSIXct(total3$time,format="%H:%M:%S",tz="PST")




#Line
ggplot(aes(x=date, y=spend_amount), data=total3) + geom_line() +stat_smooth(colour='blue', span=0.2)

#Dot
ggplot(total3, aes(date, spend_amount)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_smooth(method=lm)

#Even with smaller sample, box plots still are super squished 
ggplot(total3, aes(x=date, y=spend_amount))+ geom_boxplot(aes(group=month))


AmountByName3<-aggregate(total3$spend_amount, by=list(Category=total3$name), FUN=sum)
colnames(AmountByName3) <- c("Location Name", "Total Amount Spent")

ggplot(total3, aes(name, spend_amount, fill=state)) +
  geom_col()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(total3, aes(date, spend_amount, fill=month)) +
  geom_col()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ geom_hline(yintercept = mean(total3$spend_amount), color="blue")

ggplot(total3, aes(date, spend_amount, fill=state)) +
  geom_col()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(total3, aes(time, spend_amount, fill=state)) +
  geom_col()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ facet_grid(state ~ .)

ggplot(data=total3, aes(total3$user_id, fill=state)) + geom_histogram()+ theme(axis.text.x = element_text(angle=45))
ggplot(data=total3, aes(total3$order_id, fill=state)) + geom_histogram()+ theme(axis.text.x = element_text(angle=45))

ggplot(total, aes(name, spend_amount, fill=state)) +
  geom_col()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data=total3, aes(total3$name, fill=state)) + geom_histogram(stat="count")+ theme(axis.text.x = element_text(angle=45))

ggplot(data=topAllData2, aes(topAllData2$name, fill=state)) + geom_histogram(stat="count")+ theme(axis.text.x = element_text(angle=45))
##################################Add in item info
items<-read.csv(file="C:\\Users\\span\\Desktop\\Heather Misc\\Personal\\sweetgreen analytics exercise 20180205\\items 20180205.csv", header = TRUE, sep = ",")
#looks like there's always two identical line items for each order -- that's probably wrong? 

#It's weird that the ids don't match
items$order_id <- as.numeric(gsub(",", "", items$order_id))
items$id  <- as.numeric(gsub(",", "", items$id))

#Ohhey, they do just fine after they're the same type and have no commas *facepalm*
intersect(items$order_id,orders$order_id)
intersect(items$id,orders$order_id)

highrollersbought <- merge(total3,items,by="order_id")

####### Characterize items
itemdf<-count(items, 'item_name')
itemdf<-itemdf[order(itemdf[,2], decreasing = TRUE),]
itemdf2<-itemdf[1:20,]

ggplot(data=itemdf2, aes(x=item_name, y=freq, fill=item_name)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=45))

itemdf3<-count(highrollersbought, 'item_name')
itemdf3<-itemdf3[order(itemdf3[,2], decreasing = TRUE),]
itemdf4<-itemdf3[1:20,]

ggplot(data=itemdf4, aes(x=item_name, y=freq, fill=item_name)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=45))


ggplot(data=highrollersbought, aes(x=name, y=spend_amount, fill=item_name)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=45))

ByUserByLocation2 <- aggregate(highrollersbought$spend_amount, by = list(highrollersbought$user_id, highrollersbought$item_name), FUN=sum)
ByUserByLocation3 <- aggregate(highrollersbought$spend_amount, by = list(highrollersbought$name, highrollersbought$item_name), FUN=sum)


