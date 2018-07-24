##############################################################################
# Libs
##############################################################################

library(dplyr)
library(ggplot2)
library(tidyr)


##############################################################################
# Read
##############################################################################

Members = read.csv("//Users//heathercohen//Desktop//SNChurn.csv")
Revenue = read.csv("//Users//heathercohen//Documents//SNRevenueChurn.csv")
Averages = read.csv("//Users//heathercohen//Desktop//SNAverages.csv")

IndustriesLeaving = read.csv("//Users//heathercohen//Desktop//SNIndustLeave.csv")
IndustriesPerMonth= read.csv("//Users//heathercohen//Desktop//SNIndustBeg.csv")

Cities = read.csv("//Users//heathercohen//Desktop//SNCities.csv")



p <-ggplot(Averages, aes(x=Industry, y=mean)) + geom_point()
p + theme(axis.text.x = element_text(angle=45)) + ggtitle("Average # of Months Active\nBy Industry")+ labs(y="Average Active Months", x = "Industries")


Members$First.Of.Month<-as.Date(Members$First.Of.Month, format="%Y-%m-%d")
Revenue$First.Of.Month<-as.Date(Revenue$First.Of.Month, format="%Y-%m-%d")
IndustriesLeaving$Month<-as.Date(Revenue$First.Of.Month, format="%Y-%m-%d")
IndustriesPerMonth$Month<-as.Date(Revenue$First.Of.Month, format="%Y-%m-%d")
Cities$Month<-as.Date(Revenue$First.Of.Month, format="%Y-%m-%d")

n<-ggplot(Members, aes(First.Of.Month)) +
  geom_line(aes(y =X..Cust.At.Beginning.Of.Month , colour = "X..Cust.At.Beginning.Of.Month")) + 
  geom_line(aes(y = X..Cust.Who.Left, colour = "X..Cust.Who.Left")) 
n + ggtitle("Customer Churn")+ labs(y="Customers", x = "Time") +
  scale_color_manual(labels = c("Active Customers", "Customers Who Cancelled"), values = c("cyan", "coral")) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") +
  theme(axis.text.x=element_text(angle=60, hjust=1))


o<-ggplot(Members, aes(x=First.Of.Month, y=Customer.Churn.Rate..)) + geom_point()+ 
  geom_smooth(method='lm')
o + scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+ 
  ggtitle("Customer Churn %")+ labs(y="Churn %", x = "Time")

require(scales)
require(reshape2)

q<-ggplot(Revenue, aes(x=First.Of.Month, y=Revenue.Churn.Amount...)) + geom_point()+ 
  geom_smooth(method='lm')
q + scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+ 
  ggtitle("Revenue Churn $")+ labs(y="Churn $", x = "Time")+ scale_y_continuous(labels = comma)


r<-ggplot(Revenue, aes(x=First.Of.Month, y=Revenue.Churn.Percent...)) + geom_point()+ 
  geom_smooth(colour="darkgreen", method='lm')
r + scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+ 
  ggtitle("Revenue Churn %")+ labs(y="Churn %", x = "Time")+ scale_y_continuous(labels = comma)


df2 <- IndustriesLeaving %>% gather("variable","value", -Month)
ggplot(df2, aes(x=Month, y=value, color=variable)) + geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+ 
  ggtitle("Industries Who Lose The Most People Per Month\n by mean")+ labs(y="# Cancelling", x = "Time")


df3 <- IndustriesPerMonth %>% gather("variable","value", -Month)
ggplot(df3, aes(x=Month, y=value, color=variable)) + geom_line()+ 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+ 
  ggtitle("Most Popular Industries")+ labs(y="# Active On First Of Month", x = "Time")


df4 <- Cities %>% gather("variable","value", -Month)
ggplot(df4, aes(x=Month, y=value, color=variable)) + geom_line()+ 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+ 
  ggtitle("Most Popular Cities")+ labs(y="# Active On First Of Month", x = "Time")

