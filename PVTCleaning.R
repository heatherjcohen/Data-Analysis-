########################################
#      to do 
#---------------------
#     identify new outliers to exclude
#     overlapping hist of correct/wrong/missed pre and one post
#     overlapping % correct
#     overlapping count correct
#     how much pre score correlates to post
#





library(dplyr)
library(pastecs)
library(doBy)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(car)
recode <- car::recode
##############################################################################
# Read
##############################################################################
#Windows
#Heather updated to change to newest file and fixed syntax
data<-read.csv(file="C:\\Users\\span\\Desktop\\Heather Misc\\ForDevelopingScriptsToExtractBehavioralData\\PVT Only Behavioral Data Summary FINAL.csv", header = TRUE, sep = ",")

#Add role column

data$Role = data$Ids %% 10 
data$Role<-ifelse(data$Role==1,"Strokee",
                    ifelse(data$Role==2,"Stroker",99))

############################################################################
#Compare right/wrong before after
data$PVT_Pre_Percent_Correct<-data$PVT_Pre_Correct/data$PVT_Pre_Trial_Count
data$PVT_Pre_Percent_Correct<-data$PVT_Pre_Percent_Correct * 100

data$PVT_Post_Percent_Correct<-data$PVT_Post_Correct/data$PVT_Post_Trial_Count
data$PVT_Post_Percent_Correct<-data$PVT_Post_Percent_Correct * 100

data$Delta = data$PVT_Pre_Percent_Correct-data$PVT_Post_Percent_Correct

############################################ T Tests #############################3#
t.test(data$PVT_Pre_Correct,data$PVT_Post_Correct,paired=TRUE)

t.test(data$PVT_Pre_Percent_Correct,data$PVT_Post_Percent_Correct,paired=TRUE)

####################################Find Outliers##################
hist(data$Delta)
hist(data$PVT_Post_Percent_Correct)
hist(data$PVT_Pre_Percent_Correct)
hist(data$PVT_Post_Correct)
hist(data$PVT_Pre_Correct)

###########################################################################3
No_Outliers<-data
#remove higher than 100% correct
#toohigh<-data[data[,"PVT_Post_Percent_Correct"] >100,]
#toohigh2<-data[data[,"PVT_Pre_Percent_Correct"] >100,]
#toohigh3<-data[data[,"PVT_Pre_Correct"] >60,]
badRows<-c(11,56,102,109,110,113,126,185,187,49,78,105,154,174,176,203,206,225,149,160,220) # Hard coded after visual inspection ---- Heather Updated ---,
No_Outliers<-No_Outliers[-badRows, ]



###############################################

jk<- ggplot(No_Outliers, aes(x=Role, y=Delta, fill=Role)) + geom_boxplot()+ labs(x = "Change in PVT Scores by Role", y="PVT Score Change") +scale_fill_brewer(palette="Dark2")
jk +   labs(caption = "n=223") 



h<-subset(No_Outliers, select=c("PVT_Pre_Percent_Correct", "PVT_Post_Percent_Correct", "Ids"))


library(reshape)
hh <- melt(h, id=c("Ids"))


hh$variable<-ifelse(hh$variable=="PVT_Pre_Percent_Correct","Pre",
                  ifelse(hh$variable=="PVT_Post_Percent_Correct", "Post",99))

names(hh)[2]<-"PrePost"
names(hh)[3]<-"PVT_Correct_Percent"

kk<- ggplot(hh, aes(x=PrePost, y=PVT_Correct_Percent, fill=PrePost)) + geom_boxplot()+ labs(x = "Before and After OMing", y="Percentage of Correct PVT Responses")+ scale_x_discrete(limits=c("Pre","Post"))       


kk+ labs(caption = "n=223") 

n<-nrow(No_Outliers)
n
##################################################Basic Hists of stats####################
s1<-ggplot(No_Outliers)+ geom_histogram(aes(x=PVT_Pre_Percent_Correct), 
                                        fill="brown1", color="black") + labs(x ="Pre Percent Correct")

s2<-ggplot(No_Outliers)+ geom_histogram(aes(x=PVT_Post_Percent_Correct), 
                                        fill="cyan4", color="black") + labs(x ="Post Percent Correct")

s3<-ggplot(No_Outliers)+ geom_histogram(aes(x=PVT_Pre_Correct), 
                                        fill="aquamarine1", color="black") + labs(x ="Pre Correct Count")

s4<-ggplot(No_Outliers)+ geom_histogram(aes(x=PVT_Post_Correct), 
                                        fill="firebrick2", color="black") + labs(x ="Post Correct Count")

s5<-ggplot(No_Outliers)+ geom_histogram(aes(x=Delta), 
                                        fill="deeppink3", color="black") + labs(x ="Change in Correct Pre/Post")

s6<-ggplot(No_Outliers)+ geom_histogram(aes(x=PVT_Pre_Missed), 
                                        fill="greenyellow", color="black") + labs(x ="Pre Missed Count")

s7<-ggplot(No_Outliers)+ geom_histogram(aes(x=PVT_Post_Missed), 
                                        fill="lightblue2", color="black")  + labs(x ="Post Missed Count")

s8<-ggplot(No_Outliers)+ geom_histogram(aes(x=PVT_Pre_Wrong), 
                                        fill="mediumpurple3", color="black")  + labs(x ="Pre Wrong Count")

s9<-ggplot(No_Outliers)+ geom_histogram(aes(x=PVT_Post_Wrong), 
                                        fill="mediumvioletred", color="black")  + labs(x ="Post Wrong Count")

grid.arrange(s1,s2,s3,s4,s5,s6,s7,s8,s9, top = "PVT Basic Stats n=223")

#hist(No_Outliers$PVT_Pre_Percent_Correct, space=0,  xlim=c(0, 100),  sub="n=227", col=c("darkcyan"), xlab = "Percent Correct", ylab = "Number of Participants Per Score",  main = "PVT Scores Before Oming")
#hist(No_Outliers$PVT_Post_Percent_Correct, space=0,  xlim=c(0, 100),  sub="n=227", col=c("darkseagreen1"), xlab = "Percent Correct", ylab = "Number of Participants Per Score",  main = "PVT Scores After Oming")

##############################################################
#              Overlapping Histograms 
##Pre stats

#ggplot(No_Outliers)+ geom_histogram(aes(x=PVT_Pre_Correct), fill="aquamarine1", color="black", alpha=0.5) + geom_histogram(aes(x=PVT_Pre_Missed), fill="greenyellow", color="black",alpha=0.5) +  geom_histogram(aes(x=PVT_Pre_Wrong),fill="mediumpurple3", color="black",alpha=0.5) 


#No_Outliers %>%
#  gather(group, value,  c("PVT_Pre_Correct", "PVT_Pre_Missed", "PVT_Pre_Wrong")) %>%
#  ggplot(aes(x = value, fill = group)) +
#  geom_histogram(alpha = 0.5, color = "black", position = "identity") +
#  scale_fill_manual(values = c("aquamarine1", "greenyellow", "mediumpurple3"),labels = c("Correct", "Missed","Wrong")) + guides(fill=guide_legend(title="Response Type")) + ggtitle("PVT Pre Responses")  +
#  labs(caption = "n=223") 


#No_Outliers %>%
#  gather(group, value,  c("PVT_Post_Correct", "PVT_Post_Missed", "PVT_Post_Wrong")) %>%
#  ggplot(aes(x = value, fill = group)) +
#  geom_histogram(alpha = 0.5, color = "black", position = "identity") +
#  scale_fill_manual(values = c("aquamarine1", "greenyellow", "mediumpurple3"),labels = c("Correct", "Missed","Wrong")) + guides(fill=guide_legend(title="Response Type")) + ggtitle("PVT Post Responses") +  labs(caption = "n=223") 

###Percent correct pre post 
#ggplot(data)+
 # geom_histogram(aes(x=PVT_Pre_Percent_Correct), 
  #               fill="darkcyan", color="black",alpha=0.5) +
 # geom_histogram(aes(x=PVT_Post_Percent_Correct), 
  #               fill="darkorchid4",color="black", alpha=0.5) +labs(x = "Percent of Correct Responses/All Responses Pre and Post OM", y="Percent Correct")

No_Outliers %>%
  gather(group, value,  c("PVT_Pre_Percent_Correct", "PVT_Post_Percent_Correct")) %>%
  ggplot(aes(x = value, fill = group)) +
  geom_histogram(alpha = 0.5, color = "black", position = "identity") +
  scale_fill_manual(values = c("darkcyan", "darkorchid4"),labels = c("Pre", "Post")) + guides(fill=guide_legend(title="% Correct Responses")) + ggtitle("Percent of Correct Responses Pre and Post")   +labs(caption = "n=223") 


#Needs x-axis label.
#Decrease the opacity. As-is, the overlap looks like a 3rd solid color rather than a point of transparency.
#Include "PVT" somewhere in the title
#Consider moving the legend inside the giant black space in the plot. More efficient.




########################################################################################################Plain


No_Outliers %>%
  gather(group, value,  c("PVT_Pre_Correct", "PVT_Post_Correct")) %>%
  ggplot(aes(x = value, fill = group)) +
  geom_histogram(alpha = 0.3, color = "black", position = "identity")+
  scale_fill_manual(values = c("lightseagreen", "lightsalmon1"),labels = c("Pre", "Post")) + guides(fill=guide_legend(title="Correct Responses")) + ggtitle("Correct PVT Responses Pre and Post") +  labs(caption = "n=223", x = "Correct Responses")  + theme(legend.position = c(0.2, 0.7))



No_Outliers %>%
  gather(group, value,  c("PVT_Pre_Missed", "PVT_Post_Missed")) %>%
  ggplot(aes(x = value, fill = group)) +
  geom_histogram(alpha = 0.3, color = "black", position = "identity") +
  scale_fill_manual(values = c("violetred2", "turquoise3"),labels = c("Pre", "Post")) + guides(fill=guide_legend(title="Missed Responses")) + ggtitle("Missed PVT Responses Pre and Post") +  labs(caption = "n=223", x = "Missed Responses")  + theme(legend.position = c(0.7, 0.7))

No_Outliers %>%
  gather(group, value,  c("PVT_Pre_Wrong", "PVT_Post_Wrong")) %>%
  ggplot(aes(x = value, fill = group)) +
  geom_histogram(alpha = 0.4, color = "black", position = "identity") +
  scale_fill_manual(values = c("green", "mediumvioletred"),labels = c("Pre", "Post")) + guides(fill=guide_legend(title="Wrong Responses")) + ggtitle("Wrong PVT Responses Pre and Post") +  labs(caption = "n=223", x = "Wrong Responses")  + theme(legend.position = c(0.7, 0.7))


####################################################### best fit line added

No_Outliers %>%
  gather(group, value,  c("PVT_Pre_Correct", "PVT_Post_Correct")) %>%
  ggplot(aes(x = value, fill = group)) +
  geom_histogram(alpha = 0.3, color = "black", position = "identity", aes(y = ..density..)) + 
  scale_fill_manual(values = c("lightseagreen", "lightsalmon1"),labels = c("Pre", "Post")) +
  guides(fill=guide_legend(title="Correct Responses")) + 
  ggtitle("Correct PVT Responses Pre and Post") + 
  geom_density(aes(linetype=group, color=group, fill=NA), size=1.4, show.legend = F)+  
  labs(caption = "n=223", x = "Correct Responses")  + 
  theme(legend.position = c(0.2, 0.7)) 



No_Outliers %>%
  gather(group, value,  c("PVT_Pre_Missed", "PVT_Post_Missed")) %>%
  ggplot(aes(x = value, fill = group)) +
  geom_histogram(alpha = 0.4, color = "black", position = "identity", aes(y = ..density..)) +
  guides(fill=guide_legend(title="Missed Responses")) + 
  ggtitle("Missed PVT Responses Pre and Post") +  
  geom_density(aes(linetype=group, color=group, fill=NA), size=1.4, show.legend = F)+
  labs(caption = "n=223", x = "Missed Responses")  + 
  scale_fill_manual(values = c("orange1", "midnightblue"),labels = c("Pre", "Post")) + 
  theme(legend.position = c(0.7, 0.7))+ scale_colour_manual( values = c("orange1", "midnightblue"))

No_Outliers %>%
  gather(group, value,  c("PVT_Pre_Wrong", "PVT_Post_Wrong")) %>%
  ggplot(aes(x = value, fill = group)) +
  geom_histogram(alpha = 0.4, color = "black", position = "identity", aes(y = ..density..)) +
  scale_fill_manual(values = c("green", "mediumvioletred"),labels = c("Pre", "Post")) + 
  geom_density(aes(linetype=group, color=group, fill=NA), size=1.4,  show.legend = F)+
  guides(fill=guide_legend(title="Wrong Responses")) + 
  ggtitle("Wrong PVT Responses Pre and Post") +  
  labs(caption = "n=223", x = "Wrong Responses")  + 
  theme(legend.position = c(0.7, 0.7))+ scale_colour_manual( values = c("green", "mediumvioletred"))


################################################################3 with outliers
data %>%
  gather(group, value,  c("PVT_Pre_Correct", "PVT_Post_Correct")) %>%
  ggplot(aes(x = value, fill = group)) +
  geom_histogram(alpha = 0.3, color = "black", position = "identity", aes(y = ..density..)) + 
  scale_fill_manual(values = c("lightseagreen", "lightsalmon1"),labels = c("Pre", "Post")) +
  guides(fill=guide_legend(title="Correct Responses")) + 
  ggtitle("Correct PVT Responses Pre and Post") + 
  geom_density(aes(linetype=group, color=group, fill=NA), size=1.4, show.legend = F)+  
  labs(caption = "n=223", x = "Correct Responses")  + 
  theme(legend.position = c(0.7, 0.7)) 



data %>%
  gather(group, value,  c("PVT_Pre_Missed", "PVT_Post_Missed")) %>%
  ggplot(aes(x = value, fill = group)) +
  geom_histogram(alpha = 0.4, color = "black", position = "identity", aes(y = ..density..)) +
  guides(fill=guide_legend(title="Missed Responses")) + 
  ggtitle("Missed PVT Responses Pre and Post") +  
  geom_density(aes(linetype=group, color=group, fill=NA), size=1.4, show.legend = F)+
  labs(caption = "n=223", x = "Missed Responses")  + 
  scale_fill_manual(values = c("orange1", "midnightblue"),labels = c("Pre", "Post")) + 
  theme(legend.position = c(0.7, 0.7))+ scale_colour_manual( values = c("orange1", "midnightblue"))

data %>%
  gather(group, value,  c("PVT_Pre_Wrong", "PVT_Post_Wrong")) %>%
  ggplot(aes(x = value, fill = group)) +
  geom_histogram(alpha = 0.4, color = "black", position = "identity", aes(y = ..density..)) +
  scale_fill_manual(values = c("green", "mediumvioletred"),labels = c("Pre", "Post")) + 
  geom_density(aes(linetype=group, color=group, fill=NA), size=1.4,  show.legend = F)+
  guides(fill=guide_legend(title="Wrong Responses")) + 
  ggtitle("Wrong PVT Responses Pre and Post") +  
  labs(caption = "n=223", x = "Wrong Responses")  + 
  theme(legend.position = c(0.7, 0.7))+ scale_colour_manual( values = c("green", "mediumvioletred"))
