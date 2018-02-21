library(dplyr)
library(pastecs)
library(doBy)
library(ggplot2)
library(car)
recode <- car::recode
##############################################################################
# Read
##############################################################################
#Windows
#Heather updated to change to newest file and fixed syntax
data<-read.csv(file="C:\\Users\\span\\Desktop\\Heather Misc\\ForDevelopingScriptsToExtractBehavioralData\\PVTtrytwo\\PVT Only Behavioral Data Summary PROCESSED.csv", header = TRUE, sep = ",")
badRows<-c(176,177,178,179) # Hard coded after visual inspection ---- Heather Updated ---
data<-data[-badRows, ]
#remove empty columns 
data<-data[,-c(2,3, 14, 15, 16)] 

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

t.test(data$PVT_Pre_Percent_Correct,data$PVT_Post_Percent_Correct,paired=TRUE)

###########################################################################3
hist(data$PVT_Pre_Percent_Correct)
hist(data$PVT_Post_Percent_Correct)

boxplot(Delta ~ Role, data = data,
        xlab = "Role", ylab = "PVT Score Change",
        main = "Change in PVT Scores by Role",
        col=(c("pink","blue"))
        
)

h<-subset(data, select=c("PVT_Pre_Percent_Correct", "PVT_Post_Percent_Correct", "Ids"))


library(reshape)
hh <- melt(h, id=c("Ids"))


hh$variable<-ifelse(hh$variable=="PVT_Pre_Percent_Correct","Pre",
                  ifelse(hh$variable=="PVT_Post_Percent_Correct", "Post",99))

names(hh)[2]<-"PrePost"
names(hh)[3]<-"PVT_Correct_Percent"

kk<- ggplot(hh, aes(x=PrePost, y=PVT_Correct_Percent), xlab = "Before or After", ylab = "PVT % Correct",
        main = "PVT Scores Before and After Oming") + geom_boxplot() 


