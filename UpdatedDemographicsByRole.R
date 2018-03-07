##############################################################################
# Libs
##############################################################################

library(dplyr)
library(pastecs)
library(doBy)
library(ggplot2)
library(car)
library(reshape2)
library(data.table)
recode <- car::recode
library(ggpubr)
library(sm)


##############################################################################
# Read
##############################################################################
#Windows
#Heather updated to change to newest file and fixed syntax
data<-read.csv(file="C:\\Users\\span\\Desktop\\Heather Misc\\Questionnaires11022017updated.csv", header = TRUE, sep = ",")
#Mac
#data<-read.csv(file="//Users/nprause/Dropbox/SPAN/Business/OneTaste.Research/Data/Questionnaire_Backup_6.15.17.csv",header = TRUE, sep = ",",skip=1) 
#data[data==-999] <- NA #Define missing

#####Cleaning already done for this 11022017 file


#Merge 6191
#Copy over values from barely used line
data$education[2]<-data$education[3]
data$age[2]<-data$age[3]
data$White[2]<-data$White[3]
data$Which.of.these.best.describes.your.ethnicity.[2]<-data$Which.of.these.best.describes.your.ethnicity.[3]
data$employment[2]<-data$employment[3]
data$income[2]<-data$income[3]
data$In.the.past.year..how.often.did.you.seek.help.from.a.counselor..psychologist..or.other.mental.hea...[2]<-data$In.the.past.year..how.often.did.you.seek.help.from.a.counselor..psychologist..or.other.mental.hea...[3]
data$relationship[2]<-data$relationship[3]
#Delete the superfluous line 
data<-data[-3, ]


###################################################################   Renaming   ######################################################################################################################################
#########################################################################################################################################################################################################
#########################################################################################################################################################################################################
#Basic renaming 
rename_me<-c("Subject.number","StartDate","What.is.your.age","education","gender","Indian","Asian","Islander","Black","White","employment","income","mental.health","current.relationship.status","with.your.romantic.partner","best.describes.your.current.relationship.with.your.OM.partner.today.","current.relationship.to.OM.","relationship.with.the.person.you.are.doing.OM","vibrator","sex.films","next.two.questions","AFTER.or.when.you.were.16","have.a.physical.orgasm","Polyamorous","..Open","..Swingers","..Traditional","Monogamish")
rename_me_to<-c("ID","StartDate","age","education","gender","Indian","Asian","Islander","Black","White","employment","income","mental.health","relationship","Overlap.image.primary","Overlap.image.OM.before.OM","OMstatus","OMtodayPsStatus","MinutesVibrator","MinutesPorn","ChildhoodSexualAssault","AdultSexualAssault","PhysicalClimax","Polyamorous","Open","Swingers","Traditional","Monogamish")
names(data)[282]<-"Overlap.image.OM.after.OM"
for (i in 1:length(rename_me)){
  if (length(rename_me)!=length(rename_me_to)){print("Check that string lengths are equal")}
  names(data)[grep(rename_me[i],names(data),value=F)]<-rename_me_to[i]
}


data$relationship.status<-ifelse(data$relationship==1,"Have a partner",ifelse(data$relationship==2,"Do not have a partner",NA));prop.table(table(data$relationship.status))
#heather added
table(data$relationship.status)


#Ethnicity
ethnicity.count<-cbind(table(data$Indian),table(data$White),table(data$Black),table(data$Asian),table(data$Islander))
ethnicity.prop<-100*(cbind((table(data$Indian)/sum(ethnicity.count[1,])),(table(data$White)/sum(ethnicity.count[1,])),(table(data$Black)/sum(ethnicity.count[1,])),(table(data$Asian)/sum(ethnicity.count[1,])),(table(data$Islander)/sum(ethnicity.count[1,]))))
ethnicity<-rbind(ethnicity.count,ethnicity.prop)
colnames(ethnicity)[1:5]<-c("Indian","White","Black","Asian","Islander")
rownames(ethnicity)<-c("Count","Proportion")
ethnicity

################################################BY ROLES###################################

strokeesage = stat.desc(data$age[data$Role==1])
strokersage = stat.desc(data$age[data$Role==2])
strokeesage
strokersage

####Transform porn and vib to be minutes 
data$MinutesPorn<-
  ifelse(data$MinutesPorn==1,0,
         ifelse(data$MinutesPorn==2,10,
                ifelse(data$MinutesPorn==3,20,
                       ifelse(data$MinutesPorn==4,30,
                              ifelse(data$MinutesPorn==5,40,
                                     ifelse(data$MinutesPorn==6,50,
                                            ifelse(data$MinutesPorn==7,60,
                                                   ifelse(data$MinutesPorn==8,70,
                                                          ifelse(data$MinutesPorn==9,80,
                                                                 ifelse(data$MinutesPorn==10,90,
                                                                        ifelse(data$MinutesPorn==13,120,
                                                                               ifelse(data$MinutesPorn==15,140,
                                                                                      ifelse(data$MinutesPorn==19,180,
                                                                                             ifelse(data$MinutesPorn==25,240,
                                                                                                    ifelse(data$MinutesPorn==26,250,
                                                                                                           ifelse(data$MinutesPorn==99,"Other",NA))))))))))))))));

data$MinutesVibrator<-
  ifelse(data$MinutesVibrator==1,0,
         ifelse(data$MinutesVibrator==2,10,
                ifelse(data$MinutesVibrator==3,20,
                       ifelse(data$MinutesVibrator==4,30,
                              ifelse(data$MinutesVibrator==5,40,
                                     ifelse(data$MinutesVibrator==6,50,
                                            ifelse(data$MinutesVibrator==7,60,
                                                   ifelse(data$MinutesVibrator==8,70,
                                                          ifelse(data$MinutesVibrator==9,80,
                                                                 ifelse(data$MinutesVibrator==10,90,
                                                                        ifelse(data$MinutesVibrator==13,120,
                                                                               ifelse(data$MinutesVibrator==15,140,
                                                                                      ifelse(data$MinutesVibrator==19,180,
                                                                                             ifelse(data$MinutesVibrator==25,240,
                                                                                                    ifelse(data$MinutesVibrator==26,250,
                                                                                                           ifelse(data$MinutesVibrator==99,"Other",NA))))))))))))))));



######   PORN #######

#### Percentage of people who answered 0 to the porn question) 
sum(data$MinutesPorn[data$Role==1] %in% 0 ) / nrow(data)
sum(data$MinutesPorn[data$Role==2] %in% 0 ) / nrow(data)

## stats excluding the 0 porn people
stat.desc(data$MinutesPorn[data$MinutesPorn!=0]) 

stat.desc(data$MinutesPorn[data$MinutesPorn!=0][data$Role==1]) 
stat.desc(data$MinutesPorn[data$MinutesPorn!=0][data$Role==2]) 


#######  VIBE ###########

#### Percentage of people who answered 0 to the vibe question) 
sum(data$MinutesVibrator[data$Role==1] %in% 0 ) / nrow(data)
sum(data$MinutesVibrator[data$Role==2] %in% 0 ) / nrow(data)

## stats excluding the 0 vibe people
stat.desc(data$MinutesVibrator[data$MinutesVibrator!=0]) 

stat.desc(data$MinutesVibrator[data$MinutesVibrator!=0][data$Role==1]) 
stat.desc(data$MinutesVibrator[data$MinutesVibrator!=0][data$Role==2]) 

###################################################################Psychometrics RENAMED ####################################################
# Touch avoidance questinnaire (Andersen & Leibowitz, 1978), higher=more avoidant
# Avgs: opposite-sex,men=12.9,females 14.85;same-sex men=26.43 females=21.70
#First loop just renames the lines to start with touchavoidance
Touchstart<-agrep("Touch.Avoidance.Questionnaire.This.instrument.is.composed.of.18.statements.concerning.how.you.fee....1..A.hug.from.a.same.sex.friend.is.a.true.sign.of.friendship.", names(data))

for (i in 1:18){
  names(data)[((Touchstart-1)+i)]<-paste0("TouchAvoidance",i) # Hard coded line
}




data$TouchAvoidance4<-recode(data$TouchAvoidance4,"5=1;4=2;2=4;1=5")
data$TouchAvoidance7<-recode(data$TouchAvoidance7,"5=1;4=2;2=4;1=5")
data$TouchAvoidance8<-recode(data$TouchAvoidance8,"5=1;4=2;2=4;1=5")
data$TouchAvoidance16<-recode(data$TouchAvoidance16,"5=1;4=2;2=4;1=5")
data$TouchAvoidance1r<-recode(data$TouchAvoidance18,"5=1;4=2;2=4;1=5")
data$OppositeSexTouchAvoidanceScore<-(data$TouchAvoidance2+data$TouchAvoidance5+data$TouchAvoidance7+data$TouchAvoidance8+data$TouchAvoidance10+data$TouchAvoidance14+data$TouchAvoidance15+data$TouchAvoidance17)
data$SameSexTouchAvoidanceScore<-(data$TouchAvoidance1+data$TouchAvoidance3+data$TouchAvoidance4+data$TouchAvoidance6+data$TouchAvoidance9+data$TouchAvoidance11+data$TouchAvoidance12+data$TouchAvoidance13+data$TouchAvoidance16+data$TouchAvoidance18)
data$TouchAvoidanceScore<-(data$SameSexTouchAvoidanceScore+data$OppositeSexTouchAvoidanceScore)
oppsex<-stat.desc(data$OppositeSexTouchAvoidanceScore)
samesex<-stat.desc(data$SameSexTouchAvoidanceScore)


stat.desc(data$OppositeSexTouchAvoidanceScore[data$Role==1])
stat.desc(data$OppositeSexTouchAvoidanceScore[data$Role==2])

stat.desc(data$SameSexTouchAvoidanceScore[data$Role==1])
stat.desc(data$SameSexTouchAvoidanceScore[data$Role==2])


######relationships 
table(data$relationship)

ValidRelationshipcount <- nrow(data) - count(data[is.na(data$relationship),])

table(data$relationship[data$Role==1])
table(data$relationship[data$Role==2])

#Ethnicity
strokeeEthnicity.count<-cbind(table(data$Indian[data$Role==1]),table(data$White[data$Role==1]),table(data$Black[data$Role==1]),table(data$Asian[data$Role==1]),table(data$Islander[data$Role==1]))
strokeeEthnicity.prop<-100*(cbind((table(data$Indian[data$Role==1])/sum(strokeeEthnicity.count[1,])),(table(data$White[data$Role==1])/sum(strokeeEthnicity.count[1,])),(table(data$Black[data$Role==1])/sum(strokeeEthnicity.count[1,])),(table(data$Asian[data$Role==1])/sum(strokeeEthnicity.count[1,])),(table(data$Islander[data$Role==1])/sum(strokeeEthnicity.count[1,]))))
strokeeEthnicity<-rbind(strokeeEthnicity.count,strokeeEthnicity.prop)
colnames(strokeeEthnicity)[1:5]<-c("Indian","White","Black","Asian","Islander")
rownames(strokeeEthnicity)<-c("Count","Proportion")
strokeeEthnicity

strokerEthnicity.count<-cbind(table(data$Indian[data$Role==2]),table(data$White[data$Role==2]),table(data$Black[data$Role==2]),table(data$Asian[data$Role==2]),table(data$Islander[data$Role==2]))
strokerEthnicity.prop<-100*(cbind((table(data$Indian[data$Role==2])/sum(strokerEthnicity.count[1,])),(table(data$White[data$Role==2])/sum(strokerEthnicity.count[1,])),(table(data$Black[data$Role==2])/sum(strokerEthnicity.count[1,])),(table(data$Asian[data$Role==2])/sum(strokerEthnicity.count[1,])),(table(data$Islander[data$Role==2])/sum(strokerEthnicity.count[1,]))))
strokerEthnicity<-rbind(strokerEthnicity.count,strokerEthnicity.prop)
colnames(strokerEthnicity)[1:5]<-c("Indian","White","Black","Asian","Islander")
rownames(strokerEthnicity)<-c("Count","Proportion")
strokerEthnicity


#####Education
data$education<-ifelse(data$education==1,"Less than 7th grade",
                       ifelse((data$education>1 & data$education<6),"some high school",
                              ifelse(data$education==6,"high school grad", 
                                     ifelse((data$education>6 & data$education<10),"some college",
                                            ifelse((data$education==10|data$education==11),"College grad",
                                                   ifelse(data$education==12,"Masters",
                                                          ifelse(data$education>12,"Beyond masters",99)))))))
table(data$education)
sort(100*prop.table(table(data$education)))
sort(100*prop.table(table(data$education)))[1]+sort(100*prop.table(table(data$education)))[2]
data$employment<-ifelse(data$employment==1,"Full time",ifelse(data$employment==2,"Part time",ifelse(data$employment==3,"Unemployed",ifelse(data$employment==4,"No paid work",NA))));100*prop.table(table(data$employment))

table(data$education[data$Role==1])
table(data$education[data$Role==2])


sort(100*prop.table(table(data$education[data$Role==1])))
sort(100*prop.table(table(data$education[data$Role==2])))

# OM partner today
data$OMPsToday<-
  ifelse((data$OMtodayPsStatus==1|data$OMtodayPsStatus==2),"Romantic partner",
         ifelse((data$OMtodayPsStatus==3|data$OMtodayPsStatus==4),"Reg OM Ps only",
                ifelse(data$OMtodayPsStatus==5,"Considering romantic",
                       ifelse(data$OMtodayPsStatus==6,"Other",NA)))); 
table(data$OMPsToday);100*prop.table(table(data$OMPsToday))
data$OMPsToday_binary<-recode(data$OMPsToday,"'Romantic partner'=1;'Reg OM Ps only'=2;else=NA")

table(data$OMPsToday[data$Role==1]);100*prop.table(table(data$OMPsToday[data$Role==1]))
table(data$OMPsToday[data$Role==2]);100*prop.table(table(data$OMPsToday[data$Role==2]))



#Assault
table(data$ChildhoodSexualAssault);100*prop.table(table(data$ChildhoodSexualAssault))
table(data$AdultSexualAssault);100*prop.table(table(data$AdultSexualAssault))

table(data$ChildhoodSexualAssault[data$Role==1]);100*prop.table(table(data$ChildhoodSexualAssault[data$Role==1]))
table(data$AdultSexualAssault[data$Role==1]);100*prop.table(table(data$AdultSexualAssault[data$Role==1]))


table(data$ChildhoodSexualAssault[data$Role==2]);100*prop.table(table(data$ChildhoodSexualAssault[data$Role==2]))
table(data$AdultSexualAssault[data$Role==2]);100*prop.table(table(data$AdultSexualAssault[data$Role==2]))

table(data$PhysicalClimax[data$Role==1]);100*prop.table(table(data$PhysicalClimax[data$Role==1]))





table(data$PhysicalClimax[data$Role==2]);100*prop.table(table(data$PhysicalClimax[data$Role==2]))

