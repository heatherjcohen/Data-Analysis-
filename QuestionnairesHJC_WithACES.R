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
##############################################################################
# Read
##############################################################################
#Windows
#Heather updated to change to newest file and fixed syntax
data<-read.csv(file="C:\\Users\\span\\Downloads\\Partnered_meditation.8.25.17.csv",header = TRUE, sep = ",",skip=1) 
#Mac
#data<-read.csv(file="//Users/nprause/Dropbox/SPAN/Business/OneTaste.Research/Data/Questionnaire_Backup_6.15.17.csv",header = TRUE, sep = ",",skip=1) 
#data[data==-999] <- NA #Define missing

# Renaming
rename_me<-c("Subject.number","StartDate","What.is.your.age","education","gender","Indian","Asian","Islander","Black","White","employment","income","mental.health","current.relationship.status","with.your.romantic.partner","best.describes.your.current.relationship.with.your.OM.partner.today.","current.relationship.to.OM.","relationship.with.the.person.you.are.doing.OM","vibrator","sex.films","next.two.questions","AFTER.or.when.you.were.16","have.a.physical.orgasm","Polyamorous","..Open","..Swingers","..Traditional","Monogamish")
rename_me_to<-c("ID","StartDate","age","education","gender","Indian","Asian","Islander","Black","White","employment","income","mental.health","relationship","Overlap.image.primary","Overlap.image.OM.before.OM","OMstatus","OMtodayPsStatus","MinutesVibrator","MinutesPorn","ChildhoodSexualAssault","AdultSexualAssault","PhysicalClimax","Polyamorous","Open","Swingers","Traditional","Monogamish")
names(data)[291]<-"Overlap.image.OM.after.OM"
for (i in 1:length(rename_me)){
  if (length(rename_me)!=length(rename_me_to)){print("Check that string lengths are equal")}
  names(data)[grep(rename_me[i],names(data),value=F)]<-rename_me_to[i]
}

# Cleaning
select(data,c(ID,age,education,gender,relationship,StartDate)) # Inspect easier
badRows<-c(1,2,54,124,144) # Hard coded after visual inspection ---- Heather Updated ---
data<-data[-badRows, ]

#Inspect and update rows with IDs that aren't 4 digits/check for duplicates
#     HEATHER WILL ADD IN LOOP TO VIEW 3 digit codes
# merge rows 6&7
#check dupes
n_occur <- data.frame(table(data$ID))
n_occur <- n_occur[n_occur$Freq > 1,]
#check wonky IDs
data$ID<-as.character(data$ID)
wonky<- subset(data, !nchar(data$ID)==4 )
wonky<-select(wonky,c(ID,age,gender))
#Hardcoding fixing the wonky ones - added by Heather for 82517 csv
#Three Digits or Underscore
data$ID[5]<-"9991"
data$ID[12]<-"0702"
data$ID[13]<-"0701"
data$ID[51]<-"0531"
data$ID[58]<-"1661"
data$ID[62] <-"4492"
data$ID[72]<-"3321"
data$ID[104]<-"9332"
data$ID[105]<-"9331"
data$ID[106]<-"1382"
#Duplicates - replacing the SECOND incidence with extra 0 4402-> 44002
data$ID[113]<-"44001"
data$ID[112]<-"44002"
data$ID[128]<-"4902"
data$ID[129]<-"4901" #was dup 492
data$ID[145]<-"38501"
data$ID[146]<-"38502"
data$ID[147]<-"42101"
data$ID[143]<-"42102"

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

###############
# Scoring
###############
# PANAS
PANAS<-agrep("Sense.of..closeness",names(data))
PANASnames<-c("ClosenessToOthers","Happy","SexualArousal","Angry","Anxious","OMturnedOn","Amused")
for (i in 0:((length(PANASnames)-1))){
  names(data)[(PANAS[1]+i)]<-paste0("PANASpreOM_",PANASnames[i+1])
}
for (i in 0:((length(PANASnames)-1))){
  names(data)[(PANAS[2]+i)]<-paste0("PANASpostOM_",PANASnames[i+1])
}

###################################################CHECK THIS PART PLEASE#####################################################################################

#Makes a prepost Table
keeps<-c(names(data)[(grep("PANASpreOM", names(data)))],names(data)[(grep("PANASpostOM", names(data)))])
d <- data[keeps]
d <- reshape(d, 
             varying = names(d), 
             v.names = "Rating",
             timevar = "Emotion", 
             times = names(d), 
             new.row.names = 1:2557584,
             direction="long")
d<-summaryBy(Rating ~ Emotion, data = d,FUN =c(mean,var,sd),na.rm=TRUE)
#Heather Switched
d$PrePost<-c(2,2,2,2,2,2,2,1,1,1,1,1,1,1)
#Heather ADDED IN TO MATCH LABELS
PANASnames<-sort(PANASnames)
d$EmotionCode<-c(PANASnames,PANASnames)


#Plots the table
ggplot(d,aes(x=PrePost, y=Rating.mean, colour=as.factor(EmotionCode))) + 
  #geom_errorbar(aes(ymin=Rating.mean-Rating.var, ymax=Rating.mean+Rating.var), width=.1) +
  geom_line(size=1.5)+
  geom_point()+
  scale_x_continuous(breaks = 1:2, labels = c("Before OM","AfterOM")) +
  labs(x = "Timing", y = "Rating",colour="Emotion rated") +
  ggtitle("Reported emotions before\nand after OM") +
  theme(plot.title = element_text(hjust = 0.5))

for (i in 1:nrow(d)){if (grepl("pre",d$Emotion[i])==TRUE){d$PrePost[i]<-1}  else {d$PrePost[i]<-2}}
for (i in 1:nrow(d)){
  if (grepl("Closeness",d$Emotion[i])==TRUE){d$EmotionLabel[i]<-"Closeness"}
  if (grepl("Amused",d$Emotion[i])==TRUE){d$EmotionLabel[i]<-"Amused"}
  if (grepl("Angry",d$Emotion[i])==TRUE){d$EmotionLabel[i]<-"Angry"}
  if (grepl("Anxious",d$Emotion[i])==TRUE){d$EmotionLabel[i]<-"Anxious"}
  if (grepl("Happy",d$Emotion[i])==TRUE){d$EmotionLabel[i]<-"Happy"}
  if (grepl("OMturnedOn",d$Emotion[i])==TRUE){d$EmotionLabel[i]<-"OM on"}
  if (grepl("SexualArousal",d$Emotion[i])==TRUE){d$EmotionLabel[i]<-"Sexually aroused"}
}


##########Strokee/Stroker split of same chart#####################################################################################################
data$ID <- as.numeric(as.character(data$ID))
data$strokestatus<-sprintf('%01d', data$ID %% 10)

Strokees<- filter(data,data$strokestatus ==1)
Strokers<- filter(data,data$strokestatus ==2)

#Makes a prepost Table STROKERS
keeps01<-c(names(Strokers)[(grep("PANASpreOM", names(Strokers)))],names(Strokers)[(grep("PANASpostOM", names(Strokers)))])
d01 <- Strokers[keeps01]
d01 <- reshape(d01, 
               varying = names(d01), 
               v.names = "Rating",
               timevar = "Emotion", 
               times = names(d01), 
               new.row.names = 1:2557584,
               direction="long")
d01<-summaryBy(Rating ~ Emotion, data = d01,FUN =c(mean,var,sd),na.rm=TRUE)

#Heather Switched
d01$PrePost<-c(2,2,2,2,2,2,2,1,1,1,1,1,1,1)
#Heather ADDED IN TO MATCH LABELS
PANASnames<-sort(PANASnames)
d01$EmotionCode<-c(PANASnames,PANASnames)
d01$EmotionLabel<-d$EmotionLabel
d01$type<-c("default","default","default","weird","default","default","default","default","default","default", "weird","default","default","default")


#Plots the table
ggplot(d01,aes(x=PrePost, y=Rating.mean, colour=as.factor(EmotionCode))) + 
  geom_point()+
  geom_line(aes(linetype=type, size=.5))+
  scale_x_continuous(breaks = 1:2, labels = c("Before OM","AfterOM")) +
  labs(x = "Timing", y = "Rating",colour="Emotion rated") +
  ggtitle("Reported emotions before\nand after OM of Strokers") +
  theme(plot.title = element_text(hjust = 0.5))


################################################Strokee PrePost##########################################
#Makes a prepost Table Strokees
keeps02<-c(names(Strokees)[(grep("PANASpreOM", names(Strokees)))],names(Strokees)[(grep("PANASpostOM", names(Strokees)))])
d02 <- Strokees[keeps02]
d02 <- reshape(d02, 
               varying = names(d02), 
               v.names = "Rating",
               timevar = "Emotion", 
               times = names(d02), 
               new.row.names = 1:2557584,
               direction="long")
d02<-summaryBy(Rating ~ Emotion, data = d02,FUN =c(mean,var,sd),na.rm=TRUE)
#Heather Switched
d02$PrePost<-c(2,2,2,2,2,2,2,1,1,1,1,1,1,1)
#Heather ADDED IN TO MATCH LABELS
PANASnames<-sort(PANASnames)
d02$EmotionCode<-c(PANASnames,PANASnames)

#Plots the table
ggplot(d02,aes(x=PrePost, y=Rating.mean, colour=as.factor(EmotionCode))) + 
  #geom_errorbar(aes(ymin=Rating.mean-Rating.var, ymax=Rating.mean+Rating.var), width=.1) +
  geom_line(size=1.5)+
  geom_point()+
  scale_x_continuous(breaks = 1:2, labels = c("Before OM","AfterOM")) +
  labs(x = "Timing", y = "Rating",colour="Emotion rated") +
  ggtitle("Reported emotions before\nand after OM for the Strokee") +
  theme(plot.title = element_text(hjust = 0.5))

d02$EmotionLabel<-d$EmotionLabel


####XXXX -- THIS THROWS AN ERROR WHAT DOES IT DO???? #######
d2 <- reshape(d, 
             timevar = "EmotionLabel", 
             idvar = c("Rating","PrePost"),
             direction="wide")
fit <- manova(cbind() ~ A*B)
summary(fit, test="Pillai")

# QIDS
# Published: range 0 to 27, remission <=6
QIDSstart<-agrep("Quick.Inventory.of.Depressive.Symptomatology",names(data))
for (i in 1:15){
  names(data)[(QIDSstart+i)]<-paste0("QIDS",names(data[(QIDSstart+i)]))
}
data$QIDS<-rowSums(data[QIDSstart:(QIDSstart+15)],na.rm = TRUE,dims=1)

# FSFI check if these are collecting XXX
FSFIstart<-agrep("ask.about.your.sexual.feelings.and.responses.",names(data))
for (i in 1:19){
  names(data)[(FSFIstart+i)]<-paste0("FSFI",i)
}

# Anxiety sensitivity index (Taylor et al, 2017, 18 item)
for (i in 1:18){
  names(data)[(159+i)]<-paste0("AnxietySensitivity",i) # Hard coded line
}

# Touch avoidance questinnaire (Andersen & Leibowitz, 1978), higher=more avoidant
# Avgs: opposite-sex,men=12.9,females 14.85;same-sex men=26.43 females=21.70
#First loop just renames the lines to start with touchavoidance
for (i in 1:18){
  names(data)[(177+i)]<-paste0("TouchAvoidance",i) # Hard coded line
}
data$TouchAvoidance4r<-recode(data$TouchAvoidance4,"5=1;4=2;2=4;1=5")
data$TouchAvoidance7r<-recode(data$TouchAvoidance7,"5=1;4=2;2=4;1=5")
data$TouchAvoidance8r<-recode(data$TouchAvoidance8,"5=1;4=2;2=4;1=5")
data$TouchAvoidance16r<-recode(data$TouchAvoidance16,"5=1;4=2;2=4;1=5")
data$TouchAvoidance18r<-recode(data$TouchAvoidance18,"5=1;4=2;2=4;1=5")
data$OppositeSexTouchAvoidanceScore<-(data$TouchAvoidance2+data$TouchAvoidance5+data$TouchAvoidance7r+data$TouchAvoidance8r+data$TouchAvoidance10+data$TouchAvoidance14+data$TouchAvoidance15+data$TouchAvoidance17)
data$SameSexTouchAvoidanceScore<-(data$TouchAvoidance1+data$TouchAvoidance3+data$TouchAvoidance4r+data$TouchAvoidance6+data$TouchAvoidance9+data$TouchAvoidance11+data$TouchAvoidance12+data$TouchAvoidance13+data$TouchAvoidance16r+data$TouchAvoidance18r)
data$TouchAvoidanceScore<-(data$SameSexTouchAvoidanceScore+data$OppositeSexTouchAvoidanceScore)
oppsex<-stat.desc(data$OppositeSexTouchAvoidanceScore)
samesex<-stat.desc(data$SameSexTouchAvoidanceScore)
###############
# Demographics
###############
##Heather modified column calls so they would run
age<-stat.desc(data$age)
pornminutes<-stat.desc(data[39])
vibmin<-stat.desc(data[40])

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
data$relationship.status<-ifelse(data$relationship==1,"Have a partner",ifelse(data$relationship==2,"Do not have a partner",NA));prop.table(table(data$relationship.status))
#heather added
table(data$relationship.status)

#
table(data$ChildhoodSexualAssault);100*prop.table(table(data$ChildhoodSexualAssault))
table(data$AdultSexualAssault);100*prop.table(table(data$AdultSexualAssault))
table(data$PhysicalClimax);100*prop.table(table(data$PhysicalClimax))
CNM<-data.frame(cbind(sum(data$Traditional,na.rm=TRUE),sum(data$Monogarelamish,na.rm=TRUE),sum(data$Polyamorous,na.rm=TRUE),sum(data$Swingers,na.rm=TRUE),sum(data$Open,na.rm=TRUE)))
names(CNM)<-c("Monogamous","Monogamish","Polyamorous","Swingers","Open")

# OM partner today
data$OMPsToday<-
  ifelse((data$OMtodayPsStatus==1|data$OMtodayPsStatus==2),"Romantic partner",
  ifelse((data$OMtodayPsStatus==3|data$OMtodayPsStatus==4),"Reg OM Ps only",
  ifelse(data$OMtodayPsStatus==5,"Considering romantic",
  ifelse(data$OMtodayPsStatus==6,"Other",NA)))); 
  table(data$OMPsToday);100*prop.table(table(data$OMPsToday))
data$OMPsToday_binary<-recode(data$OMPsToday,"'Romantic partner'=1;'Reg OM Ps only'=2;else=NA")

#Ethnicity
ethnicity.count<-cbind(table(data$Indian),table(data$White),table(data$Black),table(data$Asian),table(data$Islander))
ethnicity.prop<-100*(cbind((table(data$Indian)/sum(ethnicity.count[1,])),(table(data$White)/sum(ethnicity.count[1,])),(table(data$Black)/sum(ethnicity.count[1,])),(table(data$Asian)/sum(ethnicity.count[1,])),(table(data$Islander)/sum(ethnicity.count[1,]))))
ethnicity<-rbind(ethnicity.count,ethnicity.prop)
colnames(ethnicity)[1:5]<-c("Indian","White","Black","Asian","Islander")
rownames(ethnicity)<-c("Count","Proportion")
ethnicity

#OM closeness histograms
ConnectednessTest<-t.test(data$Overlap.image.OM.before.OM,data$Overlap.image.OM.after.OM,paired=TRUE)
d <- reshape(data, 
             varying = c("Overlap.image.OM.before.OM","Overlap.image.OM.after.OM"), 
             v.names = "OverlapRating",
             timevar = "PrePostOM", 
             times = c("Overlap.image.OM.before.OM","Overlap.image.OM.after.OM"), 
             new.row.names = 1:1000,
             direction = "long")
r <- lmer(OverlapRating ~ OMPsToday_binary*PrePostOM + (1|ID),d)

par(mfrow=c(2,1))
hist(data$Overlap.image.OM.before.OM,main="Before OM",xlab=NULL)
mtext(paste0("Mean = ",round(stat.desc(data$Overlap.image.OM.before.OM)[9],2)),side=3,line=.5,cex = .8,col="blue")
hist(data$Overlap.image.OM.after.OM,main="After OM",xlab="Connectedness with OM partner")
mtext(paste0("Mean = ",round(stat.desc(data$Overlap.image.OM.after.OM)[9],2)),side=3,line=.5,cex = .8,col="blue")
mtext(paste0("t(",ConnectednessTest[2],") = ",round(as.numeric(ConnectednessTest[1]),2),", CI +/- = ",round(as.numeric(ConnectednessTest$conf.int[1]),2)," to ",round(as.numeric(ConnectednessTest$conf.int[2]),2)),side=1,line=2,cex = .8,col="red")

keeps <- c("ID","Overlap.image.OM.before.OM","Overlap.image.OM.after.OM")
d<-data[keeps]
d <- reshape(d, 
             varying = c("Overlap.image.OM.before.OM","Overlap.image.OM.after.OM"), 
             v.names = "Closeness",
             timevar = "PrePost", 
             times = c("Overlap.image.OM.before.OM","Overlap.image.OM.after.OM"), 
             new.row.names = 1:1000,
             direction = "long")
d$PrePost<-ifelse(d$PrePost=="Overlap.image.OM.before.OM",1,ifelse(d$PrePost=="Overlap.image.OM.after.OM",2,NA))

interaction.plot(d$PrePost,d$ID,jitter(d$Closeness,3),xlab="time", ylab="Overlap", col=c(1:10),legend=F) 

# add a title and subtitle 
title("Tree Growth", "example of line plot")

# add a legend 
legend(xrange[1], yrange[2], 1:ntrees, cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Tree")

alphavars<-c("study","ID","visit","time","alpha_FP1","alpha_FP2","alpha_F7","alpha_F3","alpha_FZ","alpha_F4","alpha_F8","alpha_FT7","alpha_FC3","alpha_FCZ","alpha_FC4","alpha_FT8","alpha_T3","alpha_C3","alpha_CZ","alpha_C4","alpha_T4","alpha_TP7","alpha_CP3","alpha_CPZ","alpha_CP4","alpha_TP8","alpha_A1","alpha_T5","alpha_P3","alpha_PZ","alpha_P4","alpha_T6","alpha_A2","alpha_O1","alpha_OZ","alpha_O2","alpha_FT9","alpha_FT10","alpha_PO1","alpha_PO2")
datar<-data[alphavars]


#HEATHER ACES
#Create subsegment to mess with
ACESdata<-select(data,c(ID,age,education,gender,relationship,OMPsToday,150:159))
#Rename to be easier
for (i in 1:10){
       names(ACESdata)[(6+i)]<-paste0("ACES",i) # Hard coded line
}

# Recode to make sense
ACESdata[7:16]<-
  ifelse((ACESdata[7:16]==1),0,
         ifelse((ACESdata[7:16]==3),1,NA));

#Calculate score
ACESdata$score<-ACESdata$ACES1+ACESdata$ACES2+ACESdata$ACES3+ACESdata$ACES4+ACESdata$ACES5+ACESdata$ACES6+ACESdata$ACES7+ACESdata$ACES8+ACESdata$ACES9+ACESdata$ACES10
data$ACESscore<-ACESdata$score



#Subsection Panas And Aces
PanPlusAce<-select(data,c(ID,age,education,gender,relationship,OMPsToday,ACESscore, 127:133,282:288))

#Create Prepost scores for PA and NA
PanPlusAce$PAPre<-PanPlusAce$PANASpreOM_Amused+PanPlusAce$PANASpreOM_Happy
PanPlusAce$PAPost<-PanPlusAce$PANASpostOM_Amused+PanPlusAce$PANASpostOM_Happy
PanPlusAce$NAPre<-PanPlusAce$PANASpreOM_Angry+PanPlusAce$PANASpreOM_Anxious
PanPlusAce$NAPost<-PanPlusAce$PANASpostOM_Angry+PanPlusAce$PANASpostOM_Anxious
JustScores<-select(PanPlusAce, c(ID,age,gender,relationship,OMPsToday,ACESscore,PAPre,PAPost,NAPre,NAPost))
JustScores$PADelta<-JustScores$PAPost-JustScores$PAPre
JustScores$NADelta<-JustScores$NAPost-JustScores$NAPre

#Subset of just high ace scores
HighAces<- filter(JustScores,JustScores$ACESscore >3)

#Plot
ggplot(HighAces, aes(factor(gender), ACESscore, fill = OMPsToday)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

#PA Delta 
ggplot(JustScores, aes(factor(gender), PADelta, fill = OMPsToday)) + 
       geom_bar(stat="identity", position = "dodge") + 
       scale_fill_brewer(palette = "Set1")

#NA Delta
ggplot(JustScores, aes(factor(gender), NADelta, fill = OMPsToday)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")


#Add columns to general DATA
data$ACESscore<-PanPlusAce$ACESscore
data$PAPre<-JustScores$PAPre
data$NAPre<-JustScores$NAPre
data$PAPost<-JustScores$PAPost
data$NAPost<-JustScores$NAPost
data$PADelta<-JustScores$PADelta
data$NADelta<-JustScores$NADelta

#reshape to long
k=setDT(JustScores)
j=setDT(HighAces)

#genpop
l<-melt(k[, -c("PADelta", "NADelta")], measure.vars=list(c("PAPre", "PAPost"), c("NAPre", "NAPost")), value.name=c("PAVal", "NAVal"), variable.name="prepost")
#highace
m<-melt(j[, -c("PADelta", "NADelta")], measure.vars=list(c("PAPre", "PAPost"), c("NAPre", "NAPost")), value.name=c("PAVal", "NAVal"), variable.name="prepost")

  
  
#Plots the table
#Genpop
interaction.plot(l$prepost,l$ID, l$NAVal, xlab="prepost", ylab="NAscore", col=c(1:10),legend=F)
interaction.plot(l$prepost,l$ID, l$PAVal, xlab="prepost", ylab="PAscore", col=c(1:10),legend=F)
#Highage
interaction.plot(m$prepost,m$ID, m$NAVal, xlab="prepost", ylab="NAscore", col=c(1:10),legend=F)
interaction.plot(m$prepost,m$ID, m$PAVal, xlab="prepost", ylab="PAscore", col=c(1:10),legend=F)

#Correlation workshape
justhappy <- subset(JustScores, select = c("ACESscore", "PADelta"))
justbad <- subset(JustScores, select = c("ACESscore", "NADelta"))

plot(justbad)
plot(justhappy)

sat.mod <- lm(ACESscore ~ PADelta, # regression formula
              data=justhappy) # data set
# Summarize and print the results
summary(sat.mod)


