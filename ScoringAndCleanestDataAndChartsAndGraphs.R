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

# PANAS
PANAS<-agrep("Sense.of..closeness",names(data))
PANASnames<-c("ClosenessToOthers","Happy","SexualArousal","Angry","Anxious","OMturnedOn","Amused")
for (i in 0:((length(PANASnames)-1))){
  names(data)[(PANAS[1]+i)]<-paste0("PANASpreOM_",PANASnames[i+1])
}
for (i in 0:((length(PANASnames)-1))){
  names(data)[(PANAS[2]+i)]<-paste0("PANASpostOM_",PANASnames[i+1])
}


###################################################################Psychometrics RENAMED ####################################################
# Touch avoidance questinnaire (Andersen & Leibowitz, 1978), higher=more avoidant
# Avgs: opposite-sex,men=12.9,females 14.85;same-sex men=26.43 females=21.70
#First loop just renames the lines to start with touchavoidance
for (i in 1:18){
  names(data)[(177+i)]<-paste0("TouchAvoidance",i) # Hard coded line
}


#QIDS
#The start line is an instruction line so QIDS1-15 are the real data
QIDSstart<-agrep("Quick.Inventory.of.Depressive.Symptomatology",names(data))
for (i in 1:15){
  names(data)[(QIDSstart+i)]<-paste0("QIDS",i)
}
#FSFI
FSFIstart<-agrep("ask.about.your.sexual.feelings.and.responses.",names(data))
for (i in 1:19){
  names(data)[(FSFIstart+i)]<-paste0("FSFI",i)
}

# Anxiety sensitivity index (Taylor et al, 2017, 18 item)
#starting variable is the last question before since there is no intstruction only line
ASIstart<-agrep("Adverse.Childhood.Experiences.Survey.Before.your.18th.birthday..was.a.household.member.depressed.or.mentally.ill..or.did.a.household.member.attempt.suicide.", names(data))
for (i in 1:18){
  names(data)[(ASIstart+i)]<-paste0("AnxietySensitivity",i) 
}

#Attachment Style 
#starts one before since there is no instruction line
ASQstart <- agrep("How.often.do.you.experience.a.physical.climax.during.OM..Climax",names(data))
for (i in 1:36){
  names(data)[(ASQstart+i)]<-paste0("AttachmentStyle",i) 
}

#Assertiveness
ARMCstart <- agrep("Please.describe.to.what.extent.you.believe.that.these.describe.your.personality..defends.own.beliefs",names(data))
for (i in 1:10){
  names(data)[((ARMCstart-1)+i)]<-paste0("ARMC",i) 
}

#Bisbas
BisBasStart<-agrep("Behavioral.Inhibition.Behavioral.Approach.Scale.Each.item.of.this.questionnaire.is.a.statement.th....1..If.I.think.something.unpleasant.is.going.to.happen.I.usually.get.pretty..worked.up......" ,names(data))
for (i in 1:14){
  names(data)[((BisBasStart[1]-1)+i)]<-paste0("BisBas",i) 
}

# Need for Affect Questionnaire (NAQ)
NAQstart <- agrep("Need.for.Affect.Please.read.each.item.carefully.and.indicate.how.strongly.you.agree.or.disagree.w....If.I.reflect.on.my.past..I.see.that.I.tend.to.be.afraid.of.feeling.emotion"  ,names(data))
for (i in 1:10){
  names(data)[((NAQstart-1)+i)]<-paste0("NAQ",i) 
}

# Need for Sensation Seeking (NISS)
NISSstart<-agrep("Need.for.Sensation.Seeking.Below.is.a.set.of.statements.regarding.attitudes.and.behavior..Read.ea....1..I.like.to.find.myself.in.situations.which.make.my.heart.beat.faster",names(data))
for (i in 1:17){
  names(data)[((NISSstart-1)+i)]<-paste0("NISS",i) 
}

# Flow State (FSS)
FSSstart <- agrep("After.meditation.practice.Flow.State.Scale.Please.answer.the.following.questions.in.relation.to.y....1..I.was.challenged..but.I.believed.my.skills.would.allow.me.to.meet.the.challenge.",names(data))
for (i in 1:36){
  names(data)[((FSSstart-1)+i)]<-paste0("FSS",i) 
}

#ACES
ACESstart<-agrep("Adverse.Childhood.Experiences.Survey.Before.your.18th.birthday..did.a.parent.or.other.adult.in.the.household.often.or.very.often.swear.at.you..insult.you..put.you.down..or.humiliate.you...span.style..color..FF0000....strong.or..strong...span..act.in.a.way", names(data))
for (i in 1:10){
  names(data)[((ACESstart-1)+i)]<-paste0("ACES",i) 
}

###################################################################   SCORING  ######################################################################################################################################
#########################################################################################################################################################################################################
#########################################################################################################################################################################################################

##########################################################   Touch Avoidant  ########################################################################################

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

##########################################################   ACES   ########################################################################################
ACESdata <- data[,grepl("ACE",names(data))]
# Recode to make sense
ACESdata[1:10]<-
  ifelse((ACESdata[1:10]==1),0,
         ifelse((ACESdata[1:10]==3),1,NA));


ACESdata$ACESscore<- ACESdata$ACEscore<-ACESdata$ACES1+ACESdata$ACES2+ACESdata$ACES3+ACESdata$ACES4+ACESdata$ACES5+ACESdata$ACES6+ACESdata$ACES7+ACESdata$ACES8+ACESdata$ACES9+ACESdata$ACES10
#Calculate score
data$ACESscore<-ACESdata$ACESscore
##########################################################   QIDS   ########################################################################################
data$QIDS<-rowSums(data[QIDSstart:(QIDSstart+15)],na.rm = TRUE,dims=1)

##########################################################  Anxiety ########################################################################################
ASIdf <- data[,grepl("AnxietySensitivity",names(data))]
#Scoring Requires A Minus One To All Cells (data comes in 1-5, needs to be 0-4)
ASIdf<-ASIdf-1
#Total out of 72 
ASIdf$Score<-rowSums(ASIdf,na.rm = TRUE,dims=1)
#Add back to main dataframe
data$ASIScore<-ASIdf$Score


################################ Attachment Style Questionnaire (ASQ) #####################################
ASQdf <- data[,grepl("AttachmentStyle",names(data))]
#attachement anxiety is questions: 2,4,5,8,9,10,11,13,14,15,18,20,23,25,28,31,33,34
#attachment avoidance is questions: 1,2,6,7,12,16,17,19,21,22,24,26,27,29,30,32,35,36
#questions to reverse key are: 1,4,6,10,12,16,21,22,26,27,29,30,35,36
#info from:  http://internal.psychology.illinois.edu/~rcfraley/measures/ecrritems.htm
######Reverse Keying##########
#Cols to Reverse
ASQdf[c(1,4,6,10,12,16,21,22,26,27,29,30,35,36)]<-(8-ASQdf[c(1,4,6,10,12,16,21,22,26,27,29,30,35,36)])
#Get Scores
ASQdf$AttachmentAnxietyScore <- rowMeans(ASQdf[c(2,4,5,8,9,10,11,13,14,15,18,20,23,25,28,31,33,34)])
ASQdf$AttachmentAvoidScore<-rowMeans(ASQdf[c(1,2,6,7,12,16,17,19,21,22,24,26,27,29,30,32,35,36)])
#Add back to main dataframe
data$AttachmentAnxietyScore<-ASQdf$AttachmentAnxietyScore
data$AttachmentAvoidScore<-ASQdf$AttachmentAvoidScore

################################ Assertiveness scale -Richmond & McCroskey (ARMC) #####################################
ARMCdf <- data[,grepl("ARMC",names(data))]
#Sum Score
ARMCdf$ARMCScore <-rowSums(ARMCdf,na.rm = TRUE,dims=1)
#Add back to main dataframe
data$ARMCScore<-ARMCdf$ARMCScore


################################ Behavioral Inhibition / Behavioral Approach Scale (BisBas) #####################################

BisBasdf <- data[,grepl("BisBas",names(data))]
#Reverse key all by 6 & 8
#BAS Drive: 15,14,16,17
#BAS Fun: 20,18,21,19
#BAS Reward: 10,9,13,11,12
#BIS:6,3,4,5,7,8,2
#Missing numbers are filler items 
############Cols to Reverse########
BisBasdf[c(1:5,7,9:17)]<-(5-BisBasdf[c(1:5,7,9:17)])
#######Scores #######
BisBasdf$BASDrive <-rowSums(BisBasdf[15:17],na.rm = TRUE,dims=1)
BisBasdf$BASFun <-rowSums(BisBasdf[19:20],na.rm = TRUE,dims=1)
BisBasdf$BASReward <-rowSums(BisBasdf[9:13],na.rm = TRUE,dims=1)
BisBasdf$BISScore <-rowSums(BisBasdf[2:8],na.rm = TRUE,dims=1)
#Add back to main dataframe
data$BASDrive <-BisBasdf$BASDrive 
data$BASFun <-BisBasdf$BASFun 
data$BASReward <-BisBasdf$BASReward 
data$BISScore <-BisBasdf$BISScore 


################################ Need for Affect Questionnaire (NAQ) #####################################
#AV = avoidance subscale 1,4,6,7,10
#AP = approach scale  2,3,5,8,9
#Reverse key- AV and then aggregate 
#1-7 data needs to be -3 to +3 
# 1,2,3,4,5,6,7 becomes
#-3, -2,-1,0,1,2,3 so, -4 to all then, to reverse key AV *-1 

#Seperate, rename 
NAQdf <- data[,grepl("NAQ",names(data))]

#Rescale all
NAQdf<-(NAQdf-4)
#Reverse Key AV
NAQdf[,c(1,4,6,7,10)] <-(NAQdf[,c(1,4,6,7,10)]*(-1))
#Sum AP
NAQdf$NAQAP <-rowSums(NAQdf[,c(2,3,5,8,9)],na.rm = TRUE,dims=1)
#Sum AV
NAQdf$NAQAV <-rowSums(NAQdf[,c(1,4,6,7,10)],na.rm = TRUE,dims=1)
#Total
NAQdf$NAQTotal <-NAQdf$NAQAP + NAQdf$NAQAV
#add back to data
data$NAQTotal<-NAQdf$NAQTotal




################################ Need for Sensation Seeking (NISS) #####################################

#Seperate, rename 
NISSdf <- data[,grepl("NISS",names(data))]
##there is ambiguity in the papers about how to handle the subscores, whether AR should count at all
#and, if so, whether it should be reverse scored or weighted 
#Per Nikky, add up the subscores, combine to make one score, save all three


#Sum NS
NISSdf$NISS_NS <-rowSums(NISSdf[,c(1,2,3,4,5,6,7,13,14,15,16)],na.rm = TRUE,dims=1)
#Sum AR
NISSdf$NISS_AR <-rowSums(NISSdf[,c(8,9,10,11,12,17)],na.rm = TRUE,dims=1)
#Total
NISSdf$TotalNISS <- NISSdf$NISS_NS + NISSdf$NISS_AR 

#add back to data
data$NISSTotal <- NISSdf$TotalNISS 
data$NISS_NS <- NISSdf$NISS_NS 
data$NISS_AR <- NISSdf$NISS_AR 

####################################### Flow State (FSS)   ############################################################################## 

#Challenge-skill balance (1,10,19,28)
#Action-awareness merging (2,11,20,29)
#Clear goals (3,12,21,30)
#Unamibgious feedback (4,13,22,31)
#Concentration on task at hand (5,14,23,32)
#Paradox of control (6,15,24,33)
#Loss of self-conciousness (7,16,25,34)
#Transformation of time (8,17,26,35)
#Autotelic experience (9,18,27,36)


#Seperate, rename 
FSSdf <- data[,grepl("FSS",names(data))]


#Sum Subtotals in order above
FSSdf$FSS_CSB <-rowSums(FSSdf[,c(1,10,19,28)],na.rm = TRUE,dims=1)
FSSdf$FSS_AAM <-rowSums(FSSdf[,c(2,11,20,29)],na.rm = TRUE,dims=1)
FSSdf$FSS_CG <-rowSums(FSSdf[,c(3,12,21,30)],na.rm = TRUE,dims=1)
FSSdf$FSS_UF <-rowSums(FSSdf[,c(4,13,22,31)],na.rm = TRUE,dims=1)
FSSdf$FSS_CoTaH <-rowSums(FSSdf[,c(5,14,23,32)],na.rm = TRUE,dims=1)
FSSdf$FSS_PoC <-rowSums(FSSdf[,c(6,15,24,33)],na.rm = TRUE,dims=1)
FSSdf$FSS_LSC <-rowSums(FSSdf[,c(7,16,25,34)],na.rm = TRUE,dims=1)
FSSdf$FSS_ToT <-rowSums(FSSdf[,c(8,17,26,35)],na.rm = TRUE,dims=1)
FSSdf$FSS_AE <-rowSums(FSSdf[,c(9,18,27,36)],na.rm = TRUE,dims=1)
#total
FSSdf$FFS_Total <- FSSdf$FSS_CSB + FSSdf$FSS_AAM + FSSdf$FSS_CG + FSSdf$FSS_UF + FSSdf$FSS_CoTaH + FSSdf$FSS_PoC + FSSdf$FSS_LSC + FSSdf$FSS_ToT + FSSdf$FSS_AE


#add back to data 

data$FSS_CSB <- FSSdf$FSS_CSB 
data$FSS_AAM <- FSSdf$FSS_AAM 
data$FSS_CG <- FSSdf$FSS_CG 
data$FSS_UF<- FSSdf$FSS_UF
data$FSS_CoTaH <- FSSdf$FSS_CoTaH
data$FSS_PoC <- FSSdf$FSS_PoC 
data$FSS_LSC<- FSSdf$FSS_LSC
data$FSS_ToT <- FSSdf$FSS_ToT 
data$FSS_AE <- FSSdf$FSS_AE 
data$FSS_Total <-FSSdf$FFS_Total


###################################################################   Charts and Graphs ######################################################################################################################################
#########################################################################################################################################################################################################
#########################################################################################################################################################################################################

barplot(table(data$ACESscore))
barplot(table(data$QIDS))
barplot(table(data$ASIScore))



Scores <-subset(data, select = c("ACESscore", "QIDS", "FFS_Total","NISS_NS", "NAQTotal","TotalNISS","NISS_AR", "ASIScore", "AttachmentAnxietyScore", "AttachmentAvoidScore", "PANASpostOM_ClosenessToOthers", "PANASpreOM_ClosenessToOthers"))
Scores <-na.omit(Scores)

cor(Scores$ACESscore,Scores$QIDS)
plot(Scores$ACESscore, Scores$QIDS)
cor(Scores$ACESscore,Scores$ASIScore)
plot(Scores$ACESscore,Scores$ASIScore)
cor(Scores$ACESscore,Scores$AttachmentAnxietyScore)
plot(Scores$ACESscore,Scores$AttachmentAnxietyScore)
cor(Scores$ACESscore,Scores$AttachmentAvoidScore)
plot(Scores$ACESscore,Scores$AttachmentAvoidScore)


HighAces2<- filter(Scores,Scores$ACESscore >3)


cor(HighAces2$ACESscore,HighAces2$QIDS)
plot(HighAces2$ACESscore, HighAces2$QIDS)
cor(HighAces2$ACESscore,HighAces2$ASIScore)
plot(HighAces2$ACESscore,HighAces2$ASIScore)
cor(HighAces2$ACESscore,HighAces2$AttachmentAnxietyScore)
plot(HighAces2$ACESscore,HighAces2$AttachmentAnxietyScore)
cor(HighAces2$ACESscore,HighAces2$AttachmentAvoidScore)
plot(HighAces2$ACESscore,HighAces2$AttachmentAvoidScore)


barplot(table(data$PANASpreOM_ClosenessToOthers))
barplot(table(data$PANASpostOM_ClosenessToOthers))

cor(Scores$PANASpreOM_ClosenessToOthers, Scores$PANASpostOM_ClosenessToOthers)
plot(Scores$PANASpreOM_ClosenessToOthers, Scores$PANASpostOM_ClosenessToOthers)

cor(HighAces2$PANASpreOM_ClosenessToOthers, HighAces2$PANASpostOM_ClosenessToOthers)
plot(HighAces2$PANASpreOM_ClosenessToOthers, HighAces2$PANASpostOM_ClosenessToOthers)

barplot(table(HighAces2$PANASpreOM_ClosenessToOthers))
barplot(table(HighAces2$PANASpostOM_ClosenessToOthers))


t.test(Scores$PANASpreOM_ClosenessToOthers,Scores$PANASpostOM_ClosenessToOthers,paired=TRUE)

cor(Scores$ACESscore, Scores$PANASpreOM_ClosenessToOthers)
plot(Scores$ACESscore, Scores$PANASpreOM_ClosenessToOthers)

cor(Scores$ACESscore, Scores$PANASpostOM_ClosenessToOthers)
plot(Scores$ACESscore, Scores$PANASpostOM_ClosenessToOthers)

PrePost <- subset(Scores, select = c("PANASpostOM_ClosenessToOthers", "PANASpreOM_ClosenessToOthers", "ACESscore"))
PrePost<-data.table(PrePost) 
names(PrePost)[names(PrePost) == 'PANASpostOM_ClosenessToOthers'] <-"Post"
names(PrePost)[names(PrePost) == 'PANASpreOM_ClosenessToOthers'] <-"Pre"
PrePost<- melt(PrePost, measure.vars=list(c("Post", "Pre")), value.name=c("PANASCloseness"), variable.name="prepost")


boxplot(PrePost$PANASCloseness~PrePost$prepost)
boxplot(PANASCloseness~prepost*ACESscore, data=PrePost)

boxplot(Scores$PANASpreOM_ClosenessToOthers, Scores$PANASpostOM_ClosenessToOthers, main="Closeness before and after OM", xlab = "Closeness Rating",
        horizontal=TRUE, names=c("Pre-OM","Post- OM")) 


wilcox.test(Scores$PANASpreOM_ClosenessToOthers, Scores$PANASpostOM_ClosenessToOthers,paired=TRUE,alternative="two.sided",mu=0,conf.int=TRUE,conf.level=0.95)
wilcox.test(Scores$PANASpreOM_ClosenessToOthers, Scores$PANASpostOM_ClosenessToOthers,paired=TRUE,alternative="less",mu=0,conf.int=TRUE,conf.level=0.95)




###########################################################################################
#Extract stroker/strokee  -- DONE in this version of the excel 
#Compare those types on relationship type/status - om arousal before/after - 
#experience histogram for each - exp at om v. om arousal, sex arousal, anxiety, attachment
#attachment style v baselevel closeness or anxiety 

#Redo scatter plots above with 1) Add fit line 2) Add "jitter" to get the dots off each other just enough to see
#plot(x ~y)
#lm(x~y)
#abline(lm(x~y))


theme_set(theme_bw())
boxplot(data$OMstatus~data$education, main="Education and OM status", 
        xlab="Years of Education", ylab="OM Status")


################################## Creaing Subset to plot without NA Issues###########################################

  
  
NoNANs<-subset(data, select = c("ACESscore", "Role","relationship","QIDS", "ASIScore", "AttachmentAnxietyScore", "AttachmentAvoidScore", "PANASpostOM_ClosenessToOthers", "PANASpreOM_ClosenessToOthers"))
NoNANs<-na.omit(NoNANs)

gg <- ggplot(data, aes(x=ACESscore, y=QIDS)) + 
  geom_point(aes(col=Role)) + 
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="ACES Score and QIDS by Role", 
       y="ACE", 
       x="QID", 
       title="Scatterplot")

plot(gg)

gg <- ggplot(data, aes(x=ACESscore, y=ASIScore)) + 
  geom_point(aes(col=Role)) + 
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="ACES Score and Anxiety by Role", 
       y="ACE", 
       x="Anxiety", 
       title="Scatterplot")

plot(gg)


x <- NoNANs$ACESscore
h<-hist(x, breaks=15, col="red", xlab="ACES", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x)) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)



x <- NoNANs$QIDS
h<-hist(x, breaks=15, col="red", xlab="QIDS", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x)) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

x <- NoNANs$ASIScore
h<-hist(x, breaks=15, col="red", xlab="Anxiety", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x)) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

x <- NoNANs$AttachmentAnxietyScore
h<-hist(x, breaks=15, col="red", xlab="Attachment Anxiety", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x)) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)




NoNANs$Role<-factor(NoNANs$Role, levels=c(1,2))


boxplot(ACESscore~interaction(Role, lex.order=T), col=c('mistyrose','powderblue'),  medcol=c("#FFDB00FF", "#B6FF00FF"), 
        whiskcol=c("#49FF00FF", "#00FF24FF"), 
        staplecol=c("#00FF92FF", "#00FFFFFF"), 
        boxcol=c("#0092FFFF", "#0024FFFF"), 
        outcol=c("#4900FFFF", "#B600FFFF"), 
        outbg=c("#FF00DB66", "#FF006D66"), xlab="Strokee(1) and Stroker(2)", ylab="ACE Score", NoNANs)

#########################################


boxplot(QIDS~interaction(Role, lex.order=T), col=c('mistyrose','powderblue'),  medcol=c("#FFDB00FF", "#B6FF00FF"), 
        whiskcol=c("#49FF00FF", "#00FF24FF"), 
        staplecol=c("#00FF92FF", "#00FFFFFF"), 
        boxcol=c("#0092FFFF", "#0024FFFF"), 
        outcol=c("#4900FFFF", "#B600FFFF"), 
        outbg=c("#FF00DB66", "#FF006D66"), NoNANs)

title(main="Depression and Role", sub="QID Score by Stroker/Strokee", 
      xlab="Strokee(1) & Stroker(2)", ylab="QID Score")

#########################################


boxplot(ASIScore~interaction(Role, lex.order=T), col=c('mistyrose','powderblue'),  medcol=c("#FFDB00FF", "#B6FF00FF"), 
        whiskcol=c("#49FF00FF", "#00FF24FF"), 
        staplecol=c("#00FF92FF", "#00FFFFFF"), 
        boxcol=c("#0092FFFF", "#0024FFFF"), 
        outcol=c("#4900FFFF", "#B600FFFF"), 
        outbg=c("#FF00DB66", "#FF006D66"), NoNANs)

title(main="Anxiety and Role", sub="ASIS Score by Stroker/Strokee", 
      xlab="Strokee(1) & Stroker(2)", ylab="ASIS Score")

#########################################


boxplot(PANASpreOM_ClosenessToOthers~interaction(Role, lex.order=T), col=c('mistyrose','powderblue'),  medcol=c("#FFDB00FF", "#B6FF00FF"), 
        whiskcol=c("#49FF00FF", "#00FF24FF"), 
        staplecol=c("#00FF92FF", "#00FFFFFF"), 
        boxcol=c("#0092FFFF", "#0024FFFF"), 
        outcol=c("#4900FFFF", "#B600FFFF"), 
        outbg=c("#FF00DB66", "#FF006D66"), NoNANs)

title(main="Pre OM Closeness and Role", sub="Closeness Score by Stroker/Strokee", 
      xlab="Strokee(1) & Stroker(2)", ylab="Closeness Pre OM")

#########################################


boxplot(PANASpostOM_ClosenessToOthers~interaction(Role, lex.order=T), col=c('mistyrose','powderblue'),  medcol=c("#FFDB00FF", "#B6FF00FF"), 
        whiskcol=c("#49FF00FF", "#00FF24FF"), 
        staplecol=c("#00FF92FF", "#00FFFFFF"), 
        boxcol=c("#0092FFFF", "#0024FFFF"), 
        outcol=c("#4900FFFF", "#B600FFFF"), 
        outbg=c("#FF00DB66", "#FF006D66"), NoNANs)

title(main="Post OM Closeness and Role", sub="Closeness Score by Stroker/Strokee", 
      xlab="Strokee(1) & Stroker(2)", ylab="Closeness Post OM")

#########################################


boxplot(PANASpreOM_ClosenessToOthers~interaction(relationship, lex.order=T), col=c('darkslategray1','lightslateblue'),  medcol=c("#FFDB00FF", "#B6FF00FF"), 
        whiskcol=c("#49FF00FF", "#00FF24FF"), 
        staplecol=c("#00FF92FF", "#00FFFFFF"), 
        boxcol=c("#0092FFFF", "#0024FFFF"), 
        outcol=c("#4900FFFF", "#B600FFFF"), 
        outbg=c("#FF00DB66", "#FF006D66"), xaxt='n', NoNANs)

title(main="Pre OM Closeness and Relationship Status", sub="Closeness Score by Stroker/Strokee", 
      xlab="Partnered(1) & Single(2)", ylab="Closeness Pre OM")

axis(1,at=c(1,2),labels=c("Strokee", "Stroker"))


#########################################

