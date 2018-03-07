##### LIBRARIES #############################

library(dplyr)
library(pastecs)
library(doBy)
library(ggplot2)
library(car)
recode <- car::recode


###################  READ CSV ###############################################################################
#Mac
#data<-read.csv(file="//Users//heathercohen//Documents//Questionnaires11022017updated//Partnered_meditation-Table 1.csv", header = TRUE, sep = ",") 
#Windows
data<-read.csv(file="C:\\Users\\SPAN\\Desktop\\Questionnaires11022017updated.csv", header = TRUE, sep = ",")

##### RENAME #####################################################################################################

rename_me<-c("Subject.number","StartDate","What.is.your.age","education","gender","Indian","Asian","Islander","Black","White","employment","income","mental.health","current.relationship.status","with.your.romantic.partner","best.describes.your.current.relationship.with.your.OM.partner.today.","current.relationship.to.OM.","relationship.with.the.person.you.are.doing.OM","vibrator","sex.films","next.two.questions","AFTER.or.when.you.were.16","have.a.physical.orgasm","Polyamorous","..Open","..Swingers","..Traditional","Monogamish")
rename_me_to<-c("ID","StartDate","age","education","gender","Indian","Asian","Islander","Black","White","employment","income","mental.health","relationship","Overlap.image.primary","Overlap.image.OM.before.OM","OMstatus","OMtodayPsStatus","MinutesVibrator","MinutesPorn","ChildhoodSexualAssault","AdultSexualAssault","PhysicalClimax","Polyamorous","Open","Swingers","Traditional","Monogamish")
names(data)[291]<-"Overlap.image.OM.after.OM"
for (i in 1:length(rename_me)){
  if (length(rename_me)!=length(rename_me_to)){print("Check that string lengths are equal")}
  names(data)[grep(rename_me[i],names(data),value=F)]<-rename_me_to[i]
}

################################################################################################################################
##### SCORING ######################################################################################################
################################################################################################

################################ QIDS ################################################################
# Published: range 0 to 27, remission <=6
QIDSstart<-agrep("Quick.Inventory.of.Depressive.Symptomatology",names(data))
for (i in 1:15){
  names(data)[(QIDSstart+i)]<-paste0("QIDS",names(data[(QIDSstart+i)]))
}
data$QIDS<-rowSums(data[QIDSstart:(QIDSstart+15)],na.rm = TRUE,dims=1)

################################ Anxiety Sensitivity Index (ASI) #####################################
#remove long title and subset
ASIdf <- data[,grepl("Anxiety.Sensitivity.Index.The.purpose.of.this.questionnaire.is.to.measure.your.level.of.fear.of.a",names(data))]
names(ASIdf) <- substring(names(ASIdf), 102)
colnames(ASIdf) <- paste("ASI", colnames(ASIdf), sep = "_")
#Scoring Requires A Minus One To All Cells (data comes in 1-5, needs to be 0-4)
ASIdf<-ASIdf-1
#Total out of 72 
ASIdf$Score<-rowSums(ASIdf,na.rm = TRUE,dims=1)
#Add back to main dataframe
data$ASIScore<-ASIdf$Score



################################ Attachment Style Questionnaire (ASQ) #####################################
#remove long title and subset
ASQdf <- data[,grepl("Attachment.Style.Questionnaire.Experiences.in.Close.Relationships..R....The.statements.below.conc....",names(data))]
names(ASQdf) <- substring(names(ASQdf), 102)
colnames(ASQdf) <- paste("ASQ", colnames(ASQdf), sep = "_")
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
#remove long title and subset
ARMCdf <- data[,grepl("Please.describe.to.what.extent.you.believe.that.these.describe.your.personality..",names(data))]
names(ARMCdf) <- substring(names(ARMCdf), 81)
colnames(ARMCdf) <- paste("ARMC", colnames(ARMCdf), sep = "_")
#Sum Score
ARMCdf$ARMCScore <-rowSums(ARMCdf,na.rm = TRUE,dims=1)

#Add back to main dataframe
data$ARMCScore<-ARMCdf$ARMCScore



################################ Behavioral Inhibition / Behavioral Approach Scale (BisBas) #####################################

BisBasdf <- data[,grepl("Behavioral.Inhibition.Behavioral.Approach.Scale.Each.item.of.this.questionnaire.is.a.statement.th....",names(data))]
names(BisBasdf) <- substring(names(BisBasdf), 101)
colnames(BisBasdf) <- paste("BisBas", colnames(BisBasdf), sep = "_")
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
NAQdf <- data[,grepl("Need.for.Affect.Please.read.each.item.carefully.and.indicate.how.strongly.you.agree.or.disagree.w....",names(data))]
names(NAQdf) <- substring(names(NAQdf), 102)
colnames(NAQdf) <- paste("NAQ", colnames(NAQdf), sep = "_")
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
NISSdf <- data[,grepl("Need.for.Sensation.Seeking.Below.is.a.set.of.statements.regarding.attitudes.and.behavior..Read.ea....",names(data))]
names(NISSdf) <- substring(names(NISSdf), 101)
colnames(NISSdf) <- paste("NISS", colnames(NISSdf), sep = "_")
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
FSSdf <- data[,grepl("After.meditation.practice.Flow.State.Scale.Please.answer.the.following.questions.in.relation.to.y....",names(data))]
names(FSSdf) <- substring(names(FSSdf), 102)
colnames(FSSdf) <- paste("FSS", colnames(FSSdf), sep = "_")

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











