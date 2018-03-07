#Just Pretty Charts and Graphs
#Presumes having run ScoringAndCleanestData 

library(devtools)
library(easyGgplot2)

################################## Creaing Subset to plot without NA Issues###########################################



NoNANs<-subset(data, select = c("ACESscore", "Role","relationship","QIDS", "ASIScore", "AttachmentAnxietyScore", "AttachmentAvoidScore", "PANASpostOM_ClosenessToOthers", "PANASpreOM_ClosenessToOthers"))
NoNANs<-na.omit(NoNANs)


NoNANs$Role<-ifelse(NoNANs$Role==1,"Strokee",
                       ifelse(NoNANs$Role==2,"Stroker",99))

NoNANs$relationship<-ifelse(NoNANs$relationship==1,"Partnered",
                    ifelse(NoNANs$relationship==2,"Single",99))
                             

################################## Pre OM Closeness and Relationship Type ##########################################


boxplot(PANASpreOM_ClosenessToOthers~interaction(relationship, lex.order=T), col=c('darkslategray1','lightslateblue'),  medcol=c("#FFDB00FF", "#B6FF00FF"), 
        whiskcol=c("#49FF00FF", "#00FF24FF"), 
        staplecol=c("#00FF92FF", "#00FFFFFF"), 
        boxcol=c("#0092FFFF", "#0024FFFF"), 
        outcol=c("#4900FFFF", "#B600FFFF"), 
        outbg=c("#FF00DB66", "#FF006D66"), xaxt='n', NoNANs)

title(main="Pre OM Closeness and Relationship Status", 
      xlab="Relationship Status", ylab="Closeness Pre OM")

axis(1,at=c(1,2),labels=c("Partnered", "Single"))


#########################################


################################## Post OM  Closeness  and relationship type ##########################################


boxplot(PANASpostOM_ClosenessToOthers~interaction(relationship, lex.order=T), col=c('darkslategray1','lightslateblue'),  medcol=c("#FFDB00FF", "#B6FF00FF"), 
        whiskcol=c("#49FF00FF", "#00FF24FF"), 
        staplecol=c("#00FF92FF", "#00FFFFFF"), 
        boxcol=c("#0092FFFF", "#0024FFFF"), 
        outcol=c("#4900FFFF", "#B600FFFF"), 
        outbg=c("#FF00DB66", "#FF006D66"), xaxt='n', NoNANs)

title(main="Post OM Closeness and Relationship Status", 
      xlab="Relationship Status", ylab="Closeness Post OM")

axis(1,at=c(1,2),labels=c("Partnered", "Single"))


######################################### Adverse Childhood and Depression by ROLE ######################################### 


ad<-ggplot(NoNANs, aes(x = ACESscore, y = QIDS, colour = Role)) + geom_point() + geom_smooth(method="loess", se=F) 

print(ad + ggtitle("Adversity in Childhood and Depression") + labs(y="Depression", x = "Childhood Adversity"))



######################################### Adverse Childhood and Depression by Relationship Type ######################################### 


ad<-ggplot(NoNANs, aes(x = ACESscore, y = QIDS, colour = relationship), col=c('darkslategray1','lightslateblue')) + geom_point() + geom_smooth(method="loess", se=F) 

print(ad + ggtitle("Adversity in Childhood and Depression") + labs(y="Depression", x = "Childhood Adversity"))



