#This program is for the Sequence Analysis (SA) of the PTC data for
#my dissertation. It will consist of 4 sections: (1) Getting the 
#data into R, (2) formatting the enrollment status variables (ftpt) 
#as sequence data, (3) running descriptives, and (4) running optimal
#matching on the distance matrix to produce a set of clusters within
#the population. 

#Section 1 Getting the data in
library(foreign)
library(TraMineR)
setwd("/Volumes/untitled/")

#install.packages("doBy")
#install.packages("tables")
#install.packages("psych")
#install.packages("stargazer")
#install.packages("estout")
#install.packages("xtable")
#install.packages("RColorBrewer")
#install.packages("mlogit")
#install.packages("MNP")
library("RColorBrewer")
library("stargazer")
library("estout")
library(xtable)
library("psych")
require(nnet)
require(ggplot2)
require(reshape2)
require(tables)
require(doBy)
require(MNP)


#install.packages("WeightedCluster")
library(WeightedCluster)
library(cluster)


# PTC<-read.dta("/Volumes/untitled/20131203_PTC.dta", convert.factors=FALSE, 
#               convert.underscore=TRUE)
# table(PTC$group)
# 
# PTC.BA1<-  subset(PTC,group==3)
# PTC.BA2<-  subset(PTC,group==4)
# PTC.AA1<-  subset(PTC,group==1)
# PTC.AA2<-  subset(PTC,group==2)
# PTC.CERT<-  subset(PTC,group==5)



# 
# #stargazer(PTC$degree.pursued.level.code)
# table(PTC$degree.pursued.level.code)
# table(PTC.AA$degree.pursued.level.code) #87166
# table(PTC.BA$degree.pursued.level.code) #39006
# table(PTC.CERT$degree.pursued.level.code) #972
# RaceTab<-table(PTC$ethnicity.imputed.code)
# colnames(RaceTab)<-c("White","Blace","Hispanic","Asian","Native American")
# xtable(RaceTab, caption="Ethnicity")
# 
# RaceTabBA<-table(PTC.BA$ethnicity.imputed.code)
# colnames(RaceTabBA)<-c("White","Blace","Hispanic","Asian","Native American")
# xtable(PTC.BA$RaceBA.f, caption="Ethnicity")
# PTC.BA$RaceBA.f<-factor(PTC.BA$ethnicity.imputed.code,labels=c("White","Black","Hispanic","Asian","Native American"))
# table(PTC.BA$RaceBA.f)
# PTC.AA$RaceAA.f<-factor(PTC.AA$ethnicity.imputed.code,labels=c("White","Black","Hispanic","Asian","Native American"))
# table(PTC.AA$RaceAA.f)

###SECTION 2, FORMATTING AS SEQUENCE DATA
#table(PTC.BA$r.ftptcode.sem1)
#table(PTC.BA$r.ftptcode.sem16)
varlist<-c("r.fptcode.sem1","r.fptcode.sem2","r.fptcode.sem3","r.fptcode.sem4","r.fptcode.sem5",
           "r.fptcode.sem6","r.fptcode.sem7","r.fptcode.sem8","r.fptcode.sem9","r.fptcode.sem10",
           "r.fptcode.sem11","r.fptcode.sem12","r.fptcode.sem13","r.fptcode.sem14","r.fptcode.sem15",
           "r.fptcode.sem16","r.fptcode.sem17","r.fptcode.sem18","r.fptcode.sem19","r.fptcode.sem20")
lablist<-c("Full-time","Part-time","Stop-out","Transfer", "Certificate",
           "Associate","Baccalaureate","Not enrolled post-grad")
codelist<-c("f","p","s","t","C","A","B","+")

seqDataBA1 <- seqdef(PTC.BA1, var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                  "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                  "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                  "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                  "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
                                ),labels=lablist)

seqDataBA2 <- seqdef(PTC.BA2, var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                    "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                    "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                    "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                    "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
                                ),labels=lablist)

seqDataAA1 <- seqdef(PTC.AA1, var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                  "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                  "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                  "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                  "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
                                  ),labels=lablist)

seqDataAA2 <- seqdef(PTC.AA2, var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                    "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                    "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                    "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                    "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
                                  ),labels=lablist)


seqDataCERT <- seqdef(PTC.CERT, var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                    "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                    "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                    "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                    "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
                                  ),labels=lablist)

lablist4<-c("Full-time, non-grad","Part-time, non-grad","Stop-out, non-grad","Transfer")
varlist4<-c("r.fptcode.sem1","r.fptcode.sem2","r.fptcode.sem3","r.fptcode.sem4")
lablist6<-c("Full-time, non-grad","Part-time, non-grad","Stop-out, non-grad","Transfer", "Full-time, grad",
           "Part-time, grad")
codelist6<-c("f","p","s","t","F","P")


seqDataBA4 <- seqdef(PTC.BA, var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4"
                              ),labels=lablist6)

seqDataAA4 <- seqdef(PTC.AA, var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4"
                                  ),labels=lablist6)

####SECTION 3, DESCRIPTIVES

BA1.PatternTable<-print(seqtab(seqDataBA1,format="SPS", tlim=1:20))
xtable(BA1.PatternTable, digits=c(0,0,2))
BA1.Xtab <- xtable(BA1.PatternTable) 
digits(BA1.Xtab)=c(0,0,2)
print(BA1.Xtab, file="BA1.PatternTable.tex", floating=FALSE,  booktabs = TRUE)
BA2.PatternTable<-print(seqtab(seqDataBA2,format="SPS", tlim=1:20))
xtable(BA2.PatternTable, digits=c(0,0,2))
BA2.Xtab <- xtable(BA2.PatternTable) 
digits(BA2.Xtab)=c(0,0,2)
print(BA2.Xtab, file="BA2.PatternTable.tex", floating=FALSE, booktabs = TRUE)

AA1.PatternTable<-print(seqtab(seqDataAA1,format="SPS", tlim=1:20))
xtable(AA1.PatternTable, digits=c(0,0,2))
AA1.Xtab <- xtable(AA1.PatternTable) 
digits(AA1.Xtab)=c(0,0,2)
print(AA1.Xtab, file="AA1.PatternTable.tex", floating=FALSE,  booktabs = TRUE)

AA2.PatternTable<-print(seqtab(seqDataAA2,format="SPS", tlim=1:20))
xtable(AA2.PatternTable, digits=c(0,0,2))
AA2.Xtab <- xtable(AA2.PatternTable) 
digits(AA2.Xtab)=c(0,0,2)
print(AA2.Xtab, file="AA2.PatternTable.tex", floating=FALSE,  booktabs = TRUE)


CERT.PatternTable<-print(seqtab(seqDataCERT,format="SPS", tlim=1:20))
xtable(CERT.PatternTable, digits=c(0,0,2))


#The following options are what make the table look like it does:
#cex.legend changes the size of the legend
#xtlab changes the labeling of the x-axis to numbers 1-20
#tlim changes the number of patterns shown from the default of 10 up to 20
#space removes the space between patterns
#yaxis changes the y-axis from just showing the cumulative % to showing each pattern's %

mtexti <- function(text, side, off = 0.25,
                   srt = if(side == 2) 90  else
                     if(side == 4) 270 else 0, ...) {
  # dimensions of plotting region in user units
  usr <- par('usr')
  # dimensions of plotting region in inches
  pin <- par('pin')
  # user units per inch
  upi <- c(usr[2]-usr[1],
           usr[4]-usr[3]) / pin
  # default x and y positions
  xpos <- (usr[1] + usr[2])/2
  ypos <- (usr[3] + usr[4])/2
  if(1 == side)
    ypos <- usr[3] - upi[2] * off
  if(2 == side)
    xpos <- usr[1] - upi[1] * off
  if(3 == side)
    ypos <- usr[4] + upi[2] * off
  if(4 == side)
    xpos <- usr[2] + upi[1] * off
  text(x=xpos, y=ypos, text, xpd=NA, srt=srt, ...)
}

#CONSTANT HEIGHT,BA1
par("mar"=c(5,4,4,8)+.1)
BA1.seq.freq <- seqtab(seqDataBA1,tlim=1:20)
#plot w/o axis
seqfplot(seqDataBA1,xtlab=1:20,tlim=20:1,cex.plot=.75,pbarw=FALSE,withlegend=FALSE,
         yaxis="pct",ylab="Sequence Percent",space=0)
#add axis
axis(4, at = seq(.5, by=1, length.out=20), 
     labels = rev(attr(BA1.seq.freq,"freq")$Freq), 
     mgp = c(1.5, 0.5, 0.0), las = 1, tick = FALSE, cex.axis=.75)
mtext(side=4, "Sequence Frequency (Total N=34,813)", line=3)
#now put the legend on        
legend("bottom", legend=attr(seqDataBA1, "labels"), 
       fill=attr(seqDataBA1, "cpal"), 
       inset=-.3, bty="o", xpd=NA, cex=.5, ncol=2)

#CONSTANT HEIGHT,BA2
par("mar"=c(5,4,4,8)+.1)
BA2.seq.freq <- seqtab(seqDataBA2,tlim=1:20)
#plot w/o axis
seqfplot(seqDataBA2,xtlab=1:20,tlim=20:1,cex.plot=.75,pbarw=FALSE,withlegend=FALSE,
         yaxis="pct",ylab="Sequence Percent",space=0)
#add axis
axis(4, at = seq(.5, by=1, length.out=20), 
     labels = rev(attr(BA2.seq.freq,"freq")$Freq), 
     mgp = c(1.5, 0.5, 0.0), las = 1, tick = FALSE, cex.axis=.75)
mtext(side=4, "Sequence Frequency (Total N=4,079)", line=3)
#now put the legend on        
legend("bottom", legend=attr(seqDataBA2, "labels"), 
       fill=attr(seqDataBA2, "cpal"), 
       inset=-.3, bty="o", xpd=NA, cex=.5, ncol=2)



#CONSTANT HEIGHT,AA1
par("mar"=c(5,4,4,8)+.1)
AA1.seq.freq <- seqtab(seqDataAA1,tlim=1:20)
#plot w/o axis
seqfplot(seqDataAA1,xtlab=1:20,tlim=20:1,cex.plot=.75,pbarw=FALSE,withlegend=FALSE,
         yaxis="pct",ylab="Sequence Percent",space=0)
#add axis
axis(4, at = seq(.5, by=1, length.out=20), 
     labels = rev(attr(AA1.seq.freq,"freq")$Freq), 
     mgp = c(1.5, 0.5, 0.0), las = 1, tick = FALSE, cex.axis=.75)
mtext(side=4, "Sequence Frequency (Total N=55,148)", line=3)
#now put the legend on        
legend("bottom", legend=attr(seqDataAA1, "labels"), 
       fill=attr(seqDataAA1, "cpal"), 
       inset=-.3, bty="o", xpd=NA, cex=.5, ncol=2)

#CONSTANT HEIGHT,AA2
par("mar"=c(5,4,4,8)+.1)
AA2.seq.freq <- seqtab(seqDataAA2,tlim=1:20)
#plot w/o axis
seqfplot(seqDataAA2,xtlab=1:20,tlim=20:1,cex.plot=.75,pbarw=FALSE,withlegend=FALSE,
         yaxis="pct",ylab="Sequence Percent",space=0)
#add axis
axis(4, at = seq(.5, by=1, length.out=20), 
     labels = rev(attr(AA2.seq.freq,"freq")$Freq), 
     mgp = c(1.5, 0.5, 0.0), las = 1, tick = FALSE, cex.axis=.75)
mtext(side=4, "Sequence Frequency (Total N=31,475)", line=3)
#now put the legend on        
legend("bottom", legend=attr(seqDataAA2, "labels"), 
       fill=attr(seqDataAA2, "cpal"), 
       inset=-.3, bty="o", xpd=NA, cex=.5, ncol=2)

seqfplot(seqDataAA2,cex.legend=.55,xtlab=1:20,tlim=20:1,space=0,pbarw=FALSE,cex.plot=.75,withlegend="right",
         yaxis="pct",ylab="% freq.(n=31,524)",use.layout=TRUE,legend.prop=.21)



par("mar"=c(15,4,1,20)+.1)
seqfplot(seqDataAA1,xtlab=1:20,tlim=20:1,cex.plot=.5,pbarw=FALSE,withlegend=FALSE,
         yaxis="pct",ylab="Sequence Percent",space=0)

legend("right", legend=attr(seqDataAA1, "labels"), 
       fill=attr(seqDataAA1, "cpal"), 
       inset=-1, bty="o", xpd=NA, cex=1, ncol=1)





par("mar"=c(5,4,4,1)+.1)
#BA1 distribution of states by time period
seqdplot(seqDataBA1,ylab = "Freq. (n=34,813)" ,cex.legend=.55, xtlab=1:20,withlegend="right",cex.plot=.70,use.layout=TRUE,legend.prop=.21)
seqstatd(seqDataBA1)
#BA2 distribution of states by time period
seqdplot(seqDataBA2,ylab = "Freq. (n=4,079)" ,cex.legend=.55, xtlab=1:20,withlegend="right",cex.plot=.70,use.layout=TRUE,legend.prop=.21)
seqstatd(seqDataBA2)
#AA1 distribution of states by time period
seqdplot(seqDataAA1,ylab = "Freq. (n=55,148)" ,cex.legend=.55, xtlab=1:20,withlegend="right",cex.plot=.70,use.layout=TRUE,legend.prop=.21)
seqstatd(seqDataAA1)
#AA2 distribution of states by time period
seqdplot(seqDataAA2,ylab = "Freq. (n=31,475)" ,cex.legend=.55, xtlab=1:20,withlegend="right",cex.plot=.70,use.layout=TRUE,legend.prop=.21)
seqstatd(seqDataAA2)
#CERT distribution of states by time period
seqdplot(seqDataCERT,cex.legend=.55, xtlab=1:20,withlegend="right",cex.plot=.70,use.layout=TRUE,legend.prop=.21)


#Mean Time spent in state, BA1
seqmtplot(seqDataBA1,ylab = "Mean time (n=34,813)",withlegend="right",cex.legend=.65,ylim=c(0,10))
seqmeant(seqDataBA1)
seqmeant(seqDataBA1,prop=TRUE)
#Mean Time spent in state, BA2
seqmtplot(seqDataBA2,ylab ="Mean time (n=4,079)",withlegend="right",cex.legend=.65,ylim=c(0,10))
seqmeant(seqDataBA2)
seqmeant(seqDataBA2,prop=TRUE)
#Mean Time spent in state, AA1
seqmtplot(seqDataAA1,ylab ="Mean time (n=55,148)",withlegend="right",cex.legend=.65,ylim=c(0,15))
seqmeant(seqDataAA1)
seqmeant(seqDataAA1, prop=TRUE)
#Mean Time spent in state, AA2
seqmtplot(seqDataAA2,ylab ="Mean time (n=31,475)",withlegend="right",cex.legend=.65,ylim=c(0,10))
seqmeant(seqDataAA2)
seqmeant(seqDataAA2, prop=TRUE)


BA1.meant<-print(seqmeant(seqDataBA1, prop=TRUE))
xtable(BA1.meant, digits=4)
BA2.meant<-print(seqmeant(seqDataBA2, prop=TRUE))
xtable(BA2.meant, digits=4)
AA1.meant<-print(seqmeant(seqDataAA1, prop=TRUE))
xtable(AA1.meant, digits=4)
AA2.meant<-print(seqmeant(seqDataAA2, prop=TRUE))
xtable(AA2.meant, digits=4)

#Transition between sequences rate, BA1
BA1.trate<-seqtrate(seqDataBA1)
round(BA1.trate,2)
#Transition between sequences rate, BA2
BA2.trate<-seqtrate(seqDataBA2)
xtable(round(BA2.trate,2))

#Transition between sequences rate, AA1
AA1.trate<-seqtrate(seqDataAA1)
xtable(round(AA1.trate,2))
#Transition between sequences rate, AA2
AA2.trate<-seqtrate(seqDataAA2)
xtable(round(AA2.trate,2))

#Transition between sequences rate, CERT
CERT.trate<-seqtrate(seqDataCERT)
round(CERT.trate,2)

#The following 6 lines are an attempt to put the entropy onto the same plot.
#It's possible, but I can't get it to give them different colors and it overlays
#the weighted n. 
#EntBA<-seqstatd(PTCBA.seq, weighted=TRUE, with.missing=FALSE, norm=TRUE)
#EntAA<-seqstatd(PTCAA.seq, weighted=TRUE, with.missing=FALSE, norm=TRUE)
#plot(EntBA,type="Ht")
#par(new=TRUE)
#plot(EntAA,type="Ht" )
#seqHtplot(seqData2,group=degree.pursued.level.code,xtlab=1:20)

#Transversal Entropy plot, BA1
seqHtplot(seqDataBA1,xtlab=1:20)
#Transversal Entropy plot, BA2
seqHtplot(seqDataBA2,xtlab=1:20)

#Transversal Entropy plot, AA1
seqHtplot(seqDataAA1,xtlab=1:20)
#Transversal Entropy plot, AA2
seqHtplot(seqDataAA2,xtlab=1:20)

#Transversal Entropy plot, CERT
seqHtplot(seqDataCERT,xtlab=1:20)

# #Sequence of Modal States, BA
# seqmsplot(seqDataBA,withlegend="right",cex.legend=.65,xtlab=1:20,cex.plot=.65)
# #Sequence of Modal States, AA
# seqmsplot(seqDataAA,withlegend="right",cex.legend=.65,xtlab=1:20,cex.plot=.65)

#by(seqData2,seqData2$degree.pursued.level.code,seqdplot(seqData2,cex.legend=.80))

#seqData.BA<- 

scostBA1<-seqsubm(seqDataBA1,method = "TRATE")
SubMatrixBA1<-round(scostBA1, 3)
xtable(SubMatrixBA1)
scostBA2<-seqsubm(seqDataBA2,method = "TRATE")
SubMatrixBA2<-round(scostBA2, 3)
xtable(SubMatrixBA2)

scostAA1<-seqsubm(seqDataAA1,method = "TRATE")
SubMatrixAA1<-round(scostAA1, 3)
xtable(SubMatrixAA1)
scostAA2<-seqsubm(seqDataAA2,method = "TRATE")
SubMatrixAA2<-round(scostAA2, 3)
xtable(SubMatrixAA2)

scostCERT<-seqsubm(seqDataCERT,method = "TRATE")
SubMatrixCERT<-round(scostCERT, 3)

#RUN THESE LATER ON ALL SUBMATRICES
xtable(SubMatrixBA)
xtable(SubMatrixAA)


#Creating the weighted data set, BA1
aggPTCBA1 <- wcAggregateCases(PTC.BA1[,1:20])
print(aggPTCBA1)
uniquePTCBA1<-PTC.BA1[aggPTCBA1$aggIndex,1:20]
PTCBA1.seq<-seqdef(uniquePTCBA1,weights = aggPTCBA1$aggWeights,labels=lablist)

PTCBA1.om <- seqdist(PTCBA1.seq, method = "OM", indel = 1, sm = scostBA1,full.matrix=FALSE)

WardClustBA1 <- hclust(as.dist(PTCBA1.om), method = "ward", members = aggPTCBA1$aggWeights)

WardClustQualBA1 <- as.clustrange(WardClustBA1, PTCBA1.om, weights = aggPTCBA1$aggWeights, 
                                    ncluster = 10)
BA1ClustQual.10<-summary(WardClustQualBA1, max.rank = 2)
xtable(BA1ClustQual.10)

WardClustQualBA1.20 <- as.clustrange(WardClustBA1, PTCBA1.om, weights = aggPTCBA1$aggWeights, 
                                   ncluster = 20)
BA1ClustQual.20<-summary(WardClustQualBA1.20, max.rank = 2)
xtable(BA1ClustQual.20)


WardTreeBA1.20 <- as.seqtree(WardClustBA1, seqdata = PTCBA1.seq, diss = PTCBA1.om, ncluster = 20)
seqtreedisplay(WardTreeBA1.20, type = "f",pbarw=FALSE, tlim=20:1, showdepth = TRUE,legend.fontsize=2.5)
seqtreedisplay(WardTreeBA1.20, type = "d",pbarw=FALSE,  showdepth = TRUE,legend.fontsize=2.5)


#Trying an alternate distance and cluster algorithm to see if similar results occur
PTCBA1.ham <- seqdist(PTCBA1.seq, method = "HAM", indel = 1, sm = scostBA1,full.matrix=FALSE)

CentroidClustBA1 <- hclust(as.dist(PTCBA1.ham), method = "centroid", members = aggPTCBA1$aggWeights)
CentroidClustQualBA1.20 <- as.clustrange(CentroidClustBA1, PTCBA1.ham, weights = aggPTCBA1$aggWeights, 
                                     ncluster = 20)
BA1ClustQual.Centroid.20<-summary(CentroidClustQualBA1.20, max.rank = 2)
xtable(BA1ClustQual.Centroid.20)
CentroidTreeBA1.20 <- as.seqtree(WardClustBA1, seqdata = PTCBA1.seq, diss = PTCBA1.om, ncluster = 20)
seqtreedisplay(CentroidTreeBA1.20, type = "f",pbarw=FALSE, tlim=20:1, showdepth = TRUE,legend.fontsize=2.5)
seqtreedisplay(CentroidTreeBA1.20, type = "d",pbarw=FALSE,  showdepth = TRUE,legend.fontsize=2.5)


#Creating the weighted data set, BA2
aggPTCBA2 <- wcAggregateCases(PTC.BA2[,1:20])
print(aggPTCBA2)
uniquePTCBA2<-PTC.BA2[aggPTCBA2$aggIndex,1:20]
PTCBA2.seq<-seqdef(uniquePTCBA2,weights = aggPTCBA2$aggWeights,labels=lablist)

PTCBA2.om <- seqdist(PTCBA2.seq, method = "OM", indel = 1, sm = scostBA2,full.matrix=FALSE)

WardClustBA2 <- hclust(as.dist(PTCBA2.om), method = "ward", members = aggPTCBA2$aggWeights)

WardClustQualBA2 <- as.clustrange(WardClustBA2, PTCBA2.om, weights = aggPTCBA2$aggWeights, 
                                  ncluster = 10)
BA2ClustQual.10<-summary(WardClustQualBA2, max.rank = 2)
xtable(BA2ClustQual.10)

WardClustQualBA2.20 <- as.clustrange(WardClustBA2, PTCBA2.om, weights = aggPTCBA2$aggWeights, 
                                     ncluster = 20)
BA2ClustQual.20<-summary(WardClustQualBA2.20, max.rank = 2)
xtable(BA2ClustQual.20)


WardTreeBA2.20 <- as.seqtree(WardClustBA2, seqdata = PTCBA2.seq, diss = PTCBA2.om, ncluster = 20)
seqtreedisplay(WardTreeBA2.20, type = "f",pbarw=FALSE, tlim=20:1, showdepth = TRUE,legend.fontsize=2.5)
seqtreedisplay(WardTreeBA2.20, type = "d",pbarw=FALSE,  showdepth = TRUE,legend.fontsize=2.5)



#Creating the weighted data set, AA1
aggPTCAA1 <- wcAggregateCases(PTC.AA1[,1:20])
print(aggPTCAA1)
uniquePTCAA1<-PTC.AA1[aggPTCAA1$aggIndex,1:20]
PTCAA1.seq<-seqdef(uniquePTCAA1,weights = aggPTCAA1$aggWeights,labels=lablist)

PTCAA1.om <- seqdist(PTCAA1.seq, method = "OM", indel = 1, sm = scostAA1,full.matrix=FALSE)

WardClustAA1 <- hclust(as.dist(PTCAA1.om), method = "ward", members = aggPTCAA1$aggWeights)

WardClustQualAA1 <- as.clustrange(WardClustAA1, PTCAA1.om, weights = aggPTCAA1$aggWeights, 
                                  ncluster = 10)
AA1ClustQual.10<-summary(WardClustQualAA1, max.rank = 2)
xtable(AA1ClustQual.10)

WardClustQualAA1.20 <- as.clustrange(WardClustAA1, PTCAA1.om, weights = aggPTCAA1$aggWeights, 
                                     ncluster = 20)
AA1ClustQual.20<-summary(WardClustQualAA1.20, max.rank = 2)
xtable(AA1ClustQual.20)


WardTreeAA1.20 <- as.seqtree(WardClustAA1, seqdata = PTCAA1.seq, diss = PTCAA1.om, ncluster = 20)
seqtreedisplay(WardTreeAA1.20, type = "f",pbarw=FALSE, tlim=20:1, showdepth = TRUE,legend.fontsize=2.5)
seqtreedisplay(WardTreeAA1.20, type = "d",pbarw=FALSE,  showdepth = TRUE,legend.fontsize=2.5)


#Creating the weighted data set, AA2
aggPTCAA2 <- wcAggregateCases(PTC.AA2[,1:20])
print(aggPTCAA2)
uniquePTCAA2<-PTC.AA2[aggPTCAA2$aggIndex,1:20]
PTCAA2.seq<-seqdef(uniquePTCAA2,weights = aggPTCAA2$aggWeights,labels=lablist)

PTCAA2.om <- seqdist(PTCAA2.seq, method = "OM", indel = 1, sm = scostAA2,full.matrix=FALSE)

WardClustAA2 <- hclust(as.dist(PTCAA2.om), method = "ward", members = aggPTCAA2$aggWeights)

WardClustQualAA2 <- as.clustrange(WardClustAA2, PTCAA2.om, weights = aggPTCAA2$aggWeights, 
                                  ncluster = 10)
AA2ClustQual.10<-summary(WardClustQualAA2, max.rank = 2)
xtable(AA2ClustQual.10)

WardClustQualAA2.20 <- as.clustrange(WardClustAA2, PTCAA2.om, weights = aggPTCAA2$aggWeights, 
                                     ncluster = 20)
AA2ClustQual.20<-summary(WardClustQualAA2.20, max.rank = 2)
xtable(AA2ClustQual.20)


WardTreeAA2.20 <- as.seqtree(WardClustAA2, seqdata = PTCAA2.seq, diss = PTCAA2.om, ncluster = 20)
seqtreedisplay(WardTreeAA2.20, type = "f",pbarw=FALSE, tlim=20:1, showdepth = TRUE,legend.fontsize=2.5)
seqtreedisplay(WardTreeAA2.20, type = "d",pbarw=FALSE,  showdepth = TRUE,legend.fontsize=2.5)




#Creating the weighted data set, CERT
aggPTCCERT <- wcAggregateCases(PTC.CERT[,1:20])
print(aggPTCCERT)
uniquePTCCERT<-PTC.CERT[aggPTCCERT$aggIndex,1:20]
PTCCERT.seq<-seqdef(uniquePTCCERT,weights = aggPTCCERT$aggWeights,labels=lablist)

PTCCERT.om <- seqdist(PTCCERT.seq, method = "OM", indel = 1, sm = scostCERT,full.matrix=FALSE)

WardClustCERT <- hclust(as.dist(PTCCERT.om), method = "ward", members = aggPTCCERT$aggWeights)

WardClustQualCERT <- as.clustrange(WardClustCERT, PTCCERT.om, weights = aggPTCCERT$aggWeights, 
                                   ncluster = 10)
CERTClustQual.10<-summary(WardClustQualCERT, max.rank = 2)
xtable(CERTClustQual.10)

WardClustQualCERT.20 <- as.clustrange(WardClustCERT, PTCCERT.om, weights = aggPTCCERT$aggWeights, 
                                      ncluster = 20)
CERTClustQual.20<-summary(WardClustQualCERT.20, max.rank = 2)
xtable(CERTClustQual.20)


WardTreeCERT.20 <- as.seqtree(WardClustCERT, seqdata = PTCCERT.seq, diss = PTCCERT.om, ncluster = 20)
seqtreedisplay(WardTreeCERT.20, type = "f",pbarw=FALSE, tlim=20:1, showdepth = TRUE,legend.fontsize=2.5)
seqtreedisplay(WardTreeCERT.20, type = "d",pbarw=FALSE,  showdepth = TRUE,legend.fontsize=2.5)


lablist<-c("Full-time","Part-time","Stop-out","Transfer", "Certificate",
           "Associate","Baccalaureate","Not enrolled post-grad")
#cpal "#7FC97F" "#BEAED4" "#FDC086" "#FFFF99" "#386CB0" "#F0027F" "#BF5B17" "#666666"
#######################################
#Representative sequence for BA1
#######################################
uniqueCluster10BA1 <- WardClustQualBA1.20$clustering$cluster10
PTC.BA1$cluster10 <- uniqueCluster10BA1[aggPTCBA1$disaggIndex]
table(PTC.BA1$cluster10)

# 4-yrs enrl, 1-3yr gap, BA deg  ,882
PTC.BA1.10a<-  subset(PTC.BA1,cluster10==1)
PTC.BA1.10a.seq<-seqdef(PTC.BA1.10a,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                    "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                    "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                    "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                    "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist)
PTC.BA1.10a.om <- seqdist(PTC.BA1.10a.seq, method = "OM", indel = 1, sm = scostBA1,full.matrix=FALSE)

seqrplot(PTC.BA1.10a.seq,  dist.matrix=PTC.BA1.10a.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)




# 6-7year graduates  , 2178
PTC.BA1.10b<-  subset(PTC.BA1,cluster10==2)
lablist7<-c("Full-time","Part-time","Stop-out", "Certificate",
           "Associate","Baccalaureate","Not enrolled post-grad")
cpal.BA1.10b=c("#7FC97F" ,"#BEAED4", "#FDC086", "#386CB0" ,"#F0027F", "#BF5B17" ,"#666666")
PTC.BA1.10b.seq<-seqdef(PTC.BA1.10b,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist7,cpal=cpal.BA1.10b)
scostBA1.10b<-seqsubm(PTC.BA1.10b.seq,method = "TRATE")
PTC.BA1.10b.om <- seqdist(PTC.BA1.10b.seq, method = "OM", indel = 1, sm = scostBA1.10b,full.matrix=FALSE)

seqrplot(PTC.BA1.10b.seq,  dist.matrix=PTC.BA1.10b.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55,nrep=10)


# 4-6yr stop outs , 1923
PTC.BA1.10c<-  subset(PTC.BA1,cluster10==3)
lablist.BA1.10c<-c("Full-time","Part-time","Stop-out","Transfer", 
           "Associate","Baccalaureate","Not enrolled post-grad")
cpal.BA1.10c=c("#7FC97F" ,"#BEAED4", "#FDC086", "#FFFF99" ,"#F0027F", "#BF5B17" ,"#666666")

PTC.BA1.10c.seq<-seqdef(PTC.BA1.10c,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.BA1.10c,cpal=cpal.BA1.10c)
scostBA1.10c<-seqsubm(PTC.BA1.10c.seq,method = "TRATE")
PTC.BA1.10c.om <- seqdist(PTC.BA1.10c.seq, method = "OM", indel = 1, sm = scostBA1.10c,full.matrix=FALSE)

seqrplot(PTC.BA1.10c.seq,  dist.matrix=PTC.BA1.10c.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)


# Mostly PT, some graduation after 8 years, 483
PTC.BA1.10d<-  subset(PTC.BA1,cluster10==4)
lablist.BA1.10d<-c("Full-time","Part-time","Stop-out","Transfer", 
                   "Associate","Baccalaureate","Not enrolled post-grad")
cpal.BA1.10d=c("#7FC97F" ,"#BEAED4", "#FDC086", "#FFFF99" ,"#F0027F", "#BF5B17" ,"#666666")
PTC.BA1.10d.seq<-seqdef(PTC.BA1.10d,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.BA1.10d, cpal=cpal.BA1.10d)
scostBA1.10d<-seqsubm(PTC.BA1.10d.seq,method = "TRATE")
PTC.BA1.10d.om <- seqdist(PTC.BA1.10d.seq, method = "OM", indel = 1, sm = scostBA1.10d,full.matrix=FALSE)

seqrplot(PTC.BA1.10d.seq,  dist.matrix=PTC.BA1.10d.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)


# Stop out by yr 3 then a 4 year break, 570
PTC.BA1.10e<-  subset(PTC.BA1,cluster10==5)
lablist.BA1.10e<-c("Full-time","Part-time","Stop-out","Transfer")
cpal.BA1.10e=c("#7FC97F" ,"#BEAED4", "#FDC086", "#FFFF99")

PTC.BA1.10e.seq<-seqdef(PTC.BA1.10e,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.BA1.10e, cpal=cpal.BA1.10e)
scostBA1.10e<-seqsubm(PTC.BA1.10e.seq,method = "TRATE")
PTC.BA1.10e.om <- seqdist(PTC.BA1.10e.seq, method = "OM", indel = 1, sm = scostBA1.10e,full.matrix=FALSE)

seqrplot(PTC.BA1.10e.seq,  dist.matrix=PTC.BA1.10e.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)

# On-time graduates, 16288
PTC.BA1.10f<-  subset(PTC.BA1,cluster10==6)
lablist.BA.10f<-c("Full-time","Part-time","Stop-out", "Certificate",
            "Associate","Baccalaureate","Not enrolled post-grad")
cpal.BA1.10f=c("#7FC97F" ,"#BEAED4", "#FDC086", "#386CB0" ,"#F0027F", "#BF5B17" ,"#666666")

PTC.BA1.10f.seq<-seqdef(PTC.BA1.10f,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.BA.10f, cpal=cpal.BA1.10f)
scostBA1.10f<-seqsubm(PTC.BA1.10f.seq,method = "TRATE")
PTC.BA1.10f.om <- seqdist(PTC.BA1.10f.seq, method = "OM", indel = 1, sm = scostBA1.10f,full.matrix=FALSE)

seqrplot(PTC.BA1.10f.seq,  dist.matrix=PTC.BA1.10f.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)

seqtab(PTC.BA1.10f.seq,format="SPS", tlim=1:20)

# Transfer by year 4, 3715
PTC.BA1.10g<-  subset(PTC.BA1,cluster10==7)
lablist.BA1.10g<-c("Full-time","Part-time","Stop-out","Transfer")
cpal.BA1.10g=c("#7FC97F" ,"#BEAED4", "#FDC086", "#FFFF99")

PTC.BA1.10g.seq<-seqdef(PTC.BA1.10g,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.BA1.10g, cpal=cpal.BA1.10g)
scostBA1.10g<-seqsubm(PTC.BA1.10g.seq,method = "TRATE")
PTC.BA1.10g.om <- seqdist(PTC.BA1.10g.seq, method = "OM", indel = 1, sm = scostBA1.10g,full.matrix=FALSE)

seqrplot(PTC.BA1.10g.seq,  dist.matrix=PTC.BA1.10g.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)


# Lot of pt, grad 6-8yrs, 804
PTC.BA1.10h<-  subset(PTC.BA1,cluster10==8)

lablist.BA1.10g<-c("Full-time","Part-time","Stop-out",
           "Associate","Baccalaureate","Not enrolled post-grad")
cpal.BA1.10h=c("#7FC97F", "#BEAED4" ,"#FDC086" , "#F0027F", "#BF5B17" ,"#666666")

PTC.BA1.10h.seq<-seqdef(PTC.BA1.10h,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.BA1.10g,cpal=cpal.BA1.10h)
scostBA1.10h<-seqsubm(PTC.BA1.10h.seq,method = "TRATE")
PTC.BA1.10h.om <- seqdist(PTC.BA1.10h.seq, method = "OM", indel = 1, sm = scostBA1.10h,full.matrix=FALSE)

seqrplot(PTC.BA1.10h.seq,  dist.matrix=PTC.BA1.10h.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)


# Stop out by yr 2, long break, then full-time to grad by yr 7, 8 or 10, 515
PTC.BA1.10i<-  subset(PTC.BA1,cluster10==9)
PTC.BA1.10i.seq<-seqdef(PTC.BA1.10i,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist)
PTC.BA1.10i.om <- seqdist(PTC.BA1.10i.seq, method = "OM", indel = 1, sm = scostBA1,full.matrix=FALSE)

seqrplot(PTC.BA1.10i.seq,  dist.matrix=PTC.BA1.10i.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)



# Stop outs, 750231
PTC.BA1.10j<-  subset(PTC.BA1,cluster10==10)
PTC.BA1.10j.seq<-seqdef(PTC.BA1.10j,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist)
PTC.BA1.10j.om <- seqdist(PTC.BA1.10j.seq, method = "OM", indel = 1, sm = scostBA1,full.matrix=FALSE)

seqrplot(PTC.BA1.10j.seq,  dist.matrix=PTC.BA1.10j.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)





lablist<-c("Full-time","Part-time","Stop-out","Transfer", "Certificate",
           "Associate","Baccalaureate","Not enrolled post-grad")
#cpal "#7FC97F" "#BEAED4" "#FDC086" "#FFFF99" "#386CB0" "#F0027F" "#BF5B17" "#666666"
#######################################
#Representative sequence for BA2
#######################################
uniqueCluster10BA2 <- WardClustQualBA2.20$clustering$cluster8
PTC.BA2$cluster8 <- uniqueCluster10BA2[aggPTCBA2$disaggIndex]
table(PTC.BA2$cluster8 )

# AA in 9-11sems ,317
PTC.BA2.8a<-  subset(PTC.BA2,cluster8==1)
lablist.BA2.8a<-c("Full-time","Part-time","Stop-out", "Certificate",
           "Associate","Baccalaureate","Not enrolled post-grad")
cpal.BA2.8a=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#386CB0" ,"#F0027F", "#BF5B17", "#666666")
PTC.BA2.8a.seq<-seqdef(PTC.BA2.8a,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.BA2.8a,cpal=cpal.BA2.8a)
scostBA2.8a<-seqsubm(PTC.BA2.8a.seq,method = "TRATE")
PTC.BA2.8a.om <- seqdist(PTC.BA2.8a.seq, method = "OM", indel = 1, sm = scostBA2.8a,full.matrix=FALSE)

seqrplot(PTC.BA2.8a.seq,  dist.matrix=PTC.BA2.8a.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)


#2 All over the place Porposing?, 565
PTC.BA2.8b<-  subset(PTC.BA2,cluster8==2)
PTC.BA2.8b.seq<-seqdef(PTC.BA2.8b,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                        "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                        "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                        "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                        "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist)
PTC.BA2.8b.om <- seqdist(PTC.BA2.8b.seq, method = "OM", indel = 1, sm = scostBA1,full.matrix=FALSE)

seqrplot(PTC.BA2.8b.seq,  dist.matrix=PTC.BA2.8b.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)


#3 Associate in 3-5yrs , 398
PTC.BA2.8c<-  subset(PTC.BA2,cluster8==3)
lablist.BA2.8c<-c("Full-time","Part-time","Stop-out", "Certificate",
                  "Associate","Baccalaureate","Not enrolled post-grad")
cpal.BA2.8c=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#386CB0" ,"#F0027F", "#BF5B17", "#666666")

PTC.BA2.8c.seq<-seqdef(PTC.BA2.8c,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                        "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                        "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                        "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                        "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.BA2.8c, cpal=cpal.BA2.8c)
scostBA2.8c<-seqsubm(PTC.BA2.8c.seq,method = "TRATE")

PTC.BA2.8c.om <- seqdist(PTC.BA2.8c.seq, method = "OM", indel = 1, sm = scostBA2.8c,full.matrix=FALSE)

seqrplot(PTC.BA2.8c.seq,  dist.matrix=PTC.BA2.8c.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)


#4 Stop out within 4 years, 563
PTC.BA2.8d<-  subset(PTC.BA2,cluster8==4)

lablist.BA2.8d<-c("Full-time","Part-time","Stop-out","Transfer", 
           "Associate","Not enrolled post-grad")
cpal.BA2.8d=c("#7FC97F", "#BEAED4", "#FDC086" ,"#FFFF99", "#F0027F", "#666666")

PTC.BA2.8d.seq<-seqdef(PTC.BA2.8d,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                        "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                        "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                        "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                        "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.BA2.8d,cpal=cpal.BA2.8d)
scostBA2.8d<-seqsubm(PTC.BA2.8d.seq,method = "TRATE")

PTC.BA2.8d.om <- seqdist(PTC.BA2.8d.seq, method = "OM", indel = 1, sm = scostBA2.8d,full.matrix=FALSE)

seqrplot(PTC.BA2.8d.seq,  dist.matrix=PTC.BA2.8d.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)

#5 Transfer by year 3 with 1-3 sem break (729)

PTC.BA2.8e<-  subset(PTC.BA2,cluster8==5)

lablist.BA2.8e<-c("Full-time","Part-time","Stop-out","Transfer")
cpal.BA2.8e=c("#7FC97F", "#BEAED4", "#FDC086" ,"#FFFF99")

PTC.BA2.8e.seq<-seqdef(PTC.BA2.8e,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                        "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                        "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                        "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                        "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.BA2.8e,cpal=cpal.BA2.8e)
scostBA2.8e<-seqsubm(PTC.BA2.8e.seq,method = "TRATE")

PTC.BA2.8e.om <- seqdist(PTC.BA2.8e.seq, method = "OM", indel = 1, sm = scostBA2.8e,full.matrix=FALSE)

seqrplot(PTC.BA2.8e.seq,  dist.matrix=PTC.BA2.8e.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)


#6 Those who get an associates degree mostly within 6-9 years (158)
PTC.BA2.8f<-  subset(PTC.BA2,cluster8==6)

lablist.BA2.8f<-c("Full-time","Part-time","Stop-out", "Certificate",
                  "Associate","Baccalaureate","Not enrolled post-grad")
cpal.BA2.8f=c("#7FC97F", "#BEAED4", "#FDC086" ,"#386CB0" ,"#F0027F", "#BF5B17", "#666666")

PTC.BA2.8f.seq<-seqdef(PTC.BA2.8f,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                        "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                        "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                        "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                        "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.BA2.8f,cpal=cpal.BA2.8f)
scostBA2.8f<-seqsubm(PTC.BA2.8f.seq,method = "TRATE")

PTC.BA2.8f.om <- seqdist(PTC.BA2.8f.seq, method = "OM", indel = 1, sm = scostBA2.8f,full.matrix=FALSE)

seqrplot(PTC.BA2.8f.seq,  dist.matrix=PTC.BA2.8f.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)

#7 Those who transfer after a 4-8 year break (304)
PTC.BA2.8g<-  subset(PTC.BA2,cluster8==7)

lablist.BA2.8g<-c("Full-time","Part-time","Stop-out", "Transfer")
cpal.BA2.8g=c("#7FC97F", "#BEAED4", "#FDC086" ,"#FFFF99")

PTC.BA2.8g.seq<-seqdef(PTC.BA2.8g,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                        "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                        "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                        "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                        "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.BA2.8g,cpal=cpal.BA2.8g)
scostBA2.8g<-seqsubm(PTC.BA2.8g.seq,method = "TRATE")

PTC.BA2.8g.om <- seqdist(PTC.BA2.8g.seq, method = "OM", indel = 1, sm = scostBA2.8g,full.matrix=FALSE)

seqrplot(PTC.BA2.8g.seq,  dist.matrix=PTC.BA2.8g.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)


#8 Those who stop out after 1-2 years of relatively continous enrollment (1051)
PTC.BA2.8h<-  subset(PTC.BA2,cluster8==8)

lablist.BA2.8h<-c("Full-time","Part-time","Stop-out", "Transfer","Associate","Not enrolled post-grad")
cpal.BA2.8h=c("#7FC97F", "#BEAED4", "#FDC086" ,"#FFFF99", "#F0027F" , "#666666")

PTC.BA2.8h.seq<-seqdef(PTC.BA2.8h,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                        "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                        "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                        "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                        "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.BA2.8h,cpal=cpal.BA2.8h)
scostBA2.8h<-seqsubm(PTC.BA2.8h.seq,method = "TRATE")

PTC.BA2.8h.om <- seqdist(PTC.BA2.8h.seq, method = "OM", indel = 1, sm = scostBA2.8h,full.matrix=FALSE)

seqrplot(PTC.BA2.8h.seq,  dist.matrix=PTC.BA2.8h.om, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55, nrep=10)


#Line plot of cluster quality indices
plot(WardClustQualAA, norm = "zscore")







########################################
#Representative Sequences for AA1
#######################################


uniqueCluster12AA1 <- WardClustQualAA1.20$clustering$cluster12
PTC.AA1$cluster12 <- uniqueCluster12AA1[aggPTCAA1$disaggIndex]
table(PTC.AA1$ethnicity.imputed.code, PTC.AA1$cluster12)
table(PTC.AA1$cluster12)

# BArep12<-seqrep(PTCBA.seq, group=WCBA8c.f,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTCBA.om)
# print(BArep12)

#1 A lot of partime, mostly stopout by yr 7 with 1 7yr AA grad pattern,810 
PTC.AA1.12a<-  subset(PTC.AA1,cluster12==1)
PTC.AA1.12a.seq<-seqdef(PTC.AA1.12a,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                    "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                    "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                    "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                    "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
                                    ),labels=lablist)
PTCAA1.12a.om <- seqdist(PTC.AA1.12a.seq, method = "OM", indel = 1, sm = scostAA1,full.matrix=FALSE)
AA1rep12a<-seqrep(PTC.AA1.12a.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTCAA1.12a.om)
print(AA1rep12a)

seqrplot(PTC.AA1.12a.seq,  dist.matrix=PTCAA1.12a.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)







#2 AA Grad in 5.5-7 years with a 1-3 year break in the middle, 1001
lablist.AA1.12b<-c("Full-time","Part-time","Stop-out", "Certificate",
                   "Associate","Baccalaureate","Not enrolled post-grad")
cpal.AA1.12b=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#386CB0" ,"#F0027F", "#BF5B17", "#666666")

PTC.AA1.12b<-  subset(PTC.AA1,cluster12==2)
PTC.AA1.12b.seq<-seqdef(PTC.AA1.12b,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA1.12b, cpal=cpal.AA1.12b)

scostAA1.12b<-seqsubm(PTC.AA1.12b.seq,method = "TRATE")

PTCAA1.12b.om <- seqdist(PTC.AA1.12b.seq, method = "OM", indel = 1, sm = scostAA1.12b,full.matrix=FALSE)
AA1rep12b<-seqrep(PTC.AA1.12b.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTCAA1.12b.om)
print(AA1rep12b)

seqrplot(PTC.AA1.12b.seq,  dist.matrix=PTCAA1.12b.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)



#2 AA Grad in 5.5-7 years with a 1-3 year break in the middle, 1001
lablist.AA1.12b<-c("Full-time","Part-time","Stop-out", "Certificate",
                   "Associate","Baccalaureate","Not enrolled post-grad")
cpal.AA1.12b=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#386CB0" ,"#F0027F", "#BF5B17", "#666666")

PTC.AA1.12b<-  subset(PTC.AA1,cluster12==2)
PTC.AA1.12b.seq<-seqdef(PTC.AA1.12b,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA1.12b, cpal=cpal.AA1.12b)

scostAA1.12b<-seqsubm(PTC.AA1.12b.seq,method = "TRATE")

PTCAA1.12b.om <- seqdist(PTC.AA1.12b.seq, method = "OM", indel = 1, sm = scostAA1.12b,full.matrix=FALSE)
AA1rep12b<-seqrep(PTC.AA1.12b.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTCAA1.12b.om)
print(AA1rep12b)

seqrplot(PTC.AA1.12b.seq,  dist.matrix=PTCAA1.12b.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)




#3 Stop out around year 5, 1814
lablist.AA1.12c<-c("Full-time","Part-time","Stop-out", "Transfer" ,
                   "Associate","Not enrolled post-grad")
cpal.AA1.12c=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#FFFF99" ,"#F0027F", "#666666")
PTC.AA1.12c<-  subset(PTC.AA1,cluster12==3)
PTC.AA1.12c.seq<-seqdef(PTC.AA1.12c,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA1.12c, cpal=cpal.AA1.12c)
scostAA1.12c<-seqsubm(PTC.AA1.12c.seq,method = "TRATE")

PTCAA1.12c.om <- seqdist(PTC.AA1.12c.seq, method = "OM", indel = 1, sm = scostAA1.12c,full.matrix=FALSE)
AA1rep12c<-seqrep(PTC.AA1.12c.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTCAA1.12c.om)
print(AA1rep12c)

seqrplot(PTC.AA1.12c.seq,  dist.matrix=PTCAA1.12c.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)



#4 Up to 2yrs at CUNY then a LONG break, then transfer,1045

lablist.AA1.12d<-c("Full-time","Part-time","Stop-out", "Transfer" )
cpal.AA1.12d=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#FFFF99" )
PTC.AA1.12d<-  subset(PTC.AA1,cluster12==4)
PTC.AA1.12d.seq<-seqdef(PTC.AA1.12d,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA1.12d, cpal=cpal.AA1.12d)
scostAA1.12d<-seqsubm(PTC.AA1.12d.seq,method = "TRATE")

PTCAA1.12d.om <- seqdist(PTC.AA1.12d.seq, method = "OM", indel = 1, sm = scostAA1.12d,full.matrix=FALSE)
AA1rep12d<-seqrep(PTC.AA1.12d.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTCAA1.12d.om)
print(AA1rep12d)

seqrplot(PTC.AA1.12d.seq,  dist.matrix=PTCAA1.12d.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)

#5 Up to 3 years FT, a couple of years PT, then AA, 796

lablist.AA1.12e<-c("Full-time","Part-time","Stop-out", "Certificate",
                   "Associate", "Baccalaureate", "Not enrolled post-grad")
cpal.AA1.12e=c( "#7FC97F" ,"#BEAED4" ,"#FDC086" , "#386CB0", "#F0027F", "#BF5B17", "#666666" )
PTC.AA1.12e<-  subset(PTC.AA1,cluster12==5)
PTC.AA1.12e.seq<-seqdef(PTC.AA1.12e,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA1.12e, cpal=cpal.AA1.12e)

scostAA1.12e<-seqsubm(PTC.AA1.12e.seq,method = "TRATE")

PTCAA1.12e.om <- seqdist(PTC.AA1.12e.seq, method = "OM", indel = 1, sm = scostAA1.12e,full.matrix=FALSE)
AA1rep12e<-seqrep(PTC.AA1.12e.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTCAA1.12e.om)
print(AA1rep12e)

seqrplot(PTC.AA1.12e.seq,  dist.matrix=PTCAA1.12e.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)



#6 Full-time then AA graduation within 2-4 years, 5768

lablist.AA1.12f<-c("Full-time","Part-time","Stop-out", "Certificate",
                   "Associate", "Baccalaureate", "Not enrolled post-grad")
cpal.AA1.12f=c( "#7FC97F" ,"#BEAED4" ,"#FDC086" , "#386CB0", "#F0027F", "#BF5B17", "#666666" )
PTC.AA1.12f<-  subset(PTC.AA1,cluster12==6)
PTC.AA1.12f.seq<-seqdef(PTC.AA1.12f,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA1.12f, cpal=cpal.AA1.12f)
scostAA1.12f<-seqsubm(PTC.AA1.12f.seq,method = "TRATE")

PTCAA1.12f.om <- seqdist(PTC.AA1.12f.seq, method = "OM", indel = 1, sm = scostAA1.12f,full.matrix=FALSE)
AA1rep12f<-seqrep(PTC.AA1.12f.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTCAA1.12f.om)
print(AA1rep12f)

seqrplot(PTC.AA1.12f.seq,  dist.matrix=PTCAA1.12f.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)

#7 FT then break then AA Grad, if took > 2yrs of classes then immediate grad , 565
lablist.AA1.12g<-c("Full-time","Part-time","Stop-out", "Certificate",
                   "Associate", "Baccalaureate", "Not enrolled post-grad")
cpal.AA1.12g=c( "#7FC97F" ,"#BEAED4" ,"#FDC086" , "#386CB0", "#F0027F", "#BF5B17", "#666666" )
PTC.AA1.12g<-  subset(PTC.AA1,cluster12==7)
PTC.AA1.12g.seq<-seqdef(PTC.AA1.12g,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA1.12g, cpal=cpal.AA1.12g)
scostAA1.12g<-seqsubm(PTC.AA1.12g.seq,method = "TRATE")

PTCAA1.12g.om <- seqdist(PTC.AA1.12g.seq, method = "OM", indel = 1, sm = scostAA1.12g,full.matrix=FALSE)
AA1rep12g<-seqrep(PTC.AA1.12g.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTCAA1.12g.om)
print(AA1rep12g)

seqrplot(PTC.AA1.12g.seq,  dist.matrix=PTCAA1.12g.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)


#8 1 or 2 yrs of classes, 2-4.5yr break, transfer, 1222
lablist.AA1.12h<-c("Full-time","Part-time","Stop-out", "Transfer" )
cpal.AA1.12h=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#FFFF99" )
PTC.AA1.12h<-  subset(PTC.AA1,cluster12==8)
PTC.AA1.12h.seq<-seqdef(PTC.AA1.12h,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA1.12h, cpal=cpal.AA1.12h)
scostAA1.12h<-seqsubm(PTC.AA1.12h.seq,method = "TRATE")

PTCAA1.12h.om <- seqdist(PTC.AA1.12h.seq, method = "OM", indel = 1, sm = scostAA1.12h,full.matrix=FALSE)
AA1rep12h<-seqrep(PTC.AA1.12h.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTCAA1.12h.om)
print(AA1rep12h)

seqrplot(PTC.AA1.12h.seq,  dist.matrix=PTCAA1.12h.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)



#9 2-3.5 yrs of classes then stop out, 9103
lablist.AA1.12i<-c("Full-time","Part-time","Stop-out", "Transfer",
                   "Associate","Baccalaureate","Not enrolled post-grad")
cpal.AA1.12i=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#FFFF99","#F0027F", "#BF5B17", "#666666" )
PTC.AA1.12i<-  subset(PTC.AA1,cluster12==9)
PTC.AA1.12i.seq<-seqdef(PTC.AA1.12i,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA1.12i, cpal=cpal.AA1.12i)
scostAA1.12i<-seqsubm(PTC.AA1.12i.seq,method = "TRATE")

PTCAA1.12i.om <- seqdist(PTC.AA1.12i.seq, method = "OM", indel = 1, sm = scostAA1.12i,full.matrix=FALSE)
AA1rep12i<-seqrep(PTC.AA1.12i.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTCAA1.12i.om)
print(AA1rep12i)

seqrplot(PTC.AA1.12i.seq,  dist.matrix=PTCAA1.12i.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)


#10 1-2yrs FT, 2 or so yrs PT, then stop out, 3335
lablist.AA1.12j<-c("Full-time","Part-time","Stop-out", "Transfer", "Certificate",
                   "Associate","Not enrolled post-grad")
cpal.AA1.12j=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#FFFF99","#386CB0", "#F0027F","#666666" )
PTC.AA1.12j<-  subset(PTC.AA1,cluster12==10)
PTC.AA1.12j.seq<-seqdef(PTC.AA1.12j,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA1.12j, cpal=cpal.AA1.12j)
scostAA1.12j<-seqsubm(PTC.AA1.12j.seq,method = "TRATE")

PTCAA1.12j.om <- seqdist(PTC.AA1.12j.seq, method =  "OM", indel = 1, sm = scostAA1.12j,full.matrix=FALSE)
AA1rep12j<-seqrep(PTC.AA1.12j.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTCAA1.12j.om)
print(AA1rep12j)

seqrplot(PTC.AA1.12j.seq,  dist.matrix=PTCAA1.12j.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)


#11 Stop out by year 3, 26,700
lablist.AA1.12k<-c("Full-time","Part-time","Stop-out", "Transfer", "Certificate",
                   "Associate","Not enrolled post-grad")
cpal.AA1.12k=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#FFFF99","#386CB0", "#F0027F","#666666" )
PTC.AA1.12k<-  subset(PTC.AA1,cluster12==11)
PTC.AA1.12k.seq<-seqdef(PTC.AA1.12k,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA1.12k, cpal=cpal.AA1.12k)
scostAA1.12k<-seqsubm(PTC.AA1.12k.seq,method = "TRATE")

PTCAA1.12k.om <- seqdist(PTC.AA1.12k.seq, method =  "OM", indel = 1, sm = scostAA1.12k,full.matrix=FALSE)
AA1rep12k<-seqrep(PTC.AA1.12k.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTCAA1.12k.om)
print(AA1rep12k)

seqrplot(PTC.AA1.12k.seq,  dist.matrix=PTCAA1.12k.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)


#12 Transfer within 3 yrs , 2172
lablist.AA1.12l<-c("Full-time","Part-time","Stop-out", "Transfer")
cpal.AA1.12l=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#FFFF99")
PTC.AA1.12l<-  subset(PTC.AA1,cluster12==12)
PTC.AA1.12l.seq<-seqdef(PTC.AA1.12l,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA1.12l, cpal=cpal.AA1.12l)
scostAA1.12l<-seqsubm(PTC.AA1.12l.seq,method = "TRATE")

PTCAA1.12l.om <- seqdist(PTC.AA1.12l.seq, method =  "OM", indel = 1, sm = scostAA1.12l,full.matrix=FALSE)
AA1rep12l<-seqrep(PTC.AA1.12l.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTCAA1.12l.om)
print(AA1rep12l)

seqrplot(PTC.AA1.12l.seq,  dist.matrix=PTCAA1.12l.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)




########################################
#Representative Sequences for AA2, 8 clusters total, 31524 obs total
#######################################


uniqueCluster8AA2 <- WardClustQualAA2.20$clustering$cluster8
PTC.AA2$cluster8 <- uniqueCluster8AA2[aggPTCAA2$disaggIndex]
table(PTC.AA2$ethnicity.imputed.code, PTC.AA2$cluster8)
table(PTC.AA2$cluster8)


#1 Earned AA in 3-5 yrs, earned BA in 8-9.5 yrs, 1064
PTC.AA2.8a<-  subset(PTC.AA2,cluster8==1)
PTC.AA2.8a.seq<-seqdef(PTC.AA2.8a,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist)
PTC.AA2.8a.om <- seqdist(PTC.AA2.8a.seq, method = "OM", indel = 1, sm = scostAA2,full.matrix=FALSE)
AA2rep8a<-seqrep(PTC.AA2.8a.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTC.AA2.8a.om)
print(AA2rep8a)

seqrplot(PTC.AA2.8a.seq,  dist.matrix=PTC.AA2.8a.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)



#2 Stop out by 5 yrs staggered, 3584
lablist.AA2.8b<-c("Full-time","Part-time","Stop-out", "Transfer", 
                   "Associate","Baccalaureate", "Not enrolled post-grad")
cpal.AA2.8b=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#FFFF99","#BF5B17", "#F0027F","#666666" )

PTC.AA2.8b<-  subset(PTC.AA2,cluster8==2)
PTC.AA2.8b.seq<-seqdef(PTC.AA2.8b,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                        "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                        "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                        "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                        "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA2.8b,cpal=cpal.AA2.8b)
scostAA2.8b<-seqsubm(PTC.AA2.8b.seq,method = "TRATE")
PTC.AA2.8b.om <- seqdist(PTC.AA2.8b.seq, method = "OM", indel = 1, sm = scostAA2.8b,full.matrix=FALSE)
AA2rep8b<-seqrep(PTC.AA2.8b.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTC.AA2.8b.om)
print(AA2rep8b)

seqrplot(PTC.AA2.8b.seq,  dist.matrix=PTC.AA2.8b.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)


#3 Earned AA in 2.5-4yrs then BA in 5-6.5yrs, 8489
lablist.AA2.8c<-c("Full-time","Part-time","Stop-out", "Certificate",
                  "Associate","Baccalaureate", "Not enrolled post-grad")
cpal.AA2.8c=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#386CB0","#F0027F","#BF5B17", "#666666" )

PTC.AA2.8c<-  subset(PTC.AA2,cluster8==3)
PTC.AA2.8c.seq<-seqdef(PTC.AA2.8c,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                        "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                        "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                        "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                        "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA2.8c,cpal=cpal.AA2.8c)
scostAA2.8c<-seqsubm(PTC.AA2.8c.seq,method = "TRATE")
PTC.AA2.8c.om <- seqdist(PTC.AA2.8c.seq, method = "OM", indel = 1, sm = scostAA2.8c,full.matrix=FALSE)
AA2rep8c<-seqrep(PTC.AA2.8c.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTC.AA2.8c.om)
print(AA2rep8c)

##These are the default margins in R. They need to be set to reset from earlier experiments.
par("mar"=c(5.1,4.1,4.1,2.1))
seqrplot(PTC.AA2.8c.seq,  dist.matrix=PTC.AA2.8c.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)


#4 1-2yrs enrolled then 5-8yr break then transfer (2,450)
lablist.AA2.8d<-c("Full-time","Part-time","Stop-out", "Transfer" )
cpal.AA2.8d=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#FFFF99")

PTC.AA2.8d<-  subset(PTC.AA2,cluster8==4)
PTC.AA2.8d.seq<-seqdef(PTC.AA2.8d,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                        "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                        "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                        "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                        "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA2.8d,cpal=cpal.AA2.8d)
scostAA2.8d<-seqsubm(PTC.AA2.8d.seq,method = "TRATE")
PTC.AA2.8d.om <- seqdist(PTC.AA2.8d.seq, method = "OM", indel = 1, sm = scostAA2.8d,full.matrix=FALSE)
AA2rep8d<-seqrep(PTC.AA2.8d.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTC.AA2.8d.om)
print(AA2rep8d)

seqrplot(PTC.AA2.8d.seq,  dist.matrix=PTC.AA2.8d.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)

#5 1-2.4yrs enrolled, upt to 2.5yr break, then transfer (6,833)
lablist.AA2.8e<-c("Full-time","Part-time","Stop-out", "Transfer" )
cpal.AA2.8e=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#FFFF99")

PTC.AA2.8e<-  subset(PTC.AA2,cluster8==5)
PTC.AA2.8e.seq<-seqdef(PTC.AA2.8e,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                        "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                        "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                        "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                        "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA2.8e,cpal=cpal.AA2.8e)
scostAA2.8e<-seqsubm(PTC.AA2.8e.seq,method = "TRATE")
PTC.AA2.8e.om <- seqdist(PTC.AA2.8e.seq, method = "OM", indel = 1, sm = scostAA2.8e,full.matrix=FALSE)
AA2rep8e<-seqrep(PTC.AA2.8e.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTC.AA2.8e.om)
print(AA2rep8e)

seqrplot(PTC.AA2.8e.seq,  dist.matrix=PTC.AA2.8e.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)


#6 Start AA, a 4-6yr break,transfer to BA, some get AA or BA after 8.5yrs, all over the place (1,384)
PTC.AA2.8f<-  subset(PTC.AA2,cluster8==6)
PTC.AA2.8f.seq<-seqdef(PTC.AA2.8f,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                        "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                        "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                        "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                        "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist)

PTC.AA2.8f.om <- seqdist(PTC.AA2.8f.seq, method = "OM", indel = 1, sm = scostAA2,full.matrix=FALSE)
AA2rep8f<-seqrep(PTC.AA2.8f.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTC.AA2.8f.om)
print(AA2rep8f)

seqrplot(PTC.AA2.8f.seq,  dist.matrix=PTC.AA2.8f.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)


#7 AA in 3-5yrs then BA in 5.5-8yrs largely PT (2,128)

PTC.AA2.8g<-  subset(PTC.AA2,cluster8==7)
PTC.AA2.8g.seq<-seqdef(PTC.AA2.8g,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                        "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                        "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                        "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                        "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist)
PTC.AA2.8g.om <- seqdist(PTC.AA2.8g.seq, method = "OM", indel = 1, sm = scostAA2,full.matrix=FALSE)
AA2rep8g<-seqrep(PTC.AA2.8g.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTC.AA2.8g.om)
print(AA2rep8g)

seqrplot(PTC.AA2.8g.seq,  dist.matrix=PTC.AA2.8g.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)

#8 AA within 2-4.5 yrs and BA within 2.5yrs of that With some terminal AA (5,592)
lablist.AA2.8h<-c("Full-time","Part-time","Stop-out",  "Certificate",
                  "Associate","Baccalaureate","Not enrolled post-grad")
cpal.AA2.8h=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#386CB0", "#F0027F", "#BF5B17" ,"#666666")

PTC.AA2.8h<-  subset(PTC.AA2,cluster8==8)
PTC.AA2.8h.seq<-seqdef(PTC.AA2.8h,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                        "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                        "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                        "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                        "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.AA2.8h,cpal=cpal.AA2.8h)
scostAA2.8h<-seqsubm(PTC.AA2.8h.seq,method = "TRATE")
PTC.AA2.8h.om <- seqdist(PTC.AA2.8h.seq, method = "OM", indel = 1, sm = scostAA2.8h,full.matrix=FALSE)
AA2rep8h<-seqrep(PTC.AA2.8h.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTC.AA2.8h.om)
print(AA2rep8h)

seqrplot(PTC.AA2.8h.seq,  dist.matrix=PTC.AA2.8h.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)




########################################
#Representative Sequences for CERT, 4 clusters total, 454 obs total
#######################################


uniqueCluster4CERT <- WardClustQualCERT.20$clustering$cluster4
PTC.CERT$cluster4 <- uniqueCluster4CERT[aggPTCCERT$disaggIndex]
table(PTC.CERT$ethnicity.imputed.code, PTC.CERT$cluster4)
table(PTC.CERT$cluster4)

#1 Some successful return to get a cert after a degree but some cert only and some drop out (107)
lablist.CERT.4a<-c("Full-time","Part-time","Stop-out",  "Certificate",
                   "Associate","Baccalaureate","Not enrolled post-grad")
cpal.CERT.4a=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#386CB0", "#F0027F", "#BF5B17" ,"#666666")

PTC.CERT.4a<-  subset(PTC.CERT,cluster4==1)
PTC.CERT.4a.seq<-seqdef(PTC.CERT.4a,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.CERT.4a,cpal=cpal.CERT.4a)
scostCERT.4a<-seqsubm(PTC.CERT.4a.seq,method = "TRATE")
PTC.CERT.4a.om <- seqdist(PTC.CERT.4a.seq, method = "OM", indel = 1, sm = scostCERT.4a,full.matrix=FALSE)
CERTrep4a<-seqrep(PTC.CERT.4a.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTC.CERT.4a.om)
print(CERTrep4a)

seqrplot(PTC.CERT.4a.seq,  dist.matrix=PTC.CERT.4a.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)


#2 A lot of returning after AA but little completion plus some cert only (46)
lablist.CERT.4b<-c("Full-time","Part-time","Stop-out",  "Certificate",
                   "Associate","Baccalaureate","Not enrolled post-grad")
cpal.CERT.4b=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#386CB0", "#F0027F", "#BF5B17" ,"#666666")

PTC.CERT.4b<-  subset(PTC.CERT,cluster4==2)
PTC.CERT.4b.seq<-seqdef(PTC.CERT.4b,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.CERT.4b,cpal=cpal.CERT.4b)
scostCERT.4b<-seqsubm(PTC.CERT.4b.seq,method = "TRATE")
PTC.CERT.4b.om <- seqdist(PTC.CERT.4b.seq, method = "OM", indel = 1, sm = scostCERT.4b,full.matrix=FALSE)
CERTrep4b<-seqrep(PTC.CERT.4b.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTC.CERT.4b.om)
print(CERTrep4b)

seqrplot(PTC.CERT.4b.seq,  dist.matrix=PTC.CERT.4b.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)

#3 Stop out by yr 4 (250)

lablist.CERT.4c<-c("Full-time","Part-time","Stop-out", "Transfer" ,"Certificate")
cpal.CERT.4c=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#FFFF99","#386CB0")

PTC.CERT.4c<-  subset(PTC.CERT,cluster4==3)
PTC.CERT.4c.seq<-seqdef(PTC.CERT.4c,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.CERT.4c,cpal=cpal.CERT.4c)
scostCERT.4c<-seqsubm(PTC.CERT.4c.seq,method = "TRATE")
PTC.CERT.4c.om <- seqdist(PTC.CERT.4c.seq, method = "OM", indel = 1, sm = scostCERT.4c,full.matrix=FALSE)
CERTrep4c<-seqrep(PTC.CERT.4c.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTC.CERT.4c.om)
print(CERTrep4c)

seqrplot(PTC.CERT.4c.seq,  dist.matrix=PTC.CERT.4c.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)


#4 Successful Certificate students plus a couple AAs returning  (51)

lablist.CERT.4d<-c("Full-time","Part-time","Stop-out","Certificate", "Associate","Not enrolled post-grad")
cpal.CERT.4d=c("#7FC97F" ,"#BEAED4" ,"#FDC086","#386CB0", "#F0027F","#666666")

PTC.CERT.4d<-  subset(PTC.CERT,cluster4==4)
PTC.CERT.4d.seq<-seqdef(PTC.CERT.4d,var=c("r.ftptcode.sem1", "r.ftptcode.sem2", "r.ftptcode.sem3","r.ftptcode.sem4",
                                          "r.ftptcode.sem5","r.ftptcode.sem6","r.ftptcode.sem7","r.ftptcode.sem8",
                                          "r.ftptcode.sem9","r.ftptcode.sem10","r.ftptcode.sem11","r.ftptcode.sem12",
                                          "r.ftptcode.sem13","r.ftptcode.sem14","r.ftptcode.sem15","r.ftptcode.sem16",
                                          "r.ftptcode.sem17","r.ftptcode.sem18","r.ftptcode.sem19","r.ftptcode.sem20"
),labels=lablist.CERT.4d,cpal=cpal.CERT.4d)
scostCERT.4d<-seqsubm(PTC.CERT.4d.seq,method = "TRATE")
PTC.CERT.4d.om <- seqdist(PTC.CERT.4d.seq, method = "OM", indel = 1, sm = scostCERT.4d,full.matrix=FALSE)
CERTrep4d<-seqrep(PTC.CERT.4d.seq,nrep=2, tsim=0.1,criterion="density",dist.matrix=PTC.CERT.4d.om)
print(CERTrep4d)

seqrplot(PTC.CERT.4d.seq,  dist.matrix=PTC.CERT.4d.om,nrep=10, tsim=0.1,cex.plot=.70,legend.prop=.21,use.layout=TRUE,
         withlegend="right",xtlab=1:20,pbarw=TRUE,cex.legend=.55)







#Creating an indicator variable for the macro groups within the partitions. 
#For example, within the BA1 partition, I want to first look at those who graduate
#compared with those who don't and those who transfer on a variety of demographics
#and h.s. preparation

#BA1
table(PTC.BA1$cluster10)
#1     2     3     4     5     6     7     8     9    10 
#882  2178  1923   483   570 16288  3715   804   515  7502 
PTC.BA1$clustCat[PTC.BA1$cluster10==5] <- "Transfer"
PTC.BA1$clustCat[PTC.BA1$cluster10==7] <- "Transfer"
PTC.BA1$clustCat[PTC.BA1$cluster10==10] <- "Stop Out"
PTC.BA1$clustCat[PTC.BA1$cluster10==3] <- "Stop Out"
PTC.BA1$clustCat[PTC.BA1$cluster10==2] <- "Graduate"
PTC.BA1$clustCat[PTC.BA1$cluster10==6] <- "Graduate"
PTC.BA1$clustCat[PTC.BA1$cluster10==1] <- "Graduate"
PTC.BA1$clustCat[PTC.BA1$cluster10==8] <- "Graduate"
PTC.BA1$clustCat[PTC.BA1$cluster10==9] <- "Graduate"
PTC.BA1$clustCat[PTC.BA1$cluster10==4] <- "Other"

table(PTC.BA1$clustCat)
# Graduate    Other Stop Out Transfer 
# 20667      483     9425     4285 

RaceTable.PTC.BA1<-table(PTC.BA1$clustCat,PTC.BA1$ethnicity.imputed.code)
RaceTable.PTC.BA1
prop.table(RaceTable.PTC.BA1,2)

RaceTable2.PTC.BA1<-table(PTC.BA1$ethnicity.imputed.code,PTC.BA1$clustCat)
RaceTable2.PTC.BA1
prop.table(RaceTable2.PTC.BA1,2)


GenderTable.PTC.BA1<-table(PTC.BA1$clustCat,PTC.BA1$female)
GenderTable.PTC.BA1
prop.table(GenderTable.PTC.BA1,2)

GenderTable2.PTC.BA1<-table(PTC.BA1$female,PTC.BA1$clustCat)
GenderTable2.PTC.BA1
prop.table(GenderTable2.PTC.BA1,2)

SpringTable.PTC.BA1<-table(PTC.BA1$clustCat,PTC.BA1$spring)
SpringTable.PTC.BA1
prop.table(SpringTable.PTC.BA1,2)

SpringTable2.PTC.BA1<-table(PTC.BA1$spring,PTC.BA1$clustCat)
SpringTable2.PTC.BA1
prop.table(SpringTable2.PTC.BA1,2)


NodelayTable.PTC.BA1<-table(PTC.BA1$clustCat,PTC.BA1$nodelay)
NodelayTable.PTC.BA1
prop.table(NodelayTable.PTC.BA1,2)

NodelayTable2.PTC.BA1<-table(PTC.BA1$nodelay,PTC.BA1$clustCat)
NodelayTable2.PTC.BA1
prop.table(NodelayTable2.PTC.BA1,2)

DependentTable2.PTC.BA1<-table(PTC.BA1$dependent,PTC.BA1$clustCat)
DependentTable2.PTC.BA1
prop.table(DependentTable2.PTC.BA1,2)


PellTable2.PTC.BA1<-table(PTC.BA1$pell,PTC.BA1$clustCat)
PellTable2.PTC.BA1
prop.table(PellTable2.PTC.BA1,2)

#Average Age by Cluster
tapply(PTC.BA1$entry.age,PTC.BA1$clustCat,mean,na.rm=TRUE)

#Average CPI
tapply(PTC.BA1$cpi.units.total,PTC.BA1$clustCat,mean,na.rm=TRUE)

#Average HS GPA
tapply(PTC.BA1$caa.total,PTC.BA1$clustCat,mean,na.rm=TRUE)

#Average SAT Total
tapply(PTC.BA1$cas.sat.total.recntrd,PTC.BA1$clustCat,mean,na.rm=TRUE)

#Average 1st Sem Crds
tapply(PTC.BA1$crdsem01,PTC.BA1$clustCat,mean,na.rm=TRUE)

#Average 1st Sem GPA
tapply(PTC.BA1$gpasem01,PTC.BA1$clustCat,mean,na.rm=TRUE)



#BA2

table(PTC.BA2$cluster8)
#   1    2    3    4    5    6    7    8 
#  317  565  398  563  729  158  304 1051 

#The graduate category is less informative here because it includes those who
#only got an AA and those who got a BA and then went for an AA

PTC.BA2$clustCat[PTC.BA2$cluster8==5] <- "Transfer"
PTC.BA2$clustCat[PTC.BA2$cluster8==7] <- "Transfer"
PTC.BA2$clustCat[PTC.BA2$cluster8==8] <- "Stop Out"
PTC.BA2$clustCat[PTC.BA2$cluster8==4] <- "Stop Out"
PTC.BA2$clustCat[PTC.BA2$cluster8==2] <- "Stop Out"
PTC.BA2$clustCat[PTC.BA2$cluster8==3] <- "Graduate"
PTC.BA2$clustCat[PTC.BA2$cluster8==1] <- "Graduate"
PTC.BA2$clustCat[PTC.BA2$cluster8==6] <- "Graduate"

table(PTC.BA2$clustCat)
# Graduate Stop Out Transfer 
# 873     2179     1033 

RaceTable.PTC.BA2<-table(PTC.BA2$clustCat,PTC.BA2$ethnicity.imputed.code)
RaceTable.PTC.BA2
prop.table(RaceTable.PTC.BA2,2)

RaceTable2.PTC.BA2<-table(PTC.BA2$ethnicity.imputed.code,PTC.BA2$clustCat)
RaceTable2.PTC.BA2
prop.table(RaceTable2.PTC.BA2,2)

GenderTable.PTC.BA2<-table(PTC.BA2$clustCat,PTC.BA2$female)
GenderTable.PTC.BA2
prop.table(GenderTable.PTC.BA2,2)

GenderTable2.PTC.BA2<-table(PTC.BA2$female,PTC.BA2$clustCat)
GenderTable2.PTC.BA2
prop.table(GenderTable2.PTC.BA2,2)

SpringTable.PTC.BA2<-table(PTC.BA2$clustCat,PTC.BA2$spring)
SpringTable.PTC.BA2
prop.table(SpringTable.PTC.BA2,2)

SpringTable2.PTC.BA2<-table(PTC.BA2$spring,PTC.BA2$clustCat)
SpringTable2.PTC.BA2
prop.table(SpringTable2.PTC.BA2,2)

NodelayTable.PTC.BA2<-table(PTC.BA2$clustCat,PTC.BA2$nodelay)
NodelayTable.PTC.BA2
prop.table(NodelayTable.PTC.BA2,2)

NodelayTable2.PTC.BA2<-table(PTC.BA2$nodelay,PTC.BA2$clustCat)
NodelayTable2.PTC.BA2
prop.table(NodelayTable2.PTC.BA2,2)

DependentTable2.PTC.BA2<-table(PTC.BA2$dependent,PTC.BA2$clustCat)
DependentTable2.PTC.BA2
prop.table(DependentTable2.PTC.BA2,2)

PellTable2.PTC.BA2<-table(PTC.BA2$pell,PTC.BA2$clustCat)
PellTable2.PTC.BA2
prop.table(PellTable2.PTC.BA2,2)

#Average Age by Cluster
tapply(PTC.BA2$entry.age,PTC.BA2$clustCat,mean,na.rm=TRUE)

#Average CPI
tapply(PTC.BA2$cpi.units.total,PTC.BA2$clustCat,mean,na.rm=TRUE)

#Average HS GPA
tapply(PTC.BA2$caa.total,PTC.BA2$clustCat,mean,na.rm=TRUE)

#Average SAT Total
tapply(PTC.BA2$cas.sat.total.recntrd,PTC.BA2$clustCat,mean,na.rm=TRUE)

#Average 1st Sem Crds
tapply(PTC.BA2$crdsem01,PTC.BA2$clustCat,mean,na.rm=TRUE)

#Average 1st Sem GPA
tapply(PTC.BA2$gpasem01,PTC.BA2$clustCat,mean,na.rm=TRUE)


#AA1
table(PTC.AA1$cluster12)
#    1     2     3     4     5     6     7     8     9    10    11    12 
#   810  1001  1814  1045   796  5786   565  1222  9103  3335 27600  2172 

PTC.AA1$clustCat[PTC.AA1$cluster12==12] <- "Transfer"
PTC.AA1$clustCat[PTC.AA1$cluster12==8] <- "Transfer"
PTC.AA1$clustCat[PTC.AA1$cluster12==4] <- "Transfer"
PTC.AA1$clustCat[PTC.AA1$cluster12==11] <- "Stop Out"
PTC.AA1$clustCat[PTC.AA1$cluster12==9] <- "Stop Out"
PTC.AA1$clustCat[PTC.AA1$cluster12==10] <- "Stop Out"
PTC.AA1$clustCat[PTC.AA1$cluster12==3] <- "Stop Out"
PTC.AA1$clustCat[PTC.AA1$cluster12==1] <- "Stop Out"
PTC.AA1$clustCat[PTC.AA1$cluster12==6] <- "Graduate"
PTC.AA1$clustCat[PTC.AA1$cluster12==2] <- "Graduate"
PTC.AA1$clustCat[PTC.AA1$cluster12==5] <- "Graduate"
PTC.AA1$clustCat[PTC.AA1$cluster12==7] <- "Graduate"
table(PTC.AA1$clustCat)

#Graduate Stop Out Transfer 
#  8148    42662     4439 

RaceTable.PTC.AA1<-table(PTC.AA1$clustCat,PTC.AA1$ethnicity.imputed.code)
RaceTable.PTC.AA1
prop.table(RaceTable.PTC.AA1,2)

RaceTable2.PTC.AA1<-table(PTC.AA1$ethnicity.imputed.code,PTC.AA1$clustCat)
RaceTable2.PTC.AA1
prop.table(RaceTable2.PTC.AA1,2)

GenderTable.PTC.AA1<-table(PTC.AA1$clustCat,PTC.AA1$female)
GenderTable.PTC.AA1
prop.table(GenderTable.PTC.AA1,2)

GenderTable2.PTC.AA1<-table(PTC.AA1$female,PTC.AA1$clustCat)
GenderTable2.PTC.AA1
prop.table(GenderTable2.PTC.AA1,2)

SpringTable.PTC.AA1<-table(PTC.AA1$clustCat,PTC.AA1$spring)
SpringTable.PTC.AA1
prop.table(SpringTable.PTC.AA1,2)

SpringTable2.PTC.AA1<-table(PTC.AA1$spring,PTC.AA1$clustCat)
SpringTable2.PTC.AA1
prop.table(SpringTable2.PTC.AA1,2)

NodelayTable.PTC.AA1<-table(PTC.AA1$clustCat,PTC.AA1$nodelay)
NodelayTable.PTC.AA1
prop.table(NodelayTable.PTC.AA1,2)

NodelayTable2.PTC.AA1<-table(PTC.AA1$nodelay,PTC.AA1$clustCat)
NodelayTable2.PTC.AA1
prop.table(NodelayTable2.PTC.AA1,2)

DependentTable2.PTC.AA1<-table(PTC.AA1$dependent,PTC.AA1$clustCat)
DependentTable2.PTC.AA1
prop.table(DependentTable2.PTC.AA1,2)

PellTable2.PTC.AA1<-table(PTC.AA1$pell,PTC.AA1$clustCat)
PellTable2.PTC.AA1
prop.table(PellTable2.PTC.AA1,2)


#Average Age by Cluster
tapply(PTC.AA1$entry.age,PTC.AA1$clustCat,mean,na.rm=TRUE)

#Average CPI
tapply(PTC.AA1$cpi.units.total,PTC.AA1$clustCat,mean,na.rm=TRUE)

#Average HS GPA
tapply(PTC.AA1$caa.total,PTC.AA1$clustCat,mean,na.rm=TRUE)

#Average SAT Total
tapply(PTC.AA1$cas.sat.total.recntrd,PTC.AA1$clustCat,mean,na.rm=TRUE)

#Average 1st Sem Crds
tapply(PTC.AA1$crdsem01,PTC.AA1$clustCat,mean,na.rm=TRUE)

#Average 1st Sem GPA
tapply(PTC.AA1$gpasem01,PTC.AA1$clustCat,mean,na.rm=TRUE)




#AA2
table(PTC.AA2$cluster8)
# 1    2    3    4    5    6    7    8 
#1064 3584 8489 2450 6833 1384 2128 5592 

PTC.AA2$clustCat[PTC.AA2$cluster8==5] <- "Transfer"
PTC.AA2$clustCat[PTC.AA2$cluster8==4] <- "Transfer"
PTC.AA2$clustCat[PTC.AA2$cluster8==2] <- "Stop Out"
PTC.AA2$clustCat[PTC.AA2$cluster8==6] <- "Other"
PTC.AA2$clustCat[PTC.AA2$cluster8==7] <- "Graduate"
PTC.AA2$clustCat[PTC.AA2$cluster8==8] <- "Graduate"
PTC.AA2$clustCat[PTC.AA2$cluster8==1] <- "Graduate"
PTC.AA2$clustCat[PTC.AA2$cluster8==3] <- "Graduate"

table(PTC.AA2$clustCat)

#Graduate    Other Stop Out Transfer 
#  17273     1384     3584     9283 

RaceTable.PTC.AA2<-table(PTC.AA2$clustCat,PTC.AA2$ethnicity.imputed.code)
RaceTable.PTC.AA2
prop.table(RaceTable.PTC.AA2,2)

RaceTable2.PTC.AA2<-table(PTC.AA2$ethnicity.imputed.code,PTC.AA2$clustCat)
RaceTable2.PTC.AA2
prop.table(RaceTable2.PTC.AA2,2)

GenderTable.PTC.AA2<-table(PTC.AA2$clustCat,PTC.AA2$female)
GenderTable.PTC.AA2
prop.table(GenderTable.PTC.AA2,2)

GenderTable2.PTC.AA2<-table(PTC.AA2$female,PTC.AA2$clustCat)
GenderTable2.PTC.AA2
prop.table(GenderTable2.PTC.AA2,2)

SpringTable.PTC.AA2<-table(PTC.AA2$clustCat,PTC.AA2$spring)
SpringTable.PTC.AA2
prop.table(SpringTable.PTC.AA2,2)

SpringTable2.PTC.AA2<-table(PTC.AA2$spring,PTC.AA2$clustCat)
SpringTable2.PTC.AA2
prop.table(SpringTable2.PTC.AA2,2)

NodelayTable.PTC.AA2<-table(PTC.AA2$clustCat,PTC.AA2$nodelay)
NodelayTable.PTC.AA2
prop.table(NodelayTable.PTC.AA2,2)

NodelayTable2.PTC.AA2<-table(PTC.AA2$nodelay,PTC.AA2$clustCat)
NodelayTable2.PTC.AA2
prop.table(NodelayTable2.PTC.AA2,2)

DependentTable2.PTC.AA2<-table(PTC.AA2$dependent,PTC.AA2$clustCat)
DependentTable2.PTC.AA2
prop.table(DependentTable2.PTC.AA2,2)

PellTable2.PTC.AA2<-table(PTC.AA2$pell,PTC.AA2$clustCat)
PellTable2.PTC.AA2
prop.table(PellTable2.PTC.AA2,2)


#Average Age by Cluster
tapply(PTC.AA2$entry.age,PTC.AA2$clustCat,mean,na.rm=TRUE)

#Average CPI
tapply(PTC.AA2$cpi.units.total,PTC.AA2$clustCat,mean,na.rm=TRUE)

#Average HS GPA
tapply(PTC.AA2$caa.total,PTC.AA2$clustCat,mean,na.rm=TRUE)

#Average SAT Total
tapply(PTC.AA2$cas.sat.total.recntrd,PTC.AA2$clustCat,mean,na.rm=TRUE)

#Average 1st Sem Crds
tapply(PTC.AA2$crdsem01,PTC.AA2$clustCat,mean,na.rm=TRUE)

#Average 1st Sem GPA
tapply(PTC.AA2$gpasem01,PTC.AA2$clustCat,mean,na.rm=TRUE)


#CERT
table(PTC.CERT$cluster4)
# 1   2   3   4 
#107  46 250  51 

PTC.CERT$clustCat[PTC.CERT$cluster4==1] <- "Other"
PTC.CERT$clustCat[PTC.CERT$cluster4==4] <- "Graduate"
PTC.CERT$clustCat[PTC.CERT$cluster4==2] <- "Graduate"
PTC.CERT$clustCat[PTC.CERT$cluster4==3] <- "Stop Out"

table(PTC.CERT$clustCat)
#Graduate    Other Stop Out 
#    97      107      250 

RaceTable.PTC.CERT<-table(PTC.CERT$clustCat,PTC.CERT$ethnicity.imputed.code)
RaceTable.PTC.CERT
prop.table(RaceTable.PTC.CERT,2)

RaceTable2.PTC.CERT<-table(PTC.CERT$ethnicity.imputed.code,PTC.CERT$clustCat)
RaceTable2.PTC.CERT
prop.table(RaceTable2.PTC.CERT,2)

GenderTable.PTC.CERT<-table(PTC.CERT$clustCat,PTC.CERT$female)
GenderTable.PTC.CERT
prop.table(GenderTable.PTC.CERT,2)

GenderTable2.PTC.CERT<-table(PTC.CERT$female,PTC.CERT$clustCat)
GenderTable2.PTC.CERT
prop.table(GenderTable2.PTC.CERT,2)

SpringTable.PTC.CERT<-table(PTC.CERT$clustCat,PTC.CERT$spring)
SpringTable.PTC.CERT
prop.table(SpringTable.PTC.CERT,2)

NodelayTable.PTC.CERT<-table(PTC.CERT$clustCat,PTC.CERT$nodelay)
NodelayTable.PTC.CERT
prop.table(NodelayTable.PTC.CERT,2)



#####################################
#Creating an overall data set from the degree band data sets in order to allow
#higher order analysis. 




###########################################################################
#Start here
###########################################################################










#Type 1, n=20,357: On-Time Graduation
#Type 2, n=13,111: Stop Out
#Type 3, n=1,855: Delayed Graduation
#Type 4, n= 3,683: Transfer

PTC.BA$cluster4<-factor(PTC.BA$cluster4,levels=c(1,3,4,2), labels=c("On-Time Grad","Delayed Grad","Transfer","Stop Out"))
PTC.BA$ethnicity.imputed.code<-factor(PTC.BA$ethnicity.imputed.code,levels=c(1,2,4,5,6), labels=c("White","Black","Hispanic","Asian","Native American"))

ClustRace<-table(PTC.BA$ethnicity.imputed.code, PTC.BA$cluster4)
ClustRacePer<-prop.table(ClustRace,2)
xtable(round((ClustRacePer*100),2))
xtable(ClustRacePer*100)
ClustRace2<-table( PTC.BA$cluster4,PTC.BA$ethnicity.imputed.code)
ClustRacePer2<-prop.table(ClustRace2,2)
round((ClustRacePer2*100),2)
xtable(ClustRacePer2*100)

PTC.BA$sex<-as.factor(PTC.BA$gender.desc)
ClustSex<-table(PTC.BA$sex, PTC.BA$cluster4)
ClustSexPer<-prop.table(ClustSex,2)
xtable(ClustSexPer*100)
ClustSex2<-table(PTC.BA$cluster4,PTC.BA$sex)
ClustSexPer2<-prop.table(ClustSex2,2)
xtable(ClustSexPer2*100)

PTC.BA$dependent<-as.factor(PTC.BA$dependency.status.code)
PTC.BA$dependent<- factor(PTC.BA$dependent,labels=c("Not Dependent","Dependent"))
ClustDepend<-table(PTC.BA$cluster4,PTC.BA$dependent)
ClustDependPer<-prop.table(ClustDepend,2)
xtable(ClustDependPer*100)

#note there is missingness here that will be removed in the LaTeX
ClustPellBA<-table(PTC.BA$cluster4,PTC.BA$pell.flag)
ClustPellPerBA<-prop.table(ClustPellBA,2)
xtable(ClustPellPerBA*100)

PTC.BA$delay<-as.factor(PTC.BA$nodelay)
table(PTC.BA$nodelay,PTC.BA$delay) #looks good
PTC.BA$delay<- factor(PTC.BA$delay,labels=c("No Delay","Delay"))
ClustDelayBA<-table(PTC.BA$cluster4,PTC.BA$delay)
ClustDelayPerBA<-prop.table(ClustDelayBA,2)
xtable(ClustDelayPerBA*100)

PTC.BA$spring2<-as.factor(PTC.BA$spring)
table(PTC.BA$spring,PTC.BA$spring2) #looks good
PTC.BA$spring2<- factor(PTC.BA$spring2,labels=c("Fall Entrant","Spring Entrant"))
ClustSpring2BA<-table(PTC.BA$cluster4,PTC.BA$spring2)
ClustSpring2PerBA<-prop.table(ClustSpring2BA,2)
xtable(ClustSpring2PerBA*100)
 

Covariates1<-c("CPI","caa.total","PTC.BA$cas.sat.total.recntrd","PTC.BA$gpasem01", "PTC.BA$crdsem01","PTC.BA$entry.age")

summary(PTC.BA$cpi.units.total)

BASum<-(summaryBy(cpi.units.total + caa.total + cas.sat.total.recntrd + gpasem01 + crdsem01 + entry.age ~ cluster4, 
          data=PTC.BA,FUN=c(mean,sd),na.rm=TRUE))



MlogitBA <- multinom(PTC.BA$cluster4 ~ RaceBA.f + cpi.units.total + caa.total+ cas.sat.total.recntrd + dependency.status.code + pell.flag + nodelay + spring + gpasem01 + crdsem01 + entry.age + sex, data = PTC.BA)

ModelVars<-c("cluster4","RaceBA.f","cpi.units.total","caa.total",
             "cas.sat.total.recntrd","dependency.status.code","pell.flag",
             "nodelay", "spring","gpasem01","crdsem01","entry.age", "sex",
             "oira.student.id")


LongPTCBA<-PTC.BA[ModelVars]
LongPTCBA<-na.omit(LongPTCBA)
ProbitDataBA<-mlogit.data(LongPTCBA,shape="wide",sep="",choice=LongPTCBA$cluster4,id.var=LongPTCBA$oira.student.id)
MprobitBA <- mlogit(PTC.BA$cluster4 ~ RaceBA.f + cpi.units.total + caa.total+ cas.sat.total.recntrd + dependency.status.code + pell.flag + nodelay + spring + gpasem01 + crdsem01 + entry.age + sex, data = PTC.BA,probit=TRUE)




################################
#Multinomial Logistic Regression Associate

#This is an example of how to get back to the original data.
uniqueCluster4AA <- WardClustQualAA$clustering$cluster4
PTC.AA$cluster4 <- uniqueCluster4AA[aggPTCAA$disaggIndex]
table(PTC.AA$cluster4)
#Type 1, n=23,412: On-Time Graduation
#Type 2, n=3,125: Delayed Graduation
#Type 3, n=52,924: Stop Out
#Type 4, n= 7,705: Transfer



#note there is missingness here that will be removed in the LaTeX

#The following code tells me what the column #s of the seq variables are. 
which(names(PTC) %in% c("r.ftptcode.sem1","r.ftptcode.sem20"))
which(names(PTC) %in% c("degree.pursued.level.code"))
which(names(PTC) %in% c("spring"))
which(names(PTC) %in% c("cpi.units.total"))

which(names(PTC.BA) %in% c("oira.student.id","cluster4"))
write.table(PTC.BA,file="BACluster8.csv",sep=",",row.names=FALSE,na="")

write.table(PTC.AA,file="AACluster.csv",sep=",",row.names=FALSE,na="")

write.table(PTC.BA1,file="BA1.csv",sep=",",row.names=FALSE,na="")
write.table(PTC.BA2,file="BA2.csv",sep=",",row.names=FALSE,na="")
write.table(PTC.AA1,file="AA1.csv",sep=",",row.names=FALSE,na="")
write.table(PTC.AA2,file="AA2.csv",sep=",",row.names=FALSE,na="")
write.table(PTC.CERT,file="CERT.csv",sep=",",row.names=FALSE,na="")

write.table(PTC.BA2.8a, file="BA2_c1.csv",sep=",",row.names=FALSE,na="")
