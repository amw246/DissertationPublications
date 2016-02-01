# Syntax to create the top twenty patterns by degree band


png(file = "/Users/andrewwallace/DissertationPublications/ClusterAnalysis/Analysis/Figures/Top20BA1.png",
    width = 736, height = 481)
#CONSTANT HEIGHT,BA1
par("mar"=c(7,4,2,5)+.1)
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
       inset=-.25, bty="o", xpd=NA, cex=.75, ncol=2)
dev.off()


png(file = "/Users/andrewwallace/DissertationPublications/ClusterAnalysis/Analysis/Figures/Top20BA2.png",
    width = 736, height = 481)
#CONSTANT HEIGHT,BA2
par("mar"=c(7,4,2,5)+.1)
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
       inset=-.25, bty="o", xpd=NA, cex=.75, ncol=2)
dev.off()


png(file = "/Users/andrewwallace/DissertationPublications/ClusterAnalysis/Analysis/Figures/Top20AA1.png",
    width = 736, height = 481)
#CONSTANT HEIGHT,AA1
par("mar"=c(7,4,2,5)+.1)
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
       inset=-.25, bty="o", xpd=NA, cex=.75, ncol=2)
dev.off()


png(file = "/Users/andrewwallace/DissertationPublications/ClusterAnalysis/Analysis/Figures/Top20AA2.png",
    width = 736, height = 481)
#CONSTANT HEIGHT,AA2
par("mar"=c(7,4,2,5)+.1)
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
       inset=-.25, bty="o", xpd=NA, cex=.75, ncol=2)

dev.off()

