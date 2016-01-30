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

