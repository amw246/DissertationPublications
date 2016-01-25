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
