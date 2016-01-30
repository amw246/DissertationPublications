# Save Data sets


PTC.BA1<-  subset(PTC,deg.band==3)
PTC.BA2<-  subset(PTC,deg.band==4)
PTC.AA1<-  subset(PTC,deg.band==1)
PTC.AA2<-  subset(PTC,deg.band==2)

save(PTC, file = "/Volumes/untitled/PTC.RData")
save(PTC.BA1, file = "/Volumes/untitled/PTC.BA1.RData")
save(PTC.BA2, file = "/Volumes/untitled/PTC.BA2.RData")
save(PTC.AA1, file = "/Volumes/untitled/PTC.AA1.RData")
save(PTC.AA2, file = "/Volumes/untitled/PTC.AA2.RData")
