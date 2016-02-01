# Syntax to create the state sequence objects needed for TraMineR


library(TraMineR)
state.list <- grep("^state.sem",colnames(PTC))
lablist<-c("Full-time","Part-time","Stop-out","Transfer", "Certificate",
           "Associate","Baccalaureate","Not enrolled post-grad")
codelist<-c("f","p","s","t","C","A","B","+")

seqDataBA1 <- seqdef(PTC.BA1, var= state.list,labels=lablist)
seqDataBA2 <- seqdef(PTC.BA2, var= state.list,labels=lablist)
seqDataAA1 <- seqdef(PTC.AA1, var= state.list,labels=lablist)
seqDataAA2 <- seqdef(PTC.AA2, var= state.list,labels=lablist)

save(seqDataBA1, file = "/Volumes/untitled/PTC.BA1.Seq.RData")
save(seqDataBA2, file = "/Volumes/untitled/PTC.BA2.Seq.RData")
save(seqDataAA1, file = "/Volumes/untitled/PTC.AA1.Seq.RData")
save(seqDataAA2, file = "/Volumes/untitled/PTC.AA2.Seq.RData")




