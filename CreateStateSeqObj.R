# Syntax to create the state sequence objects needed for TraMineR


library(TraMineR)

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
