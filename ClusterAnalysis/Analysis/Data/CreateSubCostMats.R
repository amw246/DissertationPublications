# Create Substitution Cost Matrices
library(xtable)

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