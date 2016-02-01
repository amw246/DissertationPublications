# Cluster the sequences using weighted data

lablist<-c("Full-time","Part-time","Stop-out","Transfer", "Certificate",
           "Associate","Baccalaureate","Not enrolled post-grad")
state.list.agg <- grep("^state.sem",colnames(uniquePTCBA1))
# Create state sequence objects on weighted data sets
PTCBA1.seq<-seqdef(uniquePTCBA1, weights = aggPTCBA1$aggWeights, 
                   labels=lablist, var = state.list.agg)
PTCBA2.seq<-seqdef(uniquePTCBA2, weights = aggPTCBA2$aggWeights,
                   labels=lablist, var = state.list.agg)
PTCAA1.seq<-seqdef(uniquePTCAA1,weights = aggPTCAA1$aggWeights,
                   labels=lablist, var = state.list.agg)
PTCAA2.seq<-seqdef(uniquePTCAA2,weights = aggPTCAA2$aggWeights,
                   labels=lablist, var = state.list.agg)

# The following chunks contain five steps 
# 1. Calculate an optimal matching distance matrix
# 2. Create an hclust object using Ward's algorithm
# 3. Create a clustrange object to compare up to 20 clustering solutions
# 4. Create a summary object of the comparison
# 5. Print the results


# BA1

PTCBA1.om <- seqdist(PTCBA1.seq, method = "OM", indel = 1, sm = scostBA1,
                     full.matrix=FALSE)


WardClustBA1 <- hclust(as.dist(PTCBA1.om), method = "ward.D")
                        ,members = aggPTCBA1$aggWeights)
ptm <- proc.time()
WardClustQualBA1.20 <- as.clustrange(WardClustBA1, PTCBA1.om, 
                                     weights = aggPTCBA1$aggWeights,
                                     ncluster = 20)
print(proc.time() - ptm)

BA1ClustQual.20<-summary(WardClustQualBA1.20, max.rank = 2)
xtable(BA1ClustQual.20)

# BA1
PTCBA2.om <- seqdist(PTCBA2.seq, method = "OM", indel = 1, sm = scostBA2,
                     full.matrix=FALSE)
WardClustBA2 <- hclust(as.dist(PTCBA2.om), method = "ward", 
                       members = aggPTCBA2$aggWeights)
WardClustQualBA2.20 <- as.clustrange(WardClustBA2, PTCBA2.om, 
                                     weights = aggPTCBA2$aggWeights, ncluster = 20)
BA2ClustQual.20<-summary(WardClustQualBA2.20, max.rank = 2)
xtable(BA2ClustQual.20)

# AA1
PTCAA1.om <- seqdist(PTCAA1.seq, method = "OM", indel = 1, sm = scostAA1,
                     full.matrix=FALSE)
WardClustAA1 <- hclust(as.dist(PTCAA1.om), method = "ward", 
                       members = aggPTCAA1$aggWeights)
WardClustQualAA1.20 <- as.clustrange(WardClustAA1, PTCAA1.om, 
                                     weights = aggPTCAA1$aggWeights, ncluster = 20)
AA1ClustQual.20<-summary(WardClustQualAA1.20, max.rank = 2)
xtable(AA1ClustQual.20)

# AA2
PTCAA2.om <- seqdist(PTCAA2.seq, method = "OM", indel = 1, sm = scostAA2,
                     full.matrix=FALSE)
WardClustAA2 <- hclust(as.dist(PTCAA2.om), method = "ward", 
                       members = aggPTCAA2$aggWeights)
WardClustQualAA2.20 <- as.clustrange(WardClustAA2, PTCAA2.om, 
                                     weights = aggPTCAA2$aggWeights, ncluster = 20)
AA2ClustQual.20<-summary(WardClustQualAA2.20, max.rank = 2)
xtable(AA2ClustQual.20)