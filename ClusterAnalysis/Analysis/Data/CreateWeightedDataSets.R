# Create Weighted Data Sets

var.list = c(1, grep("^state.sem",colnames(PTC)))

library(WeightedCluster)
aggPTCBA1 <- wcAggregateCases(PTC.BA1[,var.list])
print(aggPTCBA1)
uniquePTCBA1<-PTC.BA1[aggPTCBA1$aggIndex,var.list]

aggPTCBA2 <- wcAggregateCases(PTC.BA2[,var.list])
print(aggPTCBA2)
uniquePTCBA2<-PTC.BA2[aggPTCBA2$aggIndex,var.list]

aggPTCAA1 <- wcAggregateCases(PTC.AA1[,var.list])
print(aggPTCAA1)
uniquePTCAA1<-PTC.AA1[aggPTCAA1$aggIndex,var.list]

aggPTCAA2 <- wcAggregateCases(PTC.AA2[,var.list])
print(aggPTCAA2)
uniquePTCAA2<-PTC.AA2[aggPTCAA2$aggIndex,var.list]