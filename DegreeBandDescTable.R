#descriptives of degree bands

#set the appropriate comparison group for degree

library(plyr)
library(xtable)
deg.band.desc1 <- as.data.frame(cbind(prop.table(table(PTC$ethnicity.imputed.code2, PTC$deg.band2),2), prop.table(table(PTC$ethnicity.imputed.code2))))
colnames(deg.band.desc1) <- c("BA1", "BA2", "AA1", "AA2", "Total")

deg.band.desc2 <- as.data.frame(cbind(prop.table(table(PTC$female2, PTC$deg.band2),2), prop.table(table(PTC$female2))))
colnames(deg.band.desc2) <- c("BA1", "BA2", "AA1", "AA2", "Total")

 deg.band.desc3 <- as.data.frame(cbind(prop.table(table(PTC$dependent2, PTC$deg.band2),2), prop.table(table(PTC$dependent2))))
colnames(deg.band.desc3) <- c("BA1", "BA2", "AA1", "AA2", "Total")

 deg.band.desc4 <- as.data.frame(cbind(prop.table(table(PTC$pell2, PTC$deg.band2),2), prop.table(table(PTC$pell2))))
colnames(deg.band.desc4) <- c("BA1", "BA2", "AA1", "AA2", "Total")

deg.band.desc5 <- as.data.frame(cbind(prop.table(table(PTC$nodelay2, PTC$deg.band2),2), prop.table(table(PTC$nodelay2))))
colnames(deg.band.desc5) <- c("BA1", "BA2", "AA1", "AA2", "Total")

deg.band.desc6 <- as.data.frame(cbind(prop.table(table(PTC$spring, PTC$deg.band2),2), prop.table(table(PTC$spring))))
colnames(deg.band.desc6) <- c("BA1", "BA2", "AA1", "AA2", "Total")

deg.band.desc7 <- as.data.frame(rbind(aggregate(PTC$entry.age, by=list(PTC$deg.band2), FUN=mean, na.rm = TRUE)[2], 
                                      mean(PTC$entry.age, na.rm = TRUE)))
colnames(deg.band.desc7) <- "Age at Entry"
rownames(deg.band.desc7) <- c("BA1", "BA2", "AA1", "AA2", "Total")

deg.band.desc8 <- as.data.frame(rbind(aggregate(PTC$cpi.units.total, by=list(PTC$deg.band2), FUN=mean, na.rm = TRUE)[2], 
                        mean(PTC$cpi.units.total, na.rm = TRUE)))
colnames(deg.band.desc8) <- "College Prep Units"
rownames(deg.band.desc8) <- c("BA1", "BA2", "AA1", "AA2", "Total")

deg.band.desc9 <- as.data.frame(rbind(aggregate(PTC$caa.total, by=list(PTC$deg.band2), FUN=mean, na.rm = TRUE)[2], 
                        mean(PTC$caa.total, na.rm = TRUE)))
colnames(deg.band.desc9) <- "HS GPA"
rownames(deg.band.desc9) <- c("BA1", "BA2", "AA1", "AA2", "Total")

deg.band.desc10 <- as.data.frame(rbind(aggregate(PTC$cas.sat.total.recntrd, by=list(PTC$deg.band2), FUN=mean, na.rm = TRUE)[2], 
                          mean(PTC$cas.sat.total.recntrd, na.rm = TRUE)))
colnames(deg.band.desc10) <- "SAT Total before Transformation"
rownames(deg.band.desc10) <- c("BA1", "BA2", "AA1", "AA2", "Total")

deg.band.desc11 <- as.data.frame(rbind(aggregate(PTC$crdsem01, by=list(PTC$deg.band2), FUN=mean, na.rm = TRUE)[2], 
                         mean(PTC$crdsem01, na.rm = TRUE)))
colnames(deg.band.desc11) <- "First Sem. Credits"
rownames(deg.band.desc11) <- c("BA1", "BA2", "AA1", "AA2", "Total")

deg.band.desc12 <- as.data.frame(rbind(aggregate(PTC$gpasem01, by=list(PTC$deg.band2), FUN=mean, na.rm = TRUE)[2], 
                                       mean(PTC$gpasem01, na.rm = TRUE)))
colnames(deg.band.desc12) <- "First Sem. GPA"
rownames(deg.band.desc12) <- c("BA1", "BA2", "AA1", "AA2", "Total")

 deg.band.desc.all <- rbind(deg.band.desc1*100, deg.band.desc2*100, deg.band.desc3*100, 
                           deg.band.desc4*100, deg.band.desc5*100, deg.band.desc6*100,  
                           t(deg.band.desc7), t(deg.band.desc8), t(deg.band.desc9),
                           t(deg.band.desc10), t(deg.band.desc11), t(deg.band.desc12))

deg.band.desc.table <-   xtable(round(deg.band.desc.all,2), 
                                align = c("l", "c", "c", "c", "c", "c")
                                )
print.xtable(deg.band.desc.table, type = "latex", comment = FALSE, booktabs = TRUE)
#save(deg.band.desc.table, file = "/Users/andrewwallace/DissertationPublications/deg.band.desc.table.rda")
