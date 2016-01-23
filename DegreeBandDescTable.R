#descriptives of degree bands
PTC$ethnicity.imputed.code2 <- factor(PTC$ethnicity.imputed.code, labels = c("White", "Black", "Hispanic", "Asian"))
PTC$degree.pursued.level.code <- factor(PTC$degree.pursued.level.code, levels = c(3,2))

PTC$deg.band2 <- factor(PTC$deg.band, levels = c(3,4,1,2), labels = c("BA1", "BA2", "AA1", "AA2"))
#set the appropriate comparison group for degree


deg.band.desc1 <- cbind(prop.table(table(PTC$ethnicity.imputed.code2, PTC$deg.band2),2), prop.table(table(PTC$ethnicity.imputed.code2)))
colnames(deg.band.desc1) <- c("BA1", "BA2", "AA1", "AA2", "Total")
round(deg.band.desc1*100, 2)

PTC$female2 <- factor(PTC$female, labels = c("Male", "Female"))
deg.band.desc2 <- cbind(prop.table(table(PTC$female2, PTC$deg.band2),2), prop.table(table(PTC$female2)))
colnames(deg.band.desc2) <- c("BA1", "BA2", "AA1", "AA2", "Total")
round(deg.band.desc2*100, 2)

PTC$dependent2 <- factor(PTC$dependent, labels = c("Independent", "Dependent"))
deg.band.desc3 <- cbind(prop.table(table(PTC$dependent2, PTC$deg.band2),2), prop.table(table(PTC$dependent2)))
colnames(deg.band.desc3) <- c("BA1", "BA2", "AA1", "AA2", "Total")
round(deg.band.desc3*100, 2)

PTC$pell2 <- factor(PTC$pell, labels = c("Not Pell Recipient", "Pell Recipient"))
deg.band.desc4 <- cbind(prop.table(table(PTC$pell2, PTC$deg.band2),2), prop.table(table(PTC$pell2)))
colnames(deg.band.desc4) <- c("BA1", "BA2", "AA1", "AA2", "Total")
round(deg.band.desc4*100, 2)

PTC$nodelay2 <- factor(PTC$nodelay, labels = c("Delayed Entry", "No Delay in Entry"))
deg.band.desc5 <- cbind(prop.table(table(PTC$nodelay2, PTC$deg.band2),2), prop.table(table(PTC$nodelay2)))
colnames(deg.band.desc5) <- c("BA1", "BA2", "AA1", "AA2", "Total")
round(deg.band.desc5*100, 2)

#spring was already taken care of above. 
deg.band.desc6 <- cbind(prop.table(table(PTC$spring, PTC$deg.band2),2), prop.table(table(PTC$spring)))
colnames(deg.band.desc6) <- c("BA1", "BA2", "AA1", "AA2", "Total")
round(deg.band.desc6*100, 2)

#now for the continuous variables
deg.band.desc7 <- rbind(aggregate(PTC$entry.age, by=list(PTC$deg.band2), FUN=mean, na.rm = TRUE)[2], mean(PTC$entry.age, na.rm = TRUE))
colnames(deg.band.desc7) <- "Age at Entry"
rownames(deg.band.desc7) <- c("BA1", "BA2", "AA1", "AA2", "Total")

deg.band.desc8 <- rbind(aggregate(PTC$cpi.units.total, by=list(PTC$deg.band2), FUN=mean, na.rm = TRUE)[2], 
                        mean(PTC$cpi.units.total, na.rm = TRUE))
colnames(deg.band.desc8) <- "College Prep Units"
rownames(deg.band.desc8) <- c("BA1", "BA2", "AA1", "AA2", "Total")

deg.band.desc9 <- rbind(aggregate(PTC$caa.total, by=list(PTC$deg.band2), FUN=mean, na.rm = TRUE)[2], mean(PTC$caa.total, na.rm = TRUE))
colnames(deg.band.desc9) <- "HS GPA"
rownames(deg.band.desc9) <- c("BA1", "BA2", "AA1", "AA2", "Total")

deg.band.desc10 <- rbind(aggregate(PTC$cas.sat.total.recntrd, by=list(PTC$deg.band2), FUN=mean, na.rm = TRUE)[2], 
                         mean(PTC$cas.sat.total.recntrd, na.rm = TRUE))
colnames(deg.band.desc10) <- "SAT Total before Transformation"
rownames(deg.band.desc10) <- c("BA1", "BA2", "AA1", "AA2", "Total")

deg.band.desc11 <- rbind(aggregate(PTC$crdsem01, by=list(PTC$deg.band2), FUN=mean, na.rm = TRUE)[2], 
                         mean(PTC$crdsem01, na.rm = TRUE))
colnames(deg.band.desc11) <- "First Sem. Credits"
rownames(deg.band.desc11) <- c("BA1", "BA2", "AA1", "AA2", "Total")

deg.band.desc.all <- rbind(deg.band.desc1*100, deg.band.desc2*100, deg.band.desc3*100, 
                           deg.band.desc4*100, deg.band.desc5*100, deg.band.desc6*100,  
                           t(deg.band.desc7), t(deg.band.desc8), t(deg.band.desc9),
                           t(deg.band.desc10), t(deg.band.desc11))
print(xtable(round(deg.band.desc.all,2)), file = "/Users/andrewwallace/DissertationPublications/deg.band.desc.table.txt")
save(deg.band.desc.table, file = "/Users/andrewwallace/DissertationPublications/deg.band.desc.table.rda")
