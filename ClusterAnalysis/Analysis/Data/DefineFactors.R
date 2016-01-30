# Define factor variables
# Save Data sets

r.ftpt.list <- grep("^r.ftpt",colnames(PTC))
PTC$pattern <- apply(PTC[,r.ftpt.list ],1, paste, collapse = "")


PTC$ethnicity.imputed.code2 <- factor(PTC$ethnicity.imputed.code, labels = c("White", "Black", "Hispanic", "Asian"))
PTC$degree.pursued.level.code <- factor(PTC$degree.pursued.level.code, levels = c(3,2))

PTC$deg.band2 <- factor(PTC$deg.band, levels = c(3,4,1,2), labels = c("BA1", "BA2", "AA1", "AA2"))
PTC$female2 <- factor(PTC$female, labels = c("Male", "Female"))
PTC$dependent2 <- factor(PTC$dependent, labels = c("Independent", "Dependent"))
PTC$pell2 <- factor(PTC$pell, labels = c("Not Pell Recipient", "Pell Recipient"))
PTC$nodelay2 <- factor(PTC$nodelay, labels = c("Delayed Entry", "No Delay in Entry"))
