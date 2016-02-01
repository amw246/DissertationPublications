# Define factor variables
# Save Data sets



r.ftpt.list <- grep("^r.ftpt",colnames(PTC))
PTC$pattern <- apply(PTC[,r.ftpt.list ],1, paste, collapse = "")


# Flag spring entrants
spring.terms <- as.Date(c('2000-02-01', '2001-02-01', '2002-02-01'), "%Y-%m-%d")

PTC$spring <- ifelse(PTC$entry.date %in% spring.terms, c("Spring"), c("Fall"))
PTC$spring <- factor(PTC$spring, levels = c("Fall", "Spring"))
table(PTC$spring)

# Flag pell recipients
PTC$pell.awd.fy01[is.na(PTC$pell.awd.fy01)] <- 0 # recode Null to 0
PTC$pell <- ifelse(PTC$pell.awd.fy01 > 0, c("Pell"), c("No Pell"))
PTC$pell <- factor(PTC$pell, levels = c("No Pell", "Pell"))
table(PTC$pell)

PTC$ethnicity.imputed.code2 <- factor(PTC$ethnicity.imputed.code, labels = c("White", "Black", "Hispanic", "Asian"))
PTC$degree.pursued.level.code <- factor(PTC$degree.pursued.level.code, levels = c(3,2))

PTC$deg.band2 <- factor(PTC$deg.band, levels = c(3,4,1,2), labels = c("BA1", "BA2", "AA1", "AA2"))
PTC$female2 <- factor(PTC$female, labels = c("Male", "Female"))
PTC$dependent2 <- factor(PTC$dependent, labels = c("Independent", "Dependent"))
PTC$pell2 <- factor(PTC$pell, labels = c("Not Pell Recipient", "Pell Recipient"))
PTC$nodelay2 <- factor(PTC$nodelay, labels = c("Delayed Entry", "No Delay in Entry"))
