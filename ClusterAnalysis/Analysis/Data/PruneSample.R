# Dropping cases to get to final N and define factor variables
# Also define deg.band



#Drop the Cert students
PTC <- PTC[ which(PTC$degree.pursued.level.code > 1), ]

PTC$last.deg.pursued.R <- PTC$last.deg.pursued
PTC$last.deg.pursued.R[PTC$trans.last.college.level == 2 
                       & PTC$transferred.out == 1] <- 2
PTC$last.deg.pursued.R[PTC$trans.last.college.level == 3 
                       & PTC$transferred.out == 1] <- 3

PTC$deg.band[PTC$degree.pursued.level.code == 2 
             & PTC$last.deg.pursued.R == 2] <- 1 #AA1
PTC$deg.band[PTC$degree.pursued.level.code == 2 
             & PTC$last.deg.pursued.R == 3] <- 2 #AA2
PTC$deg.band[PTC$degree.pursued.level.code == 3 
             & PTC$last.deg.pursued.R == 3] <- 3 #BA1
PTC$deg.band[PTC$degree.pursued.level.code == 3 
             & PTC$last.deg.pursued.R == 2] <- 4 #BA2
PTC$deg.band[PTC$degree.pursued.level.code == 2 
             & PTC$last.deg.pursued.R == 1] <- 5 #Certfinal
PTC$deg.band[PTC$degree.pursued.level.code == 3
             & PTC$last.deg.pursued.R == 1] <- 5 #Certfinal

#Drop the cert final students
PTC <- PTC[ which(PTC$deg.band < 5), ]
#Drop the native american students
PTC <- PTC[ which(PTC$ethnicity.imputed.code < 6), ]
