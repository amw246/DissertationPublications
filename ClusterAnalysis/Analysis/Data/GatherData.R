# Code to load the 3 PTC files for analysis into R
# Along with the full-time status, degre earning status, and transfer data
# needed to create the final working data set


library(foreign)
PTC1 <- read.dta("/Volumes/untitled/20131125_Data_Wallace_Dissertation/20131125_Wallace_f99-s00.dta", 
                 convert.factors=FALSE, convert.underscore=TRUE)
myvars <- c("ftptcode.sem01","ftptcode.sem02","ftptcode.sem03","ftptcode.sem04",
            "ftptcode.sem05","ftptcode.sem06","ftptcode.sem07","ftptcode.sem08",
            "ftptcode.sem09","ftptcode.sem10","ftptcode.sem11","ftptcode.sem12",
            "ftptcode.sem13","ftptcode.sem14","ftptcode.sem15","ftptcode.sem16",
            "ftptcode.sem17","ftptcode.sem18","ftptcode.sem19","ftptcode.sem20",
            "oira.student.id","college.id", "entry.date","degree.pursued.level.code",
            "crdsem01","gpasem01","ethnicity.imputed.code","aa.inst.400",
            "aa.inst.300","aa.inst.200","aa.inst.150","aa.inst.100",
            "ba.inst.200","ba.inst.150","ba.inst.100","cpi.units.total", "caa.total",
            "cas.sat.total.recntrd", "aa.date", "ba.date","aa.deg.date", "ba.deg.date",
            "dependent","female","aa.100","aa.150","aa.200","aa.300","aa.400","ba.100",
            "ba.150","ba.200","nodelay","entry.age","best.math","best.writ","best.read",
            "need.any.remediation","last.college.code","grad.sem01","grad.sem02",
            "grad.sem03","grad.sem04","grad.sem05","grad.sem06","grad.sem07",
            "grad.sem08","grad.sem09","grad.sem10","grad.sem11","grad.sem12", 
            "grad.sem13","grad.sem14","grad.sem15","grad.sem16","grad.sem17",
            "grad.sem18","grad.sem19","grad.sem20",
            "college.id.semR2","college.id.semR3","college.id.semR4",
            "college.id.semR5","college.id.semR6","college.id.semR7",
            "college.id.semR8","college.id.semR9","college.id.semR10",
            "college.id.semR11","college.id.semR12","college.id.semR13",
            "college.id.semR14","college.id.semR15","college.id.semR16",
            "college.id.semR17","college.id.semR18","college.id.semR19",
            "college.id.semR20","pell.awd.fy01","ever.transferred",
            "transferred.out",
            "degree.pursued.lvl.code.sem02","degree.pursued.lvl.code.sem03",
            "degree.pursued.lvl.code.sem04","degree.pursued.lvl.code.sem05",
            "degree.pursued.lvl.code.sem06","degree.pursued.lvl.code.sem07",
            "degree.pursued.lvl.code.sem08","degree.pursued.lvl.code.sem09",
            "degree.pursued.lvl.code.sem10","degree.pursued.lvl.code.sem11",
            "degree.pursued.lvl.code.sem12","degree.pursued.lvl.code.sem13",
            "degree.pursued.lvl.code.sem14","degree.pursued.lvl.code.sem15",
            "degree.pursued.lvl.code.sem16","degree.pursued.lvl.code.sem17",
            "degree.pursued.lvl.code.sem18","degree.pursued.lvl.code.sem19",
            "degree.pursued.lvl.code.sem20"
)
PTC1 <- PTC1[myvars]

PTC2 <- read.dta("/Volumes/untitled/20131125_Data_Wallace_Dissertation/20131125_Wallace_f00-s02.dta", 
                 convert.factors=FALSE, convert.underscore=TRUE)
PTC2 <- PTC2[myvars]

PTC3 <- read.dta("/Volumes/untitled/20131125_Data_Wallace_Dissertation/20131125_Wallace_f02-s04.dta", 
                 convert.factors=FALSE, convert.underscore=TRUE)
PTC3 <- PTC3[myvars]
#table(PTC3$entry.date)
# 2002-09-01 2003-02-01 2003-09-01 2004-02-01 
# 26478       8768      27135       9020 

#For dropping the later terms
PTC3 <- PTC3[ which(PTC3$entry.date==as.Date("2002-09-01") ), ]
#table(PTC3$entry.date)
# 2002-09-01 
# 26478 

PTC <- rbind(PTC1, PTC2, PTC3)

DEGS <- read.dta("/Volumes/untitled/20131204_Degree_by_SEM.dta", 
                 convert.factors=FALSE, convert.underscore=TRUE)
PTC <- merge(PTC,DEGS,by="oira.student.id")

#Drop this once the final recoding is confirmed
FTPT <- read.dta("/Volumes/untitled/20151122_ftpt_recode.dta",
                 convert.factors = FALSE, convert.underscore = TRUE)

PTC <- merge(PTC,FTPT,by="oira.student.id")


NSC <- read.dta("/Volumes/untitled/20131120_NSC_Wide.dta",
                convert.factors = FALSE, convert.underscore = TRUE)
NSC.keeplist <- grep("year4year", colnames(NSC))
#holy crap grep is useful here! Saved a hundred lines of code
#We also want the oira id, so add column 1 to the keeplist
NSC.keeplist <- c(1, NSC.keeplist)

NSC <- NSC[NSC.keeplist]

#missingness is "" here which the lastValue function can't deal with so we recode
NSC[NSC == ""] <- NA
# there are also odd "L" values here
NSC[NSC == "L"] <- NA
#The apply in lastValue doesn't seem to like it if there is no initial value, 
# so I'm bringing in their last deg pursued from the PTC. I need to be careful not to 
# overwrite later in-CUNY changes with this . 
PTC.Level <- PTC[,c("oira.student.id", "last.deg.pursued")]
NSC <- merge(PTC.Level, NSC , by="oira.student.id")
NSC$trans.last.college.level <- apply(NSC[, 2:103], 1, lastValue)
#recode 4 (i.e. 4 year) to 3 (i.e. BA)
NSC$trans.last.college.level[NSC$trans.last.college.level == 4] <- 3
NSC$trans.last.college.level <- as.factor(NSC$trans.last.college.level)
table(NSC$trans.last.college.level)

PTC <- merge(PTC, NSC, all.x = TRUE)
