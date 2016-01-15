#This file will read in the data files from the institution,
#conduct appropriate data transformation, and lay the groundwork
#for the analysis to come. 


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

PTC$degree.earned.level.code.sem1 <- 0

deg.list <- grep("degree.earned.level.code", colnames(PTC))
deg.list <- c(deg.list[length(deg.list)],deg.list[-length(deg.list)])

PTC$college.id.semR1 <- PTC$college.id
col.list <- c(which(colnames(PTC)=="college.id.semR1"),which(colnames(PTC)=="college.id.semR2"):
                which(colnames(PTC)=="college.id.semR20"))

ftpt.list <- grep("^ftptcode.sem", colnames(PTC))

grad.list <- grep("grad.sem", colnames(PTC))

# 
# apply(PTC[ftpt.list], 2, null.recode)

PTC$degree.earned.level.code.sem2[is.na(PTC$degree.earned.level.code.sem2)] <- 0
PTC$ftptcode.sem02[is.na(PTC$ftptcode.sem02)] <- 0 
PTC$college.id.semR2[is.na(PTC$college.id.semR2)] <- 0

PTC$degree.earned.level.code.sem3[is.na(PTC$degree.earned.level.code.sem3)] <- 0
PTC$ftptcode.sem03[is.na(PTC$ftptcode.sem03)] <- 0 
PTC$college.id.semR3[is.na(PTC$college.id.semR3)] <- 0

PTC$degree.earned.level.code.sem4[is.na(PTC$degree.earned.level.code.sem4)] <- 0
PTC$ftptcode.sem04[is.na(PTC$ftptcode.sem04)] <- 0 
PTC$college.id.semR4[is.na(PTC$college.id.semR4)] <- 0

PTC$degree.earned.level.code.sem5[is.na(PTC$degree.earned.level.code.sem5)] <- 0
PTC$ftptcode.sem05[is.na(PTC$ftptcode.sem05)] <- 0 
PTC$college.id.semR5[is.na(PTC$college.id.semR5)] <- 0

PTC$degree.earned.level.code.sem6[is.na(PTC$degree.earned.level.code.sem6)] <- 0
PTC$ftptcode.sem06[is.na(PTC$ftptcode.sem06)] <- 0 
PTC$college.id.semR6[is.na(PTC$college.id.semR6)] <- 0

PTC$degree.earned.level.code.sem7[is.na(PTC$degree.earned.level.code.sem7)] <- 0
PTC$ftptcode.sem07[is.na(PTC$ftptcode.sem07)] <- 0 
PTC$college.id.semR7[is.na(PTC$college.id.semR7)] <- 0

PTC$degree.earned.level.code.sem8[is.na(PTC$degree.earned.level.code.sem8)] <- 0
PTC$ftptcode.sem08[is.na(PTC$ftptcode.sem08)] <- 0 
PTC$college.id.semR8[is.na(PTC$college.id.semR8)] <- 0

PTC$degree.earned.level.code.sem9[is.na(PTC$degree.earned.level.code.sem9)] <- 0
PTC$ftptcode.sem09[is.na(PTC$ftptcode.sem09)] <- 0 
PTC$college.id.semR9[is.na(PTC$college.id.semR9)] <- 0

PTC$degree.earned.level.code.sem10[is.na(PTC$degree.earned.level.code.sem10)] <- 0
PTC$ftptcode.sem10[is.na(PTC$ftptcode.sem10)] <- 0 
PTC$college.id.semR10[is.na(PTC$college.id.semR10)] <- 0

PTC$degree.earned.level.code.sem11[is.na(PTC$degree.earned.level.code.sem11)] <- 0
PTC$ftptcode.sem11[is.na(PTC$ftptcode.sem11)] <- 0 
PTC$college.id.semR11[is.na(PTC$college.id.semR11)] <- 0

PTC$degree.earned.level.code.sem12[is.na(PTC$degree.earned.level.code.sem12)] <- 0
PTC$ftptcode.sem12[is.na(PTC$ftptcode.sem12)] <- 0 
PTC$college.id.semR12[is.na(PTC$college.id.semR12)] <- 0

PTC$degree.earned.level.code.sem13[is.na(PTC$degree.earned.level.code.sem13)] <- 0
PTC$ftptcode.sem13[is.na(PTC$ftptcode.sem13)] <- 0 
PTC$college.id.semR13[is.na(PTC$college.id.semR13)] <- 0

PTC$degree.earned.level.code.sem14[is.na(PTC$degree.earned.level.code.sem14)] <- 0
PTC$ftptcode.sem14[is.na(PTC$ftptcode.sem14)] <- 0 
PTC$college.id.semR14[is.na(PTC$college.id.semR14)] <- 0

PTC$degree.earned.level.code.sem15[is.na(PTC$degree.earned.level.code.sem15)] <- 0
PTC$ftptcode.sem15[is.na(PTC$ftptcode.sem15)] <- 0 
PTC$college.id.semR15[is.na(PTC$college.id.semR15)] <- 0

PTC$degree.earned.level.code.sem16[is.na(PTC$degree.earned.level.code.sem16)] <- 0
PTC$ftptcode.sem16[is.na(PTC$ftptcode.sem16)] <- 0 
PTC$college.id.semR16[is.na(PTC$college.id.semR16)] <- 0

PTC$degree.earned.level.code.sem17[is.na(PTC$degree.earned.level.code.sem17)] <- 0
PTC$ftptcode.sem17[is.na(PTC$ftptcode.sem17)] <- 0 
PTC$college.id.semR17[is.na(PTC$college.id.semR17)] <- 0

PTC$degree.earned.level.code.sem18[is.na(PTC$degree.earned.level.code.sem18)] <- 0
PTC$ftptcode.sem18[is.na(PTC$ftptcode.sem18)] <- 0 
PTC$college.id.semR18[is.na(PTC$college.id.semR18)] <- 0

PTC$degree.earned.level.code.sem19[is.na(PTC$degree.earned.level.code.sem19)] <- 0
PTC$ftptcode.sem19[is.na(PTC$ftptcode.sem19)] <- 0 
PTC$college.id.semR19[is.na(PTC$college.id.semR19)] <- 0

PTC$degree.earned.level.code.sem20[is.na(PTC$degree.earned.level.code.sem20)] <- 0
PTC$ftptcode.sem20[is.na(PTC$ftptcode.sem20)] <- 0 
PTC$college.id.semR20[is.na(PTC$college.id.semR20)] <- 0


# null.recode <- function(x){
#   PTC$x[is.na(PTC$x)] <- 0
# }
# null.recode(degree.earned.level.code.sem2

state.recoding <- function(deg, ftpt, grad, college){
  if      (deg == 1) {
  return(5)
} else if (deg == 2) {
  return(6)
} else if (deg == 3) {
  return(7)
} else if (ftpt == 1 && grad == 0){
  return(1)
} else if (ftpt == 2 && grad == 0){
  return(2)
} else if (college > 30){
  return(4)
} else {
  return(3)
}
}

for (place in 1:length(deg.list)){
  PTC[,ncol(PTC)+1] <- mapply(state.recoding, 
                              deg = PTC[,deg.list[place]], 
                              ftpt = PTC[,ftpt.list[place]], 
                              grad = PTC[,grad.list[place]],
                              college = PTC[,col.list[place]])
  colnames(PTC) <- c(colnames(PTC)[-ncol(PTC)],paste0("state.sem",place))
}

state.list <- grep("^state.sem",colnames(PTC))

for (sem in 20:3){
  prev.sem = sem - 1
  while (prev.sem >= 2){
        PTC[,state.list[sem]] <- ifelse(
          (PTC[,state.list[sem]] == 3 & PTC[,state.list[prev.sem]] %in% c(5,6,7)), 
          8,PTC[,state.list[sem]]
        )
        PTC[,state.list[sem]] <- ifelse(
          (PTC[,state.list[sem]]== 3 & PTC[,state.list[prev.sem]] == 4 & PTC$transferred.out==1), 
          4,PTC[,state.list[sem]]
        )
        prev.sem = prev.sem - 1
  }
}
# View(PTC[which(PTC$ftpt20.recode.diff== TRUE), 179:220])

diff.test <- function(var1, var2){return(var1 - var2)}
r.ftpt.list <- grep("^r.ftpt",colnames(PTC))

for (place in 1:length(deg.list)){
  PTC[,ncol(PTC)+1] <- mapply(diff.test, 
                              var1 = PTC[,state.list[place]], 
                              var2 = PTC[,state.list[place]])
  colnames(PTC) <- c(colnames(PTC)[-ncol(PTC)],paste0("state.recode.test",place))
}

state.recode.test.list <- grep("^state.recode.test",colnames(PTC))
lapply(PTC[,state.recode.test.list], table)


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

# Find the last degree pursued
#the 1st sem variable is differently named and in a different location than the others
deg.pursued1 <- names(PTC)[which(colnames(PTC)=="degree.pursued.level.code")] 
deg.pursued2 <- names(PTC)[which(colnames(PTC)=="degree.pursued.lvl.code.sem02"):
                            which(colnames(PTC)=="degree.pursued.lvl.code.sem20")]
deg.pursued <- c(deg.pursued1, deg.pursued2)


lastValue <- function(x) tail(x[!is.na(x)],1)
PTC$last.deg.pursued <- apply(PTC[deg.pursued], 1, lastValue)

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

# 1     2     3 
# 146 34104 24451 

# The inclusion of certificates isn't surprising given they haven't been dropped yet
# however these numbers are bigger than those in Stata which I think exclude non-trans
# students. Do more work to reconcile these numbers. 

PTC <- merge(PTC, NSC, all.x = TRUE)




#Drop the Cert students
PTC <- PTC[ which(PTC$degree.pursued.level.code > 1), ]


#Need to calculate these:                                        
# trans.last.college.level  to get to this I went back to the NSC data, hmm 
# last.deg.pursued.R              
# group     
#delayed.grad

#create the pattern variable (don't know if still necessary)
r.ftpt.list <- names(PTC)[which(colnames(PTC)=="r.ftptcode.sem1"):
                            which(colnames(PTC)=="r.ftptcode.sem20")] 
PTC$pattern <- apply(PTC[,r.ftpt.list],1, paste, collapse = "")


#descriptives
PTC$ethnicity.imputed.code <- factor(PTC$ethnicity.imputed.code)
PTC$degree.pursued.level.code <- factor(PTC$degree.pursued.level.code, levels = c(3,2))
#set the appropriate comparison group for degree

PTC$female <- factor(PTC$female)

eth <- table(PTC$ethnicity.imputed.code)
gender <- table(PTC$female)
college <- table(PTC$college.id)
term <- table(entry.date)
deg <- table(PTC$degree.pursued.level.code)

#a quick test of probits
#ba.150 was read in as an integer
#convert it to a factor
PTC$ba.150 <- factor(PTC$ba.150)

myprobit <- glm(ba.150 ~ ethnicity.imputed.code + degree.pursued.level.code + female, family=binomial(link="probit"), data=PTC)

## model summary
summary(myprobit)

#well, it runs. 
a = matrix(c(2, 3, NA,1, NA, 1,2, 12, 19), nrow = 3, ncol = 3, byrow = TRUE)
df = data.frame(a) 
lastValue <- function(x) tail(x[!is.na(x)],1)

