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
            "grad.sem18","grad.sem19","grad.sem20","college.id.semR2", 
            "college.id.semR3","college.id.semR4","college.id.semR5",
            "college.id.semR6","college.id.semR7","college.id.semR8",
            "college.id.semR9","college.id.semR10","college.id.semR11",
            "college.id.semR12","college.id.semR13","college.id.semR14",
            "college.id.semR15","college.id.semR17","college.id.semR18",
            "college.id.semR19","college.id.semR20"
            
          )
PTC1 <- PTC1[myvars]

PTC2 <- read.dta("/Volumes/untitled/20131125_Data_Wallace_Dissertation/20131125_Wallace_f00-s02.dta", 
               convert.factors=FALSE, convert.underscore=TRUE)
PTC2 <- PTC2[myvars]

PTC3 <- read.dta("/Volumes/untitled/20131125_Data_Wallace_Dissertation/20131125_Wallace_f02-s04.dta", 
               convert.factors=FALSE, convert.underscore=TRUE)
PTC3 <- PTC3[myvars]
table(PTC3$entry.date)
# 2002-09-01 2003-02-01 2003-09-01 2004-02-01 
# 26478       8768      27135       9020 

#For dropping the later terms
PTC3 <- PTC3[ which(PTC3$entry.date==as.Date("2002-09-01") ), ]
table(PTC3$entry.date)
# 2002-09-01 
# 26478 

PTC <- rbind(PTC1, PTC2, PTC3)

DEGS <- read.dta("/Volumes/untitled/20131204_Degree_by_SEM.dta", 
                 convert.factors=FALSE, convert.underscore=TRUE)
PTC <- merge(PTC,DEGS,by="oira.student.id")

#until I can figure out how to recode ftpt below, let's use the data from Stata

FTPT <- read.dta("/Volumes/untitled/20151122_ftpt_recode.dta",
                  convert.factors = FALSE, convert.underscore = TRUE)

PTC <- merge(PTC,FTPT,by="oira.student.id")

#the number of ft students (and pt) drops from the regular to the recoded ftpt 
# variable for sem 11 (and possibly others). Let's explore these people to see if it's 
# reasonable for them to leave this category. 




x <- PTC[,which(colnames(PTC)=="r.ftptcode.sem1": which(colnames(PTC)=="r.ftptcode.sem1"]



##Need to figure out how to iterate through these variables as I do in Stata. 
#Something like the lapply below may be the trick. But it makes each A1 A2 var
#once. I need something that will go back over them an replace ONLY the appropriate
#cases. 


ftpt.list <- names(PTC)[which(colnames(PTC)=="ftptcode.sem01"):
                          which(colnames(PTC)=="ftptcode.sem20") ]
# r.ftpt.list <- paste("r", ftpt.list, sep = ".")
r.ftpt.list <- names(PTC)[which(colnames(PTC)=="r.ftptcode.sem1"):
  which(colnames(PTC)=="r.ftptcode.sem20")] 
grad.list <- names(PTC)[which(colnames(PTC)=="grad.sem01"):
                          which(colnames(PTC)=="grad.sem20")]

############THe numbers in table(PTC$degree.earned.level.code.sem2) aren't what i
#get in Stata

PTC$degree.earned.level.code.sem1 <- NA
#unfortunately there are the desc versions interspersed so a simple 2:20 doesn't work
deg.list <- names(PTC)[c(which(colnames(PTC)=="degree.earned.level.code.sem1"),
                         which(colnames(PTC)=="degree.earned.level.code.sem2"),
                         which(colnames(PTC)=="degree.earned.level.code.sem3"),
                         which(colnames(PTC)=="degree.earned.level.code.sem4"),
                         which(colnames(PTC)=="degree.earned.level.code.sem5"),
                         which(colnames(PTC)=="degree.earned.level.code.sem6"),
                         which(colnames(PTC)=="degree.earned.level.code.sem7"),
                         which(colnames(PTC)=="degree.earned.level.code.sem8"),
                         which(colnames(PTC)=="degree.earned.level.code.sem9"),
                         which(colnames(PTC)=="degree.earned.level.code.sem10"),
                         which(colnames(PTC)=="degree.earned.level.code.sem11"),
                         which(colnames(PTC)=="degree.earned.level.code.sem12"),
                         which(colnames(PTC)=="degree.earned.level.code.sem13"),
                         which(colnames(PTC)=="degree.earned.level.code.sem14"),
                         which(colnames(PTC)=="degree.earned.level.code.sem15"),
                         which(colnames(PTC)=="degree.earned.level.code.sem16"),
                         which(colnames(PTC)=="degree.earned.level.code.sem17"),
                         which(colnames(PTC)=="degree.earned.level.code.sem18"),
                         which(colnames(PTC)=="degree.earned.level.code.sem19"),
                          which(colnames(PTC)=="degree.earned.level.code.sem20"))]


#I didnt' create a 1st sem version of this in Stata but for the function below, I
#will need it. 
PTC$college.id.semR1 <- PTC$college.id
college.list <- names(PTC)[c(which(colnames(PTC)=="college.id.semR1"),
                             which(colnames(PTC)=="college.id.semR2"):
                               which(colnames(PTC)=="college.id.semR20"))]

#Create a list of tuples of the two variables to be used in the conditions
tup.list <- mapply(list, ftpt.list, grad.list, SIMPLIFY=FALSE)

# w = college.list, x = ftpt.list, y = grad.list, z = deg.list, a = r.ftpt.list
state.recoding<-  function(w,x,y,z){
  if(x == 1 & y == 0) { #full-time
    return(1)
  }else if(x == 2 & y == 0) { #part-time
    return(2)
  }else  if(z == 1) { #Cert
    return(5)
  }else  if(z == 2) { #AA
    return(6)
  }else  if(z == 3) { #BA
    return(7)
  }else  if(w > 30) { #Trans
    return(4)  
  }else   return(3) #stop out
}

test.function <- function(x) {
  table(x)
}
attach(PTC)
sapply(PTC[ftpt.list], test.function)




PTC[r.ftpt.list] <- mapply(state.recoding,
       college.list, ftpt.list, grad.list, deg.list)



#still need a way to back fill transfer and not enrolled post grad

x1 <- 0:2
x2 <- c(1, 11, 111)

mapply(function(x, y)
print(paste(x,y,sep=" "))
,
ftpt.list, grad.list)

PTC[r.ftpt.list] <- 
  lapply(PTC[tup.list]), 
    function(x,y) {
      if(x==1 & y==0){
        
      }
      
    }
    , x = tup.list[1], y = tup.list[2]
   

for(i in 1:20){
  r.ftptcode.semi <-3 
  
  A <- lapply (1 : 10, function (x) d + rnorm (3)) ?
  
}

}

# foreach i of numlist 2/20 {
# replace r_ftptcode_sem`i'=4 if college_id_semR`i'>30 & college_id_semR`i'!=. //t
# }
# 
# foreach i of numlist 20/3 {
# local j = `i' - 1
#   while `j' >=2 {
# 		replace r_ftptcode_sem`i' = 8 if  r_ftptcode_sem`i'== 3 & inlist(r_ftptcode_sem`j',5, 6,7) //post-grad
# 		replace r_ftptcode_sem`i' = 4 if  r_ftptcode_sem`i'== 3 & r_ftptcode_sem`j'==4 & transferred_out==1 //transfer
# 		local j = `j' - 1
# }
# }
#   
# }



#Drop the Cert students
PTC <- PTC[ which(PTC$degree.pursued.level.code > 1), ]


#Need to calculate these:   
# "ever.transferred","transferred.out",,"spring","pell"
# spring                  
# pell                
# pattern                        
# last.degree.pursued                  
# trans.last.college.level 
# last.deg.pursued
# last.deg.pursued.R              
# group     

#create the pattern variable (don't know if still necessary)
PTC$pattern <- apply(PTC[,r.ftpt.list],1, paste, collapse = "")


#descriptives

eth <- table(PTC$ethnicity.imputed.code)
gender <- table(PTC$female)
college <- table(PTC$college.id)
term <- table(entry.date)
