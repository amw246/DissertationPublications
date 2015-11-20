library(foreign)
# PTC1<-read.dta("/Volumes/Untitled/20131125_Wallace_f99-s00.dta", 
#                  convert.factors=FALSE, convert.underscore=TRUE)
# 
# 
# library(foreign)
PTC2<-read.dta("O:\\!Policy\\Projects\\Policy Tracking Cohort\\Public Use\\Wallace Dissertation\\20131125_Wallace_f00-s02.dta", 
               convert.factors=FALSE, convert.underscore=TRUE)
# PTC3<-read.dta("/Volumes/Untitled/20131125_Wallace_f02-s04.dta", 
#                convert.factors=FALSE, convert.underscore=TRUE)

# I keep getting an error :
# Error in read.dta("/Volumes/Untitled/20131125_Wallace_f02-s04.dta", 
#                   convert.factors = FALSE,  : 
#                     a binary read error occurred
# I'll explore this later

# PTC <- read.dta("/Volumes/Untitled/20131203_PTC.dta", 
#                 convert.factors=FALSE, convert.underscore=TRUE)

PTC <- read.dta("C:\\Users\\awallace\\Desktop\\Andrew's Files\\Dissertation\\20151120_PTC_Stata9.dta", 
                convert.factors=FALSE, convert.underscore=TRUE)

table(PTC$entry.date)
# 1999-09-01 2000-02-01 2000-09-01 2001-02-01 2001-09-01 2002-02-01 2002-09-01 
# 23729       8531      24611       8315      24926       9270      26133 

table(PTC$

