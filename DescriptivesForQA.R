library(foreign)
# PTC1<-read.dta("/Volumes/Untitled/20131125_Wallace_f99-s00.dta", 
#                  convert.factors=FALSE, convert.underscore=TRUE)
# 
# 
# library(foreign)
# PTC2<-read.dta("/Volumes/Untitled/20131125_Wallace_f00-s02.dta", 
#                convert.factors=FALSE, convert.underscore=TRUE)
# PTC3<-read.dta("/Volumes/Untitled/20131125_Wallace_f02-s04.dta", 
#                convert.factors=FALSE, convert.underscore=TRUE)

# I keep getting an error :
# Error in read.dta("/Volumes/Untitled/20131125_Wallace_f02-s04.dta", 
#                   convert.factors = FALSE,  : 
#                     a binary read error occurred
# I'll explore this later

PTC <- read.dta("/Volumes/Untitled/20131203_PTC.dta", 
                convert.factors=FALSE, convert.underscore=TRUE)

table(PTC$term.enrolled.date)
# 2/1/2000 2/1/2001 2/1/2002 9/1/1999 9/1/2000 9/1/2001 9/1/2002 
# 8589     8352     9331    23866    24724    25039    26271 

#I'm having a lot of trouble locating the syntax that took the 126k of the 20131203
#file and brought it to the final 125k. I need this also because I don't have the 
#syntax that created the degree bands. I'll look at work. BY 20140922 there were
#definitely degree bands. We also definitely had it by 20140304. The 20140306 
#descriptives file has degband but calculates degtotal which is also needed. 
#The problem is it does not include a command that opens a data set, much less 
# generates DegBand. 

#Hmm. The 20140307_Descriptives_by_Outcome.do.do file seems to be a record of 
# commands from a Stata session. It involves insheeting csv files. This might indicate
# that something was done in R if csv was needed. It creates the 
# 20140304_PTC_Cluster_All.dta data set. Did I do the degband calculation in R? Check 
# files around feb/may 2014

#That said, looking at the 20140310_Descriptives_by_Outcomes do file, I can see that
# it includes the cert final degband (4) that I need to drop. It has the 126,172 
# records. 



