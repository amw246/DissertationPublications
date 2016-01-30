# Syntax to calculate the state of a student by semester

PTC$degree.earned.level.code.sem1 <- 0

deg.list <- grep("degree.earned.level.code", colnames(PTC))
deg.list <- c(deg.list[length(deg.list)],deg.list[-length(deg.list)])

PTC$college.id.semR1 <- PTC$college.id
col.list <- c(which(colnames(PTC)=="college.id.semR1"),which(colnames(PTC)=="college.id.semR2"):
                which(colnames(PTC)=="college.id.semR20"))

ftpt.list <- grep("^ftptcode.sem", colnames(PTC))

grad.list <- grep("grad.sem", colnames(PTC))

#recode NAs to 0 (state.recoding fails when there are NAs in the ifs)
PTC[deg.list][is.na(PTC[deg.list])] <- 0
PTC[col.list][is.na(PTC[col.list])] <- 0
PTC[ftpt.list][is.na(PTC[ftpt.list])] <- 0


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

#retroactively recode non-enrolled post grad and transfers
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

#test that the difference between the recodes from Stata and the recodes from R is 0
state.recode.test.list <- grep("^state.recode.test",colnames(PTC))
lapply(PTC[,state.recode.test.list], table)
#Success!