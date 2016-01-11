library(plyr)
rankall <- function(outcome=character(),num="best") {
  
  ## reading and cleaning outcome data, splitting by state
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  cleandata<-subset(data[,c(2,7,11,17,23)]) ## we only need the names, states and 30-day mortalities
  colnames(cleandata)<-c("hospital","state","heart attack","heart failure","pneumonia")
  data_bystate<- split(cleandata,data[,7])
  
  ## checking for validity of outcome argument, and recoding outcome into a column number
  if (outcome=="heart attack") {
    outcome<-3
  }
  else if (outcome=="heart failure") {
    outcome<-4
  }
  else if (outcome=="pneumonia") {
    outcome<-5
  }
  else  {
    stop("invalid outcome")
  }
  

  ## ordering by lowest outcome value (and alphabetical order as tiebreaker), giving output
  output<-data.frame(hospital=character(0),state=character(0))
  worst<-FALSE
  if (num=="best"){num<-1}
  if (num=="worst"){
    num<-1
    worst<-TRUE
    }
  
  for (i in 1:length(data_bystate)) {
  rankdata<-data.frame(data_bystate[i])
  rankdata[,3:5]<-suppressWarnings(lapply(rankdata[,3:5],as.numeric)) ## converting character values to numeric to prevent errors
  rankdata<-rankdata[complete.cases(rankdata[,outcome]),]
  if (worst==TRUE){
  rankdata<-arrange(rankdata,-rankdata[,outcome],rankdata[,1])
  }
  else {
    rankdata<-arrange(rankdata,rankdata[,outcome],rankdata[,1])
  }
  y<-data.frame(rankdata[num,1],rankdata[1,2])
  colnames(y)<-colnames(output)
  output<-rbind(output,y)
  }
  
  rownames(output)<-output[,2]
  output
#   debugvar<-data.frame(data_bystate$"WI")
#   debugvar[,3:5]<-suppressWarnings(lapply(debugvar[,3:5],as.numeric))
#   debugvar<-debugvar[complete.cases(debugvar[,outcome]),]
#   debugvar<-arrange(debugvar, -debugvar[,outcome])
#   debugvar
#str(outcome)
#  worst
}
