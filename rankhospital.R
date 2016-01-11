library(plyr)
rankhospital <- function(state=character(),outcome=character(),num="best") {
  
  ## reading and cleaning outcome data, splitting by state
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  cleandata<-subset(data[,c(2,7,11,17,23)]) ## we only need the names, states and 30-day mortalities
  data_bystate<- split(cleandata,data[,7])
  
  ## validating of state argument
  if (!state %in% cleandata[,2]) {
    stop("invalid state")
  }
  ## validating outcome argument, and recoding outcome into a column number
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
  ##recoding num argument
  worst<-FALSE
  if (num=="best") {
    num<-1
  }
  if (num=="worst") {
    num<-1
    worst<-TRUE
  }
  
  ## ordering by lowest outcome value (and alphabetical order as tiebreaker), giving output
  rankdata<-data.frame(data_bystate[state])
  rankdata[,3:5]<-suppressWarnings(lapply(rankdata[,3:5],as.numeric)) ## converting character values to numeric to prevent errors
  rankdata<-rankdata[complete.cases(rankdata[,outcome]),]
  if (worst==TRUE) {
    rankdata<-arrange(rankdata,-rankdata[,outcome],rankdata[,1])
  }
  else {
    rankdata<-arrange(rankdata,rankdata[,outcome],rankdata[,1])
  }
  rankdata[num,1]
}
