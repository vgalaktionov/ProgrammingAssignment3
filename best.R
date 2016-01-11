best <- function(state=character(),outcome=character()) {
  
  ## reading and cleaning outcome data, splitting by state
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  cleandata<-subset(data[,c(2,7,11,17,23)]) ## we only need the names, states and 30-day mortalities
  data_bystate<- split(cleandata,data[,7])
  
  ## checking for validity of state argument
  if (!state %in% cleandata[,2]) {
    stop("invalid state")
  }
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
  rankdata<-data.frame(data_bystate[state])
  rankdata[,3:5]<-suppressWarnings(lapply(rankdata[,3:5],as.numeric)) ## converting character values to numeric to prevent errors
  ranking<-order(rankdata[,outcome],rankdata[,1])
  rankdata[ranking[1],1]
}
