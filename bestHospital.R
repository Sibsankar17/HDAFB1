best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  States <- levels(factor(data[, 7]))
  Outcomes <- c("heart attack", "heart failure", "pneumonia")
  if ((state %in% States) == FALSE) 
  {
    stop(print("invalid state input!"))
  }else if ((outcome %in% Outcomes) == FALSE)
  {
    stop(print("invalid outcome input!"))
  }
  colNumber <- if (outcome == "heart attack") 
  {11} else if (outcome == "heart failure") 
  {17}
  else 
  {23}
  selectedData <- subset(data, State == state)
  
  selectedColumns <- suppressWarnings(as.numeric(selectedData[,colNumber]))
  
  selectedData <- selectedData[!(is.na(selectedColumns)), ]
  
  selectedColumns <- as.numeric(selectedData[, colNumber])
  
  selectedRows <- which(selectedColumns == min(selectedColumns))
  
  selectedHospital <- selectedData[selectedRows, 2]
  
  sortedHospital <- sort(selectedHospital)
  
  return(sortedHospital)
}