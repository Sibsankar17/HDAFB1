rankall <- function(outcome, num="best") {
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  
  #Get index for our given outcome string.
  index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  
  data <- read.csv("./outcome-of-care-measures.csv", colClasses="character")
  data[,index] <- suppressWarnings(as.numeric(data[,index]))
  data <- data[!is.na(data[,index]),]
  
  #Sort our data by specified mortality rate and hospital name
  data.sorted <- data[order(data[,index], data[,2], na.last=TRUE),]
  data.sorted <- data.sorted[!is.na(data.sorted[,index]),]
  
  #Parse out and validate our num
  num <- ifelse(num == "best", 1, ifelse(num == "worst", length(data.sorted), as.numeric(num)))
  
  states <- sort(unique(data.sorted[,7]))
  
  state_hospital_data <- function(state) {
    slice <- subset(data.sorted, State==state)
    slice <- slice[num, c(2,7,index)]
    slice$State <- state
    return (slice)
  }
  state_data <- lapply(states, state_hospital_data)
  dataframe <- as.data.frame(do.call(rbind, lapply(states, state_hospital_data)), row.names=states)
  colnames(dframe) <- c("hospital", "state")
  return (dataframe)
}