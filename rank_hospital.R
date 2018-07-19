rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv")
  states <- levels(data[, 7])[data[, 7]]
  state_flag <- FALSE
  for (i in 1:length(states)) {
    if (state == states[i]) {
      state_flag <- TRUE
    }
  }
  if (!state_flag) {
    stop ("invalid state")
  } 
  if (!((outcome == "heart attack") | (outcome == "heart failure")
        | (outcome == "pneumonia"))) {
    stop ("invalid outcome")
  }
  
  col <- if (outcome == "heart attack") {
    11
  } else if (outcome == "heart failure") {
    17
  } else {
    23
  }
  
  data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
  data[, 2] <- as.character(data[, 2])
  statedata <- data[grep(state, data$State), ]
  orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
  if(num == "best") {
    orderdata[1, 2]
  } else if(num == "worst") {
    orderdata[nrow(orderdata), 2]
  } else{
    orderdata[num, 2]
  }
}
