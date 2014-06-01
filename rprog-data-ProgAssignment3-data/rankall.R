rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check for valid outcome
  list_of_outcome <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  if (is.null(list_of_outcome[[outcome]])) { 
    stop("invalid outcome") 
  }
  
  # Get list of state 
  states <- unique(data[,7])
  indx <- order(states)
  states <- states[indx]
  
  # Subset Outcome
  this_outcome <- data[c(7,list_of_outcome[[outcome]],2)]
  
  # Cleanup for Not Available
  those_states <- this_outcome[this_outcome[2] != "Not Available",]
  
  # Convert outcome data to numeric
  colnames(those_states)[2] <- "outcome"
  trsf <- transform(those_states, outcome = as.numeric(outcome))
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  df <- data.frame(hospital=character(0),state=character(0))
  
  for (a_state in states) {
    this_state <- trsf[trsf["State"] == a_state,]
    
    if (num == "best") {
      index <- with(this_state, order(outcome, Hospital.Name))
      rowis = 1
    } else if (num == "worst") {
      index <- with(this_state, order(-outcome, Hospital.Name))
      rowis = 1
    } else {
      index <- with(this_state, order(outcome, Hospital.Name))
      rowis = num
    }
    
    ## Feed the data frame 
    new_row = data.frame(hospital = this_state[index,][rowis,3], state = a_state)
    df <- rbind(df,new_row)
  }
  
  df
}