rankhospital <- function(state, outcome, num) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[data$State == state, ]
  ## Check that state and outcome are valid
  if(nrow(data) <= 0) {
    stop("invalid state")
  }
  outcomes <- c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  if(!is.element(outcome, names(outcomes))) {
    stop("invalid outcome")
  }
  outcome <- outcomes[outcome]
  data <- na.omit(data.frame(outcome = as.numeric(data[[outcome]]), 
                             Hospital.Name = data$Hospital.Name))
  data <- data[order(data$outcome, data$Hospital.Name),]
  nums <- c("best" = 1, "worst" = nrow(data))
  if(is.character(num)) {
    num <- nums[num]
  }
  if(is.na(num)) {
    name <- NA
  }
  name <- as.character(data[num, "Hospital.Name"])
  name
}