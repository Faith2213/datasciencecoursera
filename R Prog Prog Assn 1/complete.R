complete <- function(directory, id = 1:332) {
  matt <- matrix(, nrow = length(id), ncol = 2)
  for (i in 1:length(id)) {
    csv <- paste(direc,"/",sprintf("%03d",id[i]),".csv",sep="")
    temp <- read.csv(csv)
    matt[i,1] <- id[i]
    matt[i,2] <- nrow(temp[complete.cases(temp),])
  }
  df <- data.frame(matt)
  colnames(df) <- c("id", "nobs")
  df
}