pollutantmean <- function(directory, pollutant, id = 1:332) {
  vLen <- 0
  vSum <- 0
  fileNames <- list.files(path =directory, full.names = TRUE)[id]
  for (fileName in fileNames)  {                
    dataFile <- read.csv(fileName)
    x <- dataFile[pollutant][which(!is.na(dataFile[pollutant])),]
    vLen <- vLen + length(x)
    vSum <- vSum + sum(x)
  }
  return(sprintf("%05.3f", vSum/vLen))
}