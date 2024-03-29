
# prepare downloaded Daymet data and return the Daymet dataframe
# currently adds season (could be expanded to specify number of bins), and extracts listed data

prepDaymet <- function(data_location)
{
  library(tidyr)
  daymet_data <- read.csv(data_location,sep=",")

  daymet_data <- daymet_data[,2:10]
  daymet_data <- daymet_data %>%
    nest(cols=c(measurement,value))

  daymet_data$season <- "1"
  daymet_data$season[daymet_data$yday >= 61 & daymet_data$yday < 153] <- "2"
  daymet_data$season[daymet_data$yday >= 153 & daymet_data$yday < 245] <- "3"
  daymet_data$season[daymet_data$yday >= 245 & daymet_data$yday < 336] <- "4"

  extractListedData <- function(row)
  {
      return(row[8]$cols[[2]])
  }

  extracted <- apply(daymet_data,MARGIN=1,FUN=extractListedData)
  extracted <- do.call(rbind.data.frame, extracted)
  colnames(extracted) <- c("daylength","precip","srad","swe","tmax","tmin","vp")
  daymet_data <- cbind(daymet_data,extracted)
  daymet_data$daymet_tile <- daymet_data$tile

  return(daymet_data)
}
