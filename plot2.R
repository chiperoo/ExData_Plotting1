# the source file should be in the same directory as this script

# open a connection to the source file
con <- file("household_power_consumption.txt", "r")

# read in 100000 lines at a time
CHUNK_SIZE <- 100000

# coerce character to date
setClass("myDate")
setAs("character", "myDate", function(from) as.Date(from, format="%d/%m/%Y"))

# control variables
first <- TRUE
continue <- TRUE
data <- data.frame()

# read in the file
while(continue) {
  chunk <- read.table(con, header=first, sep=";",  
                      na.strings = "?", 
                      colClasses = c("myDate", "character", rep("numeric", 7)),
                      nrows = CHUNK_SIZE,
                      stringsAsFactors = FALSE)
  
  # append the chunk to the list of read records
  data <- rbind(data, chunk)
  
  # don't want to read the header again
  first <- FALSE
  
  # stop if we have read the data we need
  if(max(chunk$Date) > "2007-02-02") {
    continue <- FALSE
  }
}

# close the file connection
close(con)

validData <- subset(data, Date >= "2007-02-01" & Date <= "2007-02-02")

# make date-time
validData$dateTime <- as.POSIXct(paste(validData$Date, validData$Time))

# save par
old.par <- par(no.readonly = T)

# set margins
par(oma = c(0.5,0.5,0.5,0.5))

# create plot
with(validData, plot(dateTime, Global_active_power, 
                     type="l", lty="solid", 
                     xlab="", ylab="Global Active Power (kilowatts)"))

# copy to png
dev.copy(png, file="plot2.png")

# close device handle
dev.off()

# restore par
par(old.par)