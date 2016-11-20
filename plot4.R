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

# reset par
dev.off()

# set up 2 row, 2 cols for plots
# set margins
par(mfrow = c(2,2), mar = c(4,4,4,1), oma = c(0.5,0.5,0.5,0.5))

# top-left
# same as plot 2
with(validData, plot(dateTime, Global_active_power, 
                     type="l", lty="solid", 
                     xlab="", ylab="Global Active Power"))

# top-right
with(validData, plot(dateTime, Voltage, 
                     type="l", lty="solid", 
                     xlab="datetime", ylab="Voltage"))

# bottom-left
# same as plot 3
with(validData, plot(dateTime, Sub_metering_1, type="l", lty="solid", 
                     xlab="", ylab="Energy sub metering"))
with(validData, lines(dateTime, Sub_metering_2, col="red"))
with(validData, lines(dateTime, Sub_metering_3, col="blue"))
legend("topright", lwd=c(1,1), col=c("black", "red", "blue"), 
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
       bty="n", xjust=1)

# bottom-right
with(validData, plot(dateTime, Global_reactive_power, 
                     type="l", lty="solid", 
                     xlab="datetime", ylab="Global_reactive_power"))

# copy to png
dev.copy(png, file="plot4.png")

# close device handle
dev.off()

