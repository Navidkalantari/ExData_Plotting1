# Clean and prepare data

##Load my data
my_data <-
  read.table(
    "household_power_consumption.txt",
    header = TRUE,
    sep = ";",
    na.strings = "?",
    colClasses = c(
      'character',
      'character',
      'numeric',
      'numeric',
      'numeric',
      'numeric',
      'numeric',
      'numeric',
      'numeric'
    )
  )

## Format date to Type Date
my_data$Date <- as.Date(my_data$Date, "%d/%m/%Y")

## Filter to select data for dates  2007-2-1 and 2007-2-1
my_data <- subset(my_data,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-1"))

## Remove incomplete observations
my_data <- my_data[complete.cases(my_data),]

## Combine Date and Time
dateTime <- paste(my_data$Date, my_data$Time)

## Name the vector as "DateTime
dateTime <- setNames(dateTime, "DateTime")

## Remove Date and Time column
my_data <- my_data[ ,!(names(my_data) %in% c("Date","Time"))]

## Add DateTime column
my_data <- cbind(dateTime, my_data)

## Format dateTime Column
my_data$dateTime <- as.POSIXct(dateTime)


# Create Plots---
## Create the histogram PLOT 1
hist(my_data$Global_active_power,
     main = "Global Active Power",
     xlab = "Global Active Power (kilowatts)",
     col = "red")

## Save plot 1 and close device
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()

## Create Plot 2 Line plot
plot(
  my_data$Global_active_power ~ my_data$dateTime,
  type = "l",
  ylab = "Global Active Power (kilowatts)",
  xlab = ""
)

## save plot2 and close device
dev.copy(png,"plot2.png", width=480, height=480)
dev.off()



## Create Plot 3 line 
with(my_data, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})

## plot the legend
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

## Saving plot 3 and close device 
dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()


## Create Plot 4

par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(my_data, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})


## Saving to file
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()


