##########################
#                        #
#       Plot 4           #
#                        #
##########################


# The section delimeted below is commont to all charts:

# ============================================================================================
#Define url and file name

    url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    theFile   <- "household_power_consumption.txt"

# Set the date format to the format we are expecting.
    setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )

    temp <- tempfile()
    download.file(url,temp)
# Read in data only for the two days we need.  
# Also, make sure we read date and time appropriatly. --> Date & Time
    data <- subset(read.table(unz(temp, theFile), stringsAsFactor = FALSE, header = TRUE, sep = ';', colClasses=c("Date"="myDate", "Time"="character")), Date >= "2007/02/01" & Date < "2007/02/03")
    unlink(temp)

# Since most of the columns got read in as String, let's coerce the columns that need to be numeric to such type. 
    data[,c(3)] <- as.numeric(as.character(data[,c(3)], as.numeric))
    data[,c(5)] <- as.numeric(as.character(data[,c(5)], as.numeric))
    data[,c(7)] <- as.numeric(as.character(data[,c(7)], as.numeric))
    data[,c(8)] <- as.numeric(as.character(data[,c(8)], as.numeric))

#  The Charts need date and time together , as a single data point, such as this "2007-02-01 00:00:00".
#  So, we need to merge both columns Date and Time, into a dingle column. 

# Duplicate Date column
    data <- cbind(data,data[,1])

#Rename newly created column to "data_and_time"
    colnames(data)[10] <- "date_and_time"

# Merge date and time into the newly created column so that we can use Date and time as the x axis values in our charts. 
# Note that the time format needs to be specified as Y, m, d

    data$date_and_time <- paste(data$Date, data$Time)
# Now convert the data type of this column from character to POSIXlt
    strptime(data$date_and_time, "%Y-%m-%d %H:%M:%S")

# However, POSIXlt is of typeof "List", wich does not work for our plots.  Our plots need a column of vectors. 
# So, let's convert this data_and_time columns to POSICct

    data$date_and_time <- as.POSIXct((as.POSIXlt(dd$date_and_time)))

# End Section common to all charts:
# ==========================================================================================

# Set the Device to hold four charts 2 by 2 
    par(mfrow = c(2,2))
    
#Plot the first chart
    with(data, plot(Global_active_power~date_and_time, type = "l", ylab = "Global Active Power", xlab="" ))

# Plot the second plot
    with(data, plot(Voltage~date_and_time, type = "l", ylab = "Voltage", xlab="datetime" ))

# Plot the third plot
# Create the line time series for Plot 3 with multiple variates. Donot any data at this point
    with(data, plot(Sub_metering_1~date_and_time, type = "n", xlab="", ylab = "Energy sub metering"))

# Add each series:
    lines(data$Sub_metering_1~data$date_and_time, col = "Black")
    lines(data$Sub_metering_2~data$date_and_time, col = "Red")
    lines(data$Sub_metering_3~data$date_and_time, col = "Blue")

    legend("topright", pch="-", col = c("black", "red", "blue"), legend = c("Sub_metering1", "Sub_metering2", "Sub_metering3"))

#Plot the first chart
    with(data, plot(Global_reactive_power~date_and_time, type = "l", ylab = "Global_reactive_power", xlab="datetime" ))

# Copy all the plots to a single file
    dev.copy(png, file = "plot4.png")
        
# Close the PDF file device
    dev.off()    
