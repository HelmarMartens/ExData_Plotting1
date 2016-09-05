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

# Set the Device to hold just one chart
    par(mfrow = c(1,1))
    
# Open png device
    png(file = "plot1.png")
    
# Create the Histogram chart: 
    hist(data$Global_active_power, col = "Red", xlab = "Global Active Power (Kilowatts)", main = "Global Active Power")
    
# Close the PDF file device
    dev.off()
