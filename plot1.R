library(ggplot2)
library(scales)
library(lubridate) # for wday function to find weekday        
library(dplyr) #used in filtering
library(gridExtra) #only used for plot 4 but included everywhere to standardize code

plot1 <- function() {
        #setting the theme to get a white background and no grid lines (from ggplot2)
        theme_set(theme_bw(12));
        #dev.off()
        # Reading the file
        EPC <-
                read.csv(
                        "household_power_consumption.txt",
                        header = T,
                        sep = ";",
                        stringsAsFactors = F,
                        colClasses = c(Date = "character")
                )
        
        # Adding Date column + weekday column from Lubridate and the filter	clause for the date
        EPC.1 <-
                mutate(
                        EPC,
                        asDate = as.Date(Date,format = "%d/%m/%Y")
                )  %>% filter(asDate >= "2007-02-01" &   asDate <= "2007-02-02")
        
       # Creating a column which has the weekday
        Weekday <- as.data.frame(lubridate::wday(as.Date(EPC.1$Date), label = TRUE))
        colnames(Weekday) <- "Weekday"
        
        # The charts need a combination of the Date and time columns - creating a column which will then cbind to the original data frame
        DateTime <-
                as.data.frame(DateTime <-
                                      strptime(paste0(EPC.1$Date, EPC.1$Time), format = "%d/%m/%Y %H:%M:%S"))
        colnames(DateTime) <- "Date.Time"
        EPC.2 <- cbind(DateTime, Weekday, EPC.1)

        # opening a 480X480 png file to save the chart
        png(
                filename = "Plot1.png",
                width = 480,
                height = 480,
                bg = "white"
        )
        
        # plot
        hist(
                as.numeric(EPC.2$Global_active_power),
                xlab = "Global Active Power (kilowatts)",
                ylab = "Frequency",
                main = "Global Active Power",
                col = "red"
        )
        
        # need to close else png file is not written
        dev.off()
}