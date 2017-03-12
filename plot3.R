library(ggplot2)
library(scales)
library(lubridate) # for wday function to find weekday        
library(dplyr) #used in filtering
library(gridExtra) #only used for plot 4 but included everywhere to standardize code

plot3 <- function() {
        #setting the theme to get a white background and no grid lines (from ggplot2)
        theme_set(theme_bw(12));

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
                filename = "Plot3.png",
                width = 480,
                height = 480,
                bg = "white"
        )
        
        # plot - using print() as ggplot does not write automatically to file
        print(
        ggplot(data=EPC.2, aes(x=Date.Time)) + 
                geom_line(aes(y = as.numeric(Sub_metering_1), colour = "Sub_metering_1")) + 
                geom_line(aes(y = as.numeric(Sub_metering_2), colour = "Sub_metering_2")) + 
                geom_line(aes(y = as.numeric(Sub_metering_3), colour = "Sub_metering_3")) +
                ylab("Energy sub metering")  + scale_x_datetime(breaks = date_breaks("1 day"),  labels = date_format("%a")) +
                scale_colour_manual("", breaks = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), values = c("black", "red", "blue")) +
                xlab(" ") +
                theme(legend.position = c(0.8, 0.8)) 
        )
        
        # need to close else png file is not written
        dev.off()
}