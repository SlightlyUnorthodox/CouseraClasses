data <- read.table("household_power_consumption.txt",sep=";",header=TRUE)
subData <- data[data$Date %in% c("1/2/2007","2/2/2007"),]
png("plot1.png",width=480,height=480)
hist(as.numeric(subData$Global_active_power),col="red",main="Global Active Power",xlab="Global Active Power (kilowatts)")
dev.off()




