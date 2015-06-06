data <- read.table("household_power_consumption.txt",sep=";",header=TRUE)
subData <- data[data$Date %in% c("1/2/2007","2/2/2007"),]
timeData <- strptime(paste(subData$Date,subData$Time,sep=" "), "%d/%m/%Y %H:%M:%S")
png("plot2.png",width=480,height=480)
plot(timeData,as.numeric(subData$Global_active_power,length = 1),type="l",xlab = "",ylab="Global Active Power (kilowatts)",yaxt="n")
axis(2,at=c(0,1000,2000,3000),labels=c(0,2,4,6))
dev.off()


