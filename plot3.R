data <- read.table("household_power_consumption.txt",sep=";",header=TRUE,stringsAsFactors=FALSE,dec=".")
subData <- data[data$Date %in% c("1/2/2007","2/2/2007"),]
timeData <- strptime(paste(subData$Date,subData$Time,sep=" "), "%d/%m/%Y %H:%M:%S")
energy1 <- as.numeric(subData$Sub_metering_1)
energy2 <- as.numeric(subData$Sub_metering_2)
energy3 <- as.numeric(subData$Sub_metering_3)
png("plot3.png",width=480,height=480)

plot(timeData,energy1,type="l",xlab = "",ylab="Energy sub metering")
lines(timeData,energy2,type="l",col="red")
lines(timeData,energy3,type="l",col="blue")
legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=1,lwd=2.5,col=c("black","red","blue"))

dev.off()

