## REPRODUCIBLE RESEARCH ASSIGNMENT 1 ##
# 
# 
# setwd("./RepData_PeerAssessment1")
# Download code:

library(lubridate)
library(ggplot2)
library(scales)
library(stringr)

if(!file.exists("./data")){
        print("Directory not found.  Creating Data Directory...")
        dir.create("./data")}

if(!file.exists("./data/Act.zip")){
        print("Data file not found.  Downloading File...")
        fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(fileUrl,destfile = "./data/Act.zip")
        unzip("./data/Act.zip",exdir = "./data")
        file.rename("./data/Activity.csv","./data/Act.csv")
        file.remove("./data/Act.zip")
        rm(fileUrl)
        }

if(!exists("ACT")){
        print("Reading Activity file.....")
        ACT <- read.csv("../data/Act.csv",na.strings = "NA",stringsAsFactors = FALSE) 
        ACT$date <- as.Date(ACT$date,"%Y-%m-%d")
        ACT$day_type <- weekdays(ACT$date) =="Sunday" | weekdays(ACT$date) =="Saturday"
        ACT$day_type <- factor(ACT$day_type, levels=c(FALSE,TRUE),labels=c("weekday","weekend"))
        ACT$interval <-str_sub(paste(rep("000",nrow(ACT)),ACT$interval,sep=""),start=-4)
        }

# Compute Mean and Median Daily Totals and Plot a histogram of the Daily Totals
Daily_Totals <- tapply(ACT$steps,ACT$date,sum,na.rm=TRUE)
mnDT <- mean(Daily_Totals)
mdDT <- median(Daily_Totals)
print(paste("Mean Total Steps/Day:",mnDT))
print(paste("Median Total Steps/Day:",mdDT))

png(file="plot1.png",width = 480, height = 480)
hist(Daily_Totals, col = "gray",main = "Daily Total Steps", xlab="Daily Totals")
abline(v=mnDT, col="red",lty=2)
abline(v=mdDT, col="blue",lty=3)
legend("topright",legend=c("Mean","Median"),pch=151,col=c("red","blue"))
dev.off()

# Compute Average Daily Activity Pattern
Daily_activity <- aggregate(steps~interval,ACT,FUN=mean,na.rm=TRUE)
Daily_activity$Time <- as.POSIXct(strptime(Daily_activity$interval,"%H%M",tz="UTC"))

png(file="plot2.png",width = 960, height = 480)
plot(x=Daily_activity$Time,y=Daily_activity$steps,
     main = "Average Daily Activity",
     ylab="Average Steps",
     xlab = "Time of Day",
     type="l")
dev.off()

print(paste("Max Average Steps taken at:",(Daily_activity[which.max(Daily_activity$steps),]$interval)))

# Imputing Missing Values
print(paste("Total Number of NAs:",sum(is.na(ACT$steps))))
print(paste("Proportion of NAs:",100*mean(is.na(ACT$steps)),"%"))

ACT2 <-ACT

for (i in 1:nrow(ACT2)){
        if (is.na(ACT2[i,1])) ACT2[i,1]<-Daily_activity[which(ACT2[i,3] == Daily_activity[,1]),2]
}


# Heat map to find NA distribution
base_size <-10
xbase_size <- 10
ybase_size <- c(10,0,0,0,0,0,0,0,0,0)

(p <- ggplot(ACT, aes(date, interval)) 
+ geom_tile(aes(fill = steps, colour = "white")) 
+ scale_fill_gradient(low = "white", high = "steelblue")
+ theme_grey(base_size = base_size)  + labs(x = "", y = "") 
+ theme(legend.position = "none", 
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = xbase_size * 1, angle = 330,  
                                   hjust = 0, colour = "grey50"),
        axis.text.y = element_text(size = ybase_size * 0.8, angle = 330,
                                   hjust = 0, colour = "grey50"))
        )
# Heatmap Post-Imputation

(p <- ggplot(ACT2, aes(date, interval)) 
+ geom_tile(aes(fill = steps, colour = "white")) 
+ scale_fill_gradient(low = "white", high = "steelblue")
+ theme_grey(base_size = base_size)  + labs(x = "", y = "") 
+ theme(legend.position = "none", 
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = xbase_size * 0.8, angle = 330,  
                                   hjust = 0, colour = "grey50"),
        axis.text.y = element_text(size = ybase_size * 0.8, angle = 330,
                                   hjust = 0, colour = "grey50"))
)

Daily_Totals2 <- tapply(ACT2$steps,ACT2$date,sum,na.rm=TRUE)
mnDT2 <- mean(Daily_Totals2)
mdDT2 <- median(Daily_Totals2)
print(paste("Mean Total Steps/Day (after imputation):",mnDT2))
print(paste("Median Total Steps/Day (after imputation):",mdDT2))

png(file="plot3.png",width = 480, height = 480)
hist(Daily_Totals2, col = "gray",main = "Daily Total Steps (with imputation)", xlab="Daily Totals")
abline(v=mnDT2, col="red",lty=2)
abline(v=mdDT2, col="blue",lty=3)
legend("topright",legend=c("Mean","Median"),pch=151,col=c("red","blue"))
dev.off()

Daily_activity <- aggregate(steps~interval,ACT,FUN=mean,na.rm=TRUE)
Wkdy_activity <- aggregate(steps~interval+day_type,ACT2,FUN=mean,na.rm=TRUE)
Wkdy_activity$Time <- as.POSIXct(strptime(Daily_activity$interval,"%H%M",tz="UTC"))

base_size = 10

g <- ggplot(Wkdy_activity, aes(Time,steps))
( g + geom_line()+facet_grid(day_type~.)
+theme(legend.position = "none", axis.ticks = element_blank(), 
                                axis.text.x = element_text(size = base_size * 0.8, 
                                                           angle = 330,  
                                 hjust = 0, colour = "grey50"))
+ labs(title = "Average Daily Activity by Day Type")
+ scale_x_datetime("Interval",labels = date_format("%H:%M"))
)

