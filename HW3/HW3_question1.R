#Get current working directory
setwd('C:/Users/xihao/Documents/ѧϰ/4. Data-driven Mobility Model/HW3')

# Before reading raw data:
# Here are the definitions for data in each column
# col_1=Timestamp; col_2=Station; col_3: District; col_4=Freeway #; col_5=Directon (NSEW)
# col_6=Lane Type; col_7=Station Length; col_8=Samples; col_9=% Observed; col_10=Total Flow
# col11=Avg Occupancy; col_12=Ave Speed; 
# the remaining columns show individual info (Lane N Samples, Flow, Avg Occ, Avg Speed,Obverved(=1observed; 0=imputed))

Filename <- "d03_text_station_5min_2018_02_02.txt.gz"
PeMS <- read.delim(gzfile(Filename), sep="," ,header=FALSE)
dim(PeMS)
PeMS[1,]


############################
#  Question 1: Simple plot
############################
# Selecting a loop detector station with a given ID, for instance SelectedID=404922 (lanes=3)
SelectedID <- 312133   
StationData_1 <- subset(PeMS,PeMS [,2]==SelectedID)

SelectedID <- 312134
StationData_2 <- subset(PeMS,PeMS [,2]==SelectedID)

# Temporal distribution of flow, average speed, and average occupancy for station 404916
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,3))

TimeInterval <- 5
LowTime <- 0
UpTime <- length(Filename)*24*60/TimeInterval
plot(ts(StationData_1[,1]),StationData_1[,10],type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(StationData_1[,10])*1.1),xlab="Timestamp (Interval=5min)",ylab="Flow (veh/5min)",main="Time vs. Flow",col="blue")
plot(ts(StationData_1[,1]),StationData_1[,12],type="l",xlim=c(LowTime,UpTime),ylim=c(0,100),xlab="Timestamp (Interval=5min)",ylab="Average Speed (mph)",main="Time vs. Average Speed",col="blue")
plot(ts(StationData_1[,1]),StationData_1[,11]*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,100),xlab="Timestamp (Interval=5min)",ylab="Average Occupancy (%)",main="Time vs. Average Occupancy",col="blue")
plot(StationData_1[,10],StationData_1[,12],xlim=c(0,max(StationData_1[,10])*1.1),ylim=c(0,100),xlab="Flow (veh/5min)",ylab="Average Speed (mph)",main="Flow vs. Average Speed",col="blue",pch=19,cex=0.8)
plot(StationData_1[,10],StationData_1[,11]*100,xlim=c(0,600),ylim=c(0,100),xlab="Flow (veh/5min)",ylab="Average Occupancy (%)",main="Flow vs. Average Occupancy",col="blue",pch=19,cex=0.8)
plot(StationData_1[,12],StationData_1[,11]*100,xlim=c(0,100),ylim=c(0,100),xlab="Average Speed (mph)",ylab="Average Occupancy (%)",main="Average Occupancy vs. Average Speed",col="blue",pch=19,cex=0.8)


# Temporal distribution of flow, average speed, and average occupancy for station 404922
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,3))

TimeInterval <- 5
LowTime <- 0
UpTime <- length(Filename)*24*60/TimeInterval
plot(ts(StationData_2[,1]),StationData_2[,10],type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(StationData_2[,10])*1.1),xlab="Timestamp (Interval=5min)",ylab="Flow (veh/5min)",main="Time vs. Flow",col="blue")
plot(ts(StationData_2[,1]),StationData_2[,12],type="l",xlim=c(LowTime,UpTime),ylim=c(0,100),xlab="Timestamp (Interval=5min)",ylab="Average Speed (mph)",main="Time vs. Average Speed",col="blue")
plot(ts(StationData_2[,1]),StationData_2[,11]*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,100),xlab="Timestamp (Interval=5min)",ylab="Average Occupancy (%)",main="Time vs. Average Occupancy",col="blue")
plot(StationData_2[,10],StationData_2[,12],xlim=c(0,max(StationData_2[,10])*1.1),ylim=c(0,100),xlab="Flow (veh/5min)",ylab="Average Speed (mph)",main="Flow vs. Average Speed",col="blue",pch=19,cex=0.8)
plot(StationData_2[,10],StationData_2[,11]*100,xlim=c(0,600),ylim=c(0,100),xlab="Flow (veh/5min)",ylab="Average Occupancy (%)",main="Flow vs. Average Occupancy",col="blue",pch=19,cex=0.8)
plot(StationData_2[,12],StationData_2[,11]*100,xlim=c(0,100),ylim=c(0,100),xlab="Average Speed (mph)",ylab="Average Occupancy (%)",main="Average Occupancy vs. Average Speed",col="blue",pch=19,cex=0.8)

