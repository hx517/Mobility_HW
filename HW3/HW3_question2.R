#Question 2
#Get current working directory
setwd('C:/Users/xihao/Documents/学习/4. Data-driven Mobility Model/HW3')


Filename <- "d04_text_station_5min_2018_02_07.txt.gz"
PeMS <- read.delim(gzfile(Filename), sep="," ,header=FALSE)
dim(PeMS)
PeMS[1,]


############################
#  Question 1: Simple plot
############################
# Selecting a loop detector station with a given ID, for instance SelectedID=404922 (lanes=3)
SelectedID <- 403430 
StationData_1 <- subset(PeMS,PeMS [,2]==SelectedID)

SelectedID <- 401513  
StationData_2 <- subset(PeMS,PeMS [,2]==SelectedID)

## Station 1 
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,2))
# Assume capacity is 1000 for each lane
lanes=5  # Number of lanes of the roadway at a given direction
obliqueQ <- 1000*lanes/12*(1:288)
plot(ts(StationData_1[,1]),cumsum(StationData_1[,10]),type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(obliqueQ,cumsum(StationData_1[,10]))*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Flow N (t) (veh/5min)",main="Time vs. Flow",col="blue")
lines(1:288,obliqueQ,lwd=2,col=2)
plot(ts(StationData_1[,1]),cumsum(StationData_1[,10])-obliqueQ,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData_1[,10])-obliqueQ)*1.1,max(cumsum(StationData_1[,10])-obliqueQ)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Flow: N(t)-q*t (veh/5min)",main="Time vs. Flow",col="blue")

# Assume normal occupancy is 6%
obliqueO <- 0.06*(1:288)*100
plot(ts(StationData_1[,1]),cumsum(StationData_1[,11])*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(cumsum(StationData_1[,11]))*100*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Occupancy O(t) (%)",main="Time vs. Average Occupancy",col="blue")
lines(1:288,obliqueO,lwd=2,col=2)
plot(ts(StationData_1[,1]),cumsum(StationData_1[,11])*100-obliqueO,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData_1[,11])*100-obliqueO)*1.1,max(cumsum(StationData_1[,11])*100-obliqueO)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Occupancy: O(t)-occ*t (%)",main="Time vs. Average Occupancy",col="blue")



## Station 2 
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,2))
# Assume capacity is 1000 for each lane
lanes=5  # Number of lanes of the roadway at a given direction
obliqueQ <- 1000*lanes/12*(1:288)
plot(ts(StationData_2[,1]),cumsum(StationData_2[,10]),type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(obliqueQ,cumsum(StationData_2[,10]))*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Flow N (t) (veh/5min)",main="Time vs. Flow",col="blue")
lines(1:288,obliqueQ,lwd=2,col=2)
plot(ts(StationData_2[,1]),cumsum(StationData_2[,10])-obliqueQ,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData_2[,10])-obliqueQ)*1.1,max(cumsum(StationData_2[,10])-obliqueQ)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Flow: N(t)-q*t (veh/5min)",main="Time vs. Flow",col="blue")

# Assume normal occupancy is 6%
obliqueO <- 0.06*(1:288)*100
plot(ts(StationData_2[,1]),cumsum(StationData_2[,11])*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(cumsum(StationData_2[,11]))*100*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Occupancy O(t) (%)",main="Time vs. Average Occupancy",col="blue")
lines(1:288,obliqueO,lwd=2,col=2)
plot(ts(StationData_2[,1]),cumsum(StationData_2[,11])*100-obliqueO,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData_2[,11])*100-obliqueO)*1.1,max(cumsum(StationData_2[,11])*100-obliqueO)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Occupancy: O(t)-occ*t (%)",main="Time vs. Average Occupancy",col="blue")
