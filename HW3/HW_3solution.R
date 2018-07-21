# Data-driven mobility modeling and simulation (CUSP-GX 9007)
# Lab 5
# Cumulative and Oblique Curve 
# By Kun Xie 

# Include a libraty to read txt.gz files (You may not need for latest R version)
library("utils")  

# Set up your work directory that include your datasets


############################
#  Example 1: Simple plot  #
############################

# Before read raw data:
# You can refer to the website above to check the definition of each column
# col_1=Timestamp; col_2=Station; col_3: District; col_4=Freeway #; col_5=Directon (NSEW)
# col_6=Lane Type; col_7=Station Length; col_8=Samples; col_9=% Observed; col_10=Total Flow
# col11=Avg Occupancy; col_12=Ave Speed; 
# the remaining columns show individual info (Lane N Samples, Flow, Avg Occ, Avg Speed,Obverved(=1observed; 0=imputed))

# Now load one-day data from I880,
# Note: You can replace the file name to select another district/stations if you have the data in the folder
Filename <- "d04_text_station_5min_2018_02_07.txt.gz"
# In Class Analysis
PeMS <- read.delim(gzfile(Filename), sep="," ,header=FALSE)
dim(PeMS)
PeMS[1,]

# Selection a loop detector station with a given ID, for instance SelectedID=400340 (lanes=4)
SelectedID <- 400340   
StationData <- subset(PeMS,PeMS [,2]==SelectedID)
StationData[1,]
dim(StationData)

# Let's draw each of the plot
# Show multiple plots on the same window
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,3))
# Note: ts()--convert the timestamp to a time sequence
# Note: max()--find the max value of a set of data
TimeInterval <- 5
LowTime <- 0
UpTime <- length(Filename)*24*60/TimeInterval
plot(ts(StationData[,1]),StationData[,10],type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(StationData[,10])*1.1),xlab="Timestamp (Interval=5min)",ylab="Flow (veh/5min)",main="Time vs. Flow",col="blue")
plot(ts(StationData[,1]),StationData[,12],type="l",xlim=c(LowTime,UpTime),ylim=c(0,100),xlab="Timestamp (Interval=5min)",ylab="Average Speed (mph)",main="Time vs. Average Speed",col="blue")
plot(ts(StationData[,1]),StationData[,11]*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,100),xlab="Timestamp (Interval=5min)",ylab="Average Occupancy (%)",main="Time vs. Average Occupancy",col="blue")
plot(StationData[,10],StationData[,12],xlim=c(0,max(StationData[,10])*1.1),ylim=c(0,100),xlab="Flow (veh/5min)",ylab="Average Speed (mph)",main="Flow vs. Average Speed",col="blue",pch=19,cex=0.8)
plot(StationData[,10],StationData[,11]*100,xlim=c(0,600),ylim=c(0,100),xlab="Flow (veh/5min)",ylab="Average Occupancy (%)",main="Flow vs. Average Occupancy",col="blue",pch=19,cex=0.8)
plot(StationData[,12],StationData[,11]*100,xlim=c(0,100),ylim=c(0,100),xlab="Average Speed (mph)",ylab="Average Occupancy (%)",main="Average Occupancy vs. Average Speed",col="blue",pch=19,cex=0.8)


#############################################
#  Example 2: Cumulative and Oblique Curves #
#############################################
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,2))
# Assume capacity is 600 for each lane
lanes=4  # Number of lanes of the roadway at a given direction
obliqueQ <- 600*lanes/12*(1:288)
plot(ts(StationData[,1]),cumsum(StationData[,10]),type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(obliqueQ,cumsum(StationData[,10]))*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Flow N (t) (veh/5min)",main="Time vs. Flow",col="blue")
lines(1:288,obliqueQ,lwd=2,col=2)
plot(ts(StationData[,1]),cumsum(StationData[,10])-obliqueQ,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData[,10])-obliqueQ)*1.1,max(cumsum(StationData[,10])-obliqueQ)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Flow: N(t)-q*t (veh/5min)",main="Time vs. Flow",col="blue")

# Assume normal occupancy is 5%
obliqueO <- 0.05*(1:288)*100
plot(ts(StationData[,1]),cumsum(StationData[,11])*100,type="l",xlim=c(LowTime,UpTime),ylim=c(0,max(cumsum(StationData[,11]))*100*1.5),xlab="Timestamp (Interval=5min)",ylab="Cumulative Occupancy O(t) (%)",main="Time vs. Average Occupancy",col="blue")
lines(1:288,obliqueO,lwd=2,col=2)
plot(ts(StationData[,1]),cumsum(StationData[,11])*100-obliqueO,type="l",xlim=c(LowTime,UpTime),ylim=c(min(cumsum(StationData[,11])*100-obliqueO)*1.1,max(cumsum(StationData[,11])*100-obliqueO)*1.1),xlab="Timestamp (Interval=5min)",ylab="Oblique Occupancy: O(t)-occ*t (%)",main="Time vs. Average Occupancy",col="blue")

