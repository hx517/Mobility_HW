######################################
##  Conservation Law             ##
######################################

N0=0
dT= 1 # minutes
dX= 1 # mile
k0=N0/dX
for (i in 1:100){
  plot(NA,NA,xlim=c(0,140),ylim=c(0,100),main="Simulation of Conservation Law",xlab="Road Section (X)",ylab="",axes = FALSE,frame.plot = TRUE)
  q1=sample(1:10,1,replace=T)# veh/minute
  q2=min (N0+q1,sample(1:10,1,replace=T)) # veh/minute
  ki = (N0+q1-q2)/dX
  N0=(N0+q1-q2)
  text(40, 90, paste("Time = ",i,sep=""), pos =4,col=2)
  text(40, 80, paste("N = ",N0,sep=""), pos =4,col=4)
  text(-3, 10, paste("q1 = ",q1,sep=""), pos =4,col=2)
  text(120, 10, paste("q2 = ",q2,sep=""), pos =4,col=2)
  text(80, 90, paste("Ki-1 = ",k0,sep=""), pos =4,col=4)
  text(80, 80, paste("Ki = ",ki,sep=""), pos =4,col=4)
  text(40,70,paste("Delta_q = ", q2-q1), pos =4,col=4)
  text(80,70,paste("Delta_k = ", ki-k0), pos =4,col=4)
  rect(xleft=20, ybottom=20, xright=120, ytop=60, density = NULL, angle = 45, col = "gray", border = 3,  lwd = 1)
  color ="yellow"
  if(120-ki>120){
    color="red"
  }
  rect(xleft=120-ki, ybottom=20, xright=120, ytop=60, density = NULL, angle = 45, col = color, border = 3,  lwd = 1)
  k0 = ki
  Sys.sleep(0.3)
}


######################################
##  Check Actual Dataset            ##
######################################

#Set up your work directory that include your datasets
setwd("C:/Users/xihao/Documents/学习/4. Data-driven Mobility Model/HW4")
Filename<-"d04_text_station_5min_2013_01_04.txt"
PeMS <- read.delim(gzfile(Filename), sep="," ,header=FALSE)
dim(PeMS)
PeMS[1,]
TimeInterval<-5
LowTime<-0
UpTime<-length(Filename)*24*60/TimeInterval

#Selection a loop detector station with a given ID, for instance SelectedID=402278
#402828
TrafficCount<-function(upstreamSensorID, downsteramSensorID){
  SelectedID<-upstreamSensorID
  StationData<-subset(PeMS,PeMS[,2]==SelectedID)
  q<-StationData[,10]
  return (q)
}


sensor1 = TrafficCount(401613)  # Upstream Sensor
sensor2 = TrafficCount(400536)  # Downstream Sensor
# Simulated downstream departure flow
set.seed(10009)
SimulatedDeparture<-(sensor2+sample(1:as.integer(sd(sensor2)),65,replace=T)* sample(c(-1,1),65,replace=T))
SimulatedDeparture[which(SimulatedDeparture<0)]=0

N0=500
dT= 5 # minutes
dX= 0.59 # mile
k0=N0/dX
simulation<-function(inFlow, outFlow){
  for (i in 1:length(inFlow)){
    plot(NA,NA,xlim=c(0,140),ylim=c(0,100),main="Verifying Conservation Law Using Sensor Data",xlab="Road Section (X)",ylab="",axes = FALSE,frame.plot = TRUE)
    q1=inFlow[i] # veh/minute
    q2=outFlow[i] # veh/minute
    ki = (N0+q1-q2)/dX
    N0=(N0+q1-q2)
    text(40, 90, paste("Time = ",i,sep=""), pos =4,col=2)
    text(40, 80, paste("N = ",N0,sep=""), pos =4,col=4)
    text(-3, 10, paste("q1 = ",q1,sep=""), pos =4,col=2)
    text(120, 10, paste("q2 = ",q2,sep=""), pos =4,col=2)
    text(80, 90, paste("Ki-1 = ",k0,sep=""), pos =4,col=4)
    text(80, 80, paste("Ki = ",ki,sep=""), pos =4,col=4)
    text(40,70,paste("Delta_q = ", q2-q1), pos =4,col=4)
    text(80,70,paste("Delta_k = ", ki-k0), pos =4,col=4)
    rect(xleft=20, ybottom=20, xright=120, ytop=60, density = NULL, angle = 45, col = "gray", border = 3,  lwd = 1)
    color ="yellow"
    if(120-ki/5000*120>120){
      color="red"
    }
    rect(xleft=120-ki/5000*120, ybottom=20, xright=120, ytop=60, density = NULL, angle = 45, col =color, border = 3,  lwd = 1)
    k0 = ki
    Sys.sleep(0.3)
  }  
}

# Simulate with actual arrival and departure flows
simulation(sensor1, sensor2)
# Simulate with actual arrival flow and simulated departure flow
simulation(sensor1, SimulatedDeparture)

sensor1

