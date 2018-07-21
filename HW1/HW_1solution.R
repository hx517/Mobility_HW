#setup working folder
setwd("")
#load dataset
data<-read.delim("HW1_Dataset.csv",sep=",")
#check dimension of the dataset
dim(data)
colnames(data)
# maximum actual speed
max(data$Actual_Speed.mph.)
# alternative
max(data[,4])
# maximum actual speed
max(data$Historical_Speed.mph.)
# alternative
max(data[,5])

# mean actual travel time in minutes
ATT<-((data$Segment_Length.mile.)/(data$Actual_Speed.mph.)*60)
mean(ATT)
max(ATT)/60
# variance of travel time
# first hour
var(ATT[1:12])
# second hour
var(ATT[13:24])
# third hour
var(ATT[25:36])

# mean actual travel time in minutes
HTT<-((data$Segment_Length.mile.)/(data$Historical_Speed.mph.)*60)
mean(HTT)
max(HTT)
# variance of travel time
# first hour
var(HTT[1:12])
# second hour
var(HTT[13:24])
# third hour
var(HTT[25:36])

# Difference in travel time
max(abs(ATT-HTT))
# The maximum difference occurred at:
data[16,2]

#Plot
par(mfrow=c(1,1))
plot(ATT,ylim=c(0,3),xlab="Time Sequence",,ylab="Travel Time (min)",type="l",col=2)
lines(HTT,col=1)
legend("topright",c("Actual","Historical"),lty=c(1,1),col=c(2,1))







