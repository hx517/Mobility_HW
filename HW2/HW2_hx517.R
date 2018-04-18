# Problem 1
# a)
data = read.csv('busDataHW.csv')

data$date = substr(data$date,1,19)
data$date = strptime(data$date, format="%Y-%m-%dT%H:%M:%S")

filter = data[(format(data$date,"%H") == 17 & data["publishlinename"]=="B54" & data["directionref"]==0),]

# b),c)
filter = filter[with(filter, order(filter$date)),]

library("rgl")
plot3d(filter$locx,filter$locy, filter$date, type="l", col=as.numeric(filter$datedvehiclejourneyref))

Buses = substr(filter$datedvehiclejourneyref,36,42)
library("ggplot2")
p = ggplot(filter,aes(x=date, y=CallDistanceAlongRoute, colour=Buses)) + 
  geom_line() + labs(title = "Time-space Diagram for All The Buses within The Filtered Dataset") + theme(legend.position="top") + theme(plot.title = element_text(face = "bold",hjust = 0.5)) + theme(legend.title = element_text(face = "bold")) 
p + annotate("rect", xmin = filter$date[125], xmax = filter$date[160], ymin = 900, ymax = 1900,
             alpha = .2) +
    annotate("text", x = filter$date[130], y = 2100, label = "where bus-bunching takes place",colour = "red", size = 5) + annotate("rect", xmin = filter$date[119], xmax = filter$date[145], ymin = 6000, ymax = 7000, 
                                                                                                                                   alpha = .2) +
  annotate("text", x = filter$date[123], y = 7200, label = "where bus-bunching takes place",colour = "red", size = 5) 

# Problem 2
# a)
filter2 = data[data$datedvehiclejourneyref %in% filter$datedvehiclejourneyref,][c('datedvehiclejourneyref','date','CallDistanceAlongRoute')]
filter2 = filter2[filter2$CallDistanceAlongRoute < 6000,]

ggplot(filter2,aes(x=date, y=CallDistanceAlongRoute, colour=datedvehiclejourneyref)) + 
  geom_line() 
# There is a bus changing its direction halfway so I'm going to eliminate the second track of it
change_date = filter2[(filter2$CallDistanceAlongRoute == max(filter2[filter2$datedvehiclejourneyref=='MTA NYCT_FP_B7-Weekday-SDon-101400_B54_214',]$CallDistanceAlongRoute)&filter2$datedvehiclejourneyref == 'MTA NYCT_FP_B7-Weekday-SDon-101400_B54_214'),]$date

delete_row = row.names(filter2[(filter2$datedvehiclejourneyref=='MTA NYCT_FP_B7-Weekday-SDon-101400_B54_214'&filter2$date>change_date),])
filter2 = filter2[!rownames(filter2) %in% delete_row,]
ggplot(filter2,aes(x=date, y=CallDistanceAlongRoute, colour=datedvehiclejourneyref)) + 
  geom_line() 

# Since all buses seem to run in a straight line in the time-space diagram, I'm going to assume that all buses are running at a fixed speed. So I'm going to do linear regression on them and predict their tracks
V = c()
for (bus in unique(filter2$datedvehiclejourneyref)){
  df = filter2[filter2$datedvehiclejourneyref==bus,]
  fit = lm(CallDistanceAlongRoute ~ as.numeric(date),data=df)
  v = 6000/((6000+as.numeric(coefficients(fit)[1]))/as.numeric(coefficients(fit)[2])-(0+as.numeric(coefficients(fit)[1]))/as.numeric(coefficients(fit)[2]))
  V = c(V,v)
}
# The time here is all in second(s)
# Time mean spped 
v_t = 1/length(V)*sum(V)
v_t

# Space mean speed
v_reci = c()
for (v in V){
  v_reci = c(v_reci,1/v)
}
v_s = length(V)/sum(v_reci)
v_s
