# Data-Driven Week #3 Code
# Abdullah Kurkcu

# Install the required packages
# install.packages("ggplot2")
# install.packages("parsedate")
#install.packages("reshape2")

# Libraries
library("ggplot2")
library("parsedate")
#library("reshape2")
#library("dplyr")
# Read the input into a dataframe
idf <- read.csv("busDataHW.csv", head=TRUE)

head(idf)

# Bus line = M15
df <- subset(idf, idf$publishlinename == "B54")

# Format date column
df$date <- as.POSIXlt(parse_iso_8601(substr(df$date,1,19)))

# Extract hour from the column
df$date$hour

# Hour = 18
df <- subset(df, df$date$hour == "17")

# Direction = 0
df <- subset(df, df$direction == 0)

# Let's take a look at the data
head(df)

## Creating multiple plots using ggplot2
ggplot(df,aes(x=date, y=CallDistanceAlongRoute,colour=datedvehiclejourneyref)) + 
  geom_line()+ labs(x="Time (Hour:Minute)", y="Distance (ft)", title="Time-Space Diagram (B54-Direction:0)")+
  theme(legend.position="none")

## Selection location x = 4000 ft
## Reset Index
rownames(df) = 1:length(df$X)

## Easiest way to find two points
below4k <- subset(df,df$CallDistanceAlongRoute<=4000)
above4k <- subset(df,df$CallDistanceAlongRoute>4000)
common <- intersect(below4k$datedvehiclejourneyref, above4k$datedvehiclejourneyref)  
below4k <- subset(below4k,below4k$datedvehiclejourneyref %in% common)
above4k <- subset(above4k,above4k$datedvehiclejourneyref %in% common)

# use aggregate to create new data frame with the maxima
below4k.agg <- aggregate(CallDistanceAlongRoute ~ datedvehiclejourneyref, below4k, max)
# then simply merge with the original
below4k.max <- merge(below4k.agg, below4k)
below4k.max

# Let's Plot what it looks like 
ggplot(df,aes(x=date, y=CallDistanceAlongRoute,colour=datedvehiclejourneyref)) + 
 
  geom_line()+ labs(x="Time (Hour:Minute)", y="Distance (ft)", title="Time-Space Diagram (B54-Direction:0)")+
  theme(legend.position="none")

# use aggregate to create new data frame with the minima
above4k.agg <- aggregate(CallDistanceAlongRoute ~ datedvehiclejourneyref, above4k, min)
# then simply merge with the original
above4k.min <- merge(above4k.agg, above4k)
above4k.min

# Let's Plot what it looks like 
ggplot(df,aes(x=date, y=CallDistanceAlongRoute,colour=datedvehiclejourneyref)) + 
  geom_point(data=below4k.max,aes(x=date, y=CallDistanceAlongRoute,colour=datedvehiclejourneyref))+
  geom_point(data=above4k.min,aes(x=date, y=CallDistanceAlongRoute,colour=datedvehiclejourneyref))+
  geom_line()+ labs(x="Time (Hour:Minute)", y="Distance (ft)", title="Time-Space Diagram (B54-Direction:0)")+
  theme(legend.position="none")

speed <- c()


# I explained how to compute space mean speed in the class
# Please read the definition again if you have quesitons.
# This is an example of what it should look like
# speed = distance diff / time diff

for(i in 0:length(common)){
  
  s1 <- subset(below4k.max,below4k.max$datedvehiclejourneyref==common[i])
  s2 <- subset(above4k.min,above4k.min$datedvehiclejourneyref==common[i])
  speed[i] <- (min(s2$CallDistanceAlongRoute)-max(s1$CallDistanceAlongRoute)) /as.numeric(difftime(min(s2$date),max(s1$date),units="secs"))
}

mean(speed)




