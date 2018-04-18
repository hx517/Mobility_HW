library(lubridate)
library(ggplot2)
library(tidyr)
library(devtools)
install_github("dgrtwo/broom")
library(broom)

yygetwd()
mydata = read.csv('HW1_Dataset.csv',header = TRUE)

#Q1
head(mydata, 3)

#Q2
mydata['Actual_Travel_Time'] = mydata['Segment_Length.mile.'] / mydata['Actual_Speed.mph.']
max(mydata['Actual_Travel_Time'])
ggplot(mydata, aes(x=rownames(mydata), Actual_Travel_Time)) + geom_bar(stat='identity')

mydata['Historical_Travel_Time'] = mydata['Segment_Length.mile.'] / mydata['Historical_Speed.mph.']
max(mydata['Historical_Travel_Time'])
ggplot(mydata, aes(x=rownames(mydata), Historical_Travel_Time)) + geom_bar(stat='identity')

sapply(mydata, class)

#Q3 actual travel time variance in each hour
times <- as.POSIXct(mydata$`Local_Time`, format='%m/%d/%Y %H:%M')
hours <- format(as.POSIXct(times), "%H")
mydata['hours'] = hours
aggregate(.~hours, data = mydata['Actual_Travel_Time'], var)

#Q4 maximum difference and plot
mydata['Act_His_Difference'] = abs(mydata['Actual_Travel_Time'] - mydata['Historical_Travel_Time'])
mydata[which(mydata['Act_His_Difference'] == max(mydata['Act_His_Difference'])),]
ggplot(mydata, aes(x=rownames(mydata), Act_His_Difference)) + geom_bar(stat='identity')

#Q5 Regression 
plot(mydata$Historical_Travel_Time, mydata$Actual_Travel_Time)
qplot(Historical_Travel_Time, Actual_Travel_Time, data=mydata)

reg1 = lm(Historical_Travel_Time ~ Actual_Travel_Time, data=mydata)
summary(reg1)
lmfit <- tidy(reg1)
lmfit
write.csv(lmfit, "Q5reg.csv")
