# Data-driven mobility modeling and simulation (CUSP-GX 9007)
# Lab 1
# R package installation and a few examples 
# By Kun Xie 


# Install and load packages
install.packages('googleVis')
library(googleVis)

# Example 1
states <- data.frame(state.name, state.x77)
GeoStates <- gvisGeoChart(states, "state.name", "Population",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)

# Example 2
Cal <- gvisCalendar(Cairo, 
                    datevar="Date", 
                    numvar="Temp",
                    options=list(
                          title="Daily temperature in Cairo",
                          height=320,
                          calendar="{yearLabel: { fontName: 'Times-Roman',
                          fontSize: 32, color: '#1A8763', bold: true},
                          cellSize: 10,
                          cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                          focusedCellColor: {stroke:'red'}}"))
plot(Cal)

# Example 3
AndrewMap <- gvisMap(Andrew, "LatLong" , "Tip", 
                     options=list(showTip=TRUE, 
                                  showLine=TRUE, 
                                  enableScrollWheel=TRUE,
                                  mapType='terrain', 
                                  useMapTypeControl=TRUE))
plot(AndrewMap)

# Example 4
install.packages("forecast")
library("forecast")
fit <- auto.arima(USAccDeaths)
plot(forecast(fit,h=30))


        