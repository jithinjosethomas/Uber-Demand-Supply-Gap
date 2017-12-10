#Uber case study
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
#1. Import the uber csv file
uber_data_raw <- read.csv("Uber Request Data.csv")
#analyse the structure of the data set
str(uber_data_raw)

#2. Factorise relevant data
uber_data_raw$Pickup.point <- as.factor(uber_data_raw$Pickup.point)
uber_data_raw$Driver.id <- as.factor(uber_data_raw$Driver.id)
str(uber_data_raw)

#3. Get a basic feel of the dataset
#noOfRows: 6745
#noOfColumns: 6
#---------------------------------------------------------------------------------------------------------------------------------#
#Request id: A unique identifier of the request
#noOfUniques: 6745
length(unique(uber_data_raw$Request.id))
#noOfMissing: 0
sum(is.na(uber_data_raw$Request.id))
#---------------------------------------------------------------------------------------------------------------------------------#
#Time of request: The date and time at which the customer made the trip request
#noOfUniques: 5618
length(unique(uber_data_raw$Request.timestamp))
#noOfMissing: 0
sum(is.na(uber_data_raw$Request.timestamp))
#---------------------------------------------------------------------------------------------------------------------------------#
#Drop-off time: The drop-off date and time, in case the trip was completed 
#noOfUniques: 2599
length(unique(uber_data_raw$Drop.timestamp))
#noOfMissing: 3914
sum(is.na(uber_data_raw$Drop.timestamp))
#---------------------------------------------------------------------------------------------------------------------------------#
#Pick-up point: The point from which the request was made
#noOfUniques: 2
length(unique(uber_data_raw$Pickup.point))
#noOfMissing: 0
sum(is.na(uber_data_raw$Pickup.point))
#---------------------------------------------------------------------------------------------------------------------------------#
#Driver id: The unique identification number of the driver
#noOfUniques: 301
length(unique(uber_data_raw$Driver.id))
#noOfMissing: 2650
sum(is.na(uber_data_raw$Driver.id))
#---------------------------------------------------------------------------------------------------------------------------------#
#Status of the request: The final status of the trip, that can be either completed, cancelled by the driver or no cars available
#noOfUniques: 3
length(unique(uber_data_raw$Status))
#noOfMissing: 0
sum(is.na(uber_data_raw$Status))
#---------------------------------------------------------------------------------------------------------------------------------#



#4.Convert the Request.timestamp date-time to proper date-time POSIX ct format
# will only filter out date
clean_date1 <- as.POSIXct(uber_data_raw$Request.timestamp,  format = "%d-%m-%Y %H:%M:%S")
clean_date2 <- as.POSIXct(uber_data_raw$Request.timestamp,  format = "%d/%m/%Y %H:%M")
#identify the index of the valid dates
idx_clean_date1 <- which(!is.na(clean_date1))
idx_clean_date2 <- which(!is.na(clean_date2))

#convert Request.timestamp to proper date-time format
uber_data_raw$Request.timestamp <- replace(clean_date1, idx_clean_date2, clean_date2[!is.na(clean_date2)])

#5.Convert Drop.timestamp to proper date-time POSIX ct format
clean_date1 <- as.POSIXct(uber_data_raw$Drop.timestamp,  format = "%d-%m-%Y %H:%M:%S")
clean_date2 <- as.POSIXct(uber_data_raw$Drop.timestamp,  format = "%d/%m/%Y %H:%M")
#identify the index of the valid dates
idx_clean_date1 <- which(!is.na(clean_date1))
idx_clean_date2 <- which(!is.na(clean_date2))
#replace clean_date1's NA values with corresponding values from clean_data2, and store final result to Drop.timestamp
uber_data_raw$Drop.timestamp <- replace(clean_date1, idx_clean_date2, clean_date2[!is.na(clean_date2)])


#6.Extracting day, hour of the day, trip duration in minutes
#day of the week
uber_data_raw$Day <- weekdays(uber_data_raw$Request.timestamp)
#month of the year
uber_data_raw$Month <- months(uber_data_raw$Request.timestamp)
#trip duration in minutes, round it off
uber_data_raw$TripDuration <- difftime(uber_data_raw$Drop.timestamp, uber_data_raw$Request.timestamp, units = "mins")
uber_data_raw$TripDuration <- round(uber_data_raw$TripDuration,0)
#req timestamp hour of the day only (bin)
uber_data_raw$PickTimehourOnly <- factor(format(uber_data_raw$Request.timestamp, "%H"))
#drop timestamp hour of the day only (bin)
uber_data_raw$DropTimehourOnly <- factor(format(uber_data_raw$Drop.timestamp, "%H"))

#7) Important metrics
#A) City to Airport trips
#i) Peak hours: 04:00 to 10:00
uber_data_raw_cityToairport <- subset(uber_data_raw, uber_data_raw$Pickup.point == "City")
cityToairportCount <- nrow(uber_data_raw_cityToairport) #3507 
sort(table(uber_data_raw_cityToairport$PickTimehourOnly), decreasing=TRUE)[1:7] # peak hours 05  08  09  07  06  10  04
#ii) mean/1st, 3rd quartile travel duration
summary(as.numeric(uber_data_raw_cityToairport$TripDuration)) #mean: 52.57mins
#iii) Trips completed % 
cityToairportTripsCompleted <- nrow(subset(uber_data_raw_cityToairport,uber_data_raw_cityToairport$Status == "Trip Completed"))
cityToairportTripsRejected <- nrow(subset(uber_data_raw_cityToairport,uber_data_raw_cityToairport$Status == "Cancelled"))
cityToairportTripsNotAvailable <- nrow(subset(uber_data_raw_cityToairport,uber_data_raw_cityToairport$Status == "No Cars Available"))
cityToairportTripsCompleted/(cityToairportTripsRejected+cityToairportTripsCompleted) #58.5%
#iv) Supply-Demand gap:  #438
GapCityToAirport <- cityToairportTripsRejected - cityToairportTripsCompleted


#B) Airport to City trips
#i) Peak hours: 04:00 to 10:00
uber_data_raw_airportTocity <- subset(uber_data_raw, uber_data_raw$Pickup.point == "Airport")
airportTocityCount <- nrow(uber_data_raw_airportTocity) #3238
sort(table(uber_data_raw_airportTocity$PickTimehourOnly), decreasing=TRUE)[1:7] # peak hours 18  20  19  21  17  22  23
#ii) mean/1st, 3rd quartile travel duration
summary(as.numeric(uber_data_raw_airportTocity$TripDuration)) #mean: 52.24mins
#iii) Trips completed % 
airportTocityTripsCompleted <- nrow(subset(uber_data_raw_airportTocity,uber_data_raw_airportTocity$Status == "Trip Completed"))
airportTocityTripsRejected <- nrow(subset(uber_data_raw_airportTocity,uber_data_raw_airportTocity$Status == "Cancelled"))
airportTocityTripsNotAvailable <- nrow(subset(uber_data_raw_airportTocity,uber_data_raw_airportTocity$Status == "No Cars Available"))
airportTocityTripsCompleted/(airportTocityTripsRejected+airportTocityTripsCompleted) #87%
#iv) Supply-Demand gap:  #584
GapAirportToCity <- airportTocityTripsNotAvailable + airportTocityTripsRejected - airportTocityTripsCompleted
GapAirportToCity


#8)Revenue loss during peak hours
#a) city to airport
#Cancelled
cityToairportTripsRejected <- subset(uber_data_raw_cityToairport,uber_data_raw_cityToairport$Status == "Cancelled")
peakHourcityToairportCancellation <- nrow(filter(cityToairportTripsRejected, as.numeric(cityToairportTripsRejected$PickTimehourOnly) %in% c(05, 08,  09,  07,  06,  10,  04)))
#No Cars Available
cityToairportTripsNotAvailable <- subset(uber_data_raw_cityToairport,uber_data_raw_cityToairport$Status == "No Cars Available")
peakHourcityToairportNotAvailable <- nrow(filter(cityToairportTripsNotAvailable, as.numeric(cityToairportTripsNotAvailable$PickTimehourOnly) %in% c(05, 08,  09,  07,  06,  10,  04)))
#b) airport to city
#Cancelled
airportTocityTripsRejected <- subset(uber_data_raw_airportTocity,uber_data_raw_airportTocity$Status == "Cancelled")
peakHourairportTocityCancellation <-nrow(filter(airportTocityTripsRejected, as.numeric(airportTocityTripsRejected$PickTimehourOnly) %in% c(18,  20,  19,  21,  17,  22,  23)))
#No Cars Available
airportTocityTripsNotAvailable <- subset(uber_data_raw_airportTocity,uber_data_raw_airportTocity$Status == "No Cars Available")
peakHourairportTocityNotAvailable <- nrow(filter(airportTocityTripsNotAvailable, as.numeric(airportTocityTripsNotAvailable$PickTimehourOnly) %in% c(18,  20,  19,  21,  17,  22,  23)))

#Inference
#A) For city to airport trips (loss in terms of requests)
#Total loss of revenue = 2003
#Total loss of revenue during peak hour = 871(cancellation)+451(No cars available) = 1322

#B) For airport to city trips (loss in terms of requests)
#Total loss of revenue = 1911
#Total loss of revenue during peak hour = 123(cancellation)+1430(No cars available) = 1553


