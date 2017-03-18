#import libraries and load dataset
library(readr)
library(stringr)
library(stringi)
Airplane_Crashes_and_Fatalities_Since_1908 <- read_csv("C:/Users/Aleks/Desktop/Big Data/Airplane_Crashes_and_Fatalities_Since_1908.csv")

#Adding values to missing data in the Aboard column.  The added data is the average overall.
Airplane_Crashes_and_Fatalities_Since_1908$Aboard = ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Aboard),
ave(Airplane_Crashes_and_Fatalities_Since_1908$Aboard, FUN = function(x) mean(x, na.rm = TRUE)),
Airplane_Crashes_and_Fatalities_Since_1908$Aboard)

#Adding values to missing data in the Fatalities column.  The added data is the average overall.
Airplane_Crashes_and_Fatalities_Since_1908$Fatalities = ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Fatalities),
ave(Airplane_Crashes_and_Fatalities_Since_1908$Fatalities, FUN = function(x) mean(x, na.rm = TRUE)),
Airplane_Crashes_and_Fatalities_Since_1908$Fatalities)

#Adding values to missing data in the Ground column.  The added data is the average overall.
Airplane_Crashes_and_Fatalities_Since_1908$Ground = ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Ground),
ave(Airplane_Crashes_and_Fatalities_Since_1908$Ground, FUN = function(x) mean(x, na.rm = TRUE)),
Airplane_Crashes_and_Fatalities_Since_1908$Ground)






#Converting all values to a whole number from both columns
as.integer(Airplane_Crashes_and_Fatalities_Since_1908$Aboard)
as.integer(Airplane_Crashes_and_Fatalities_Since_1908$Fatalities)
as.integer(Airplane_Crashes_and_Fatalities_Since_1908$Ground)

#Replacing all values with NA in the summary column with a more meanigful message
Airplane_Crashes_and_Fatalities_Since_1908$Summary <- as.character(Airplane_Crashes_and_Fatalities_Since_1908$Summary)
Airplane_Crashes_and_Fatalities_Since_1908$Summary <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Summary),
'No information on how the accident occured', Airplane_Crashes_and_Fatalities_Since_1908$Summary)

#Replacing all values with NA in the time column with a more meanigful message
Airplane_Crashes_and_Fatalities_Since_1908$Time <- as.character(Airplane_Crashes_and_Fatalities_Since_1908$Time)
Airplane_Crashes_and_Fatalities_Since_1908$Time <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Time),
'Unknown time', Airplane_Crashes_and_Fatalities_Since_1908$Time)

#Replacing all values with NA in the Flight # column with a more meanigful message
Airplane_Crashes_and_Fatalities_Since_1908$'Flight #' <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$'Flight #'),
'Unknown Flight #', Airplane_Crashes_and_Fatalities_Since_1908$'Flight #')

#Replacing all values with NA in the Route column with a more meanigful message
Airplane_Crashes_and_Fatalities_Since_1908$Route <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Route),
'Unknown Route', Airplane_Crashes_and_Fatalities_Since_1908$Route)

#Replacing all values with NA in the Type column with a more meanigful message
Airplane_Crashes_and_Fatalities_Since_1908$Type <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Type),
'Unknown Type', Airplane_Crashes_and_Fatalities_Since_1908$Type)

#Replacing all values with NA in the Registration column with a more meanigful message
Airplane_Crashes_and_Fatalities_Since_1908$Registration <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Registration),
'Unknown Registration', Airplane_Crashes_and_Fatalities_Since_1908$Registration)

#Replacing all values with NA in the Operator column with a more meanigful message
Airplane_Crashes_and_Fatalities_Since_1908$Operator <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Operator),
'Unknown Operator', Airplane_Crashes_and_Fatalities_Since_1908$Operator)

#Replacing all values with NA in the Date column with a more meanigful message
Airplane_Crashes_and_Fatalities_Since_1908$Date <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Date),
'Unknown Date', Airplane_Crashes_and_Fatalities_Since_1908$Date)

#Replacing all values with NA in the Location column with a more meanigful message
Airplane_Crashes_and_Fatalities_Since_1908$Location <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Location),
'Unknown Location', Airplane_Crashes_and_Fatalities_Since_1908$Location)







#Delete the cn/cl column as it does not appear to be hugely important for what I am trying to achieve
#Also there is only about 400 out of 5268 rows of actual data within that column
Airplane_Crashes_and_Fatalities_Since_1908$'cn/In' <- NULL

#Convert to String
Airplane_Crashes_and_Fatalities_Since_1908$Location <- sapply(Airplane_Crashes_and_Fatalities_Since_1908$Location, as.character)

#replace everything before the ",.*" with a " " 
Airplane_Crashes_and_Fatalities_Since_1908$Location <- gsub(".*,", "", Airplane_Crashes_and_Fatalities_Since_1908$Location)

#Remove white space
Airplane_Crashes_and_Fatalities_Since_1908$Location <- stri_enc_toutf8(Airplane_Crashes_and_Fatalities_Since_1908$Location)



#Convert Type column from a character to factor so that we can categorise it
Airplane_Crashes_and_Fatalities_Since_1908$Type <- as.factor(Airplane_Crashes_and_Fatalities_Since_1908$Type)

#Create a new Column 'Manufacturer' and insert the first word from eahc value in the Type columnn
Airplane_Crashes_and_Fatalities_Since_1908$Manufacturer <- gsub("([A-Za-z]+).*", "\\1", Airplane_Crashes_and_Fatalities_Since_1908$Type)


#Getting the manufacturers with highest plane accidents
manufacturersWithHighestPlaneAccidents <- tail(names(sort(table(Airplane_Crashes_and_Fatalities_Since_1908$Manufacturer))),10)

#Getting the manufacturer that crashed the most planes
highestCountPlaneAccidentsByManufacturer <- names(which.max(table(Airplane_Crashes_and_Fatalities_Since_1908$Manufacturer)))



#Create a table to hold the manufacturers and count the data, to be used in the function i.e. Globally
manufacturerDataTableFormat <- table(Airplane_Crashes_and_Fatalities_Since_1908$Manufacturer)

#User defined function that requires one argument (the name of the manufacturer), and this function
#will then return the amount of planes that manufacturer crashed
accidentOccurencesByManufacturerFunction <- function(manufacturer) 
{paste("This manufacturer crashed",
 manufacturerDataTableFormat[names(manufacturerDataTableFormat)==manufacturer],"planes",sep = " ")}






#Assign the fatalities column to a variable name
fatalitiesVector <- Airplane_Crashes_and_Fatalities_Since_1908$Fatalities

#Get the mean of the above vector and remove in NA fields if found
fatalitiesMean <- mean(fatalitiesVector,na.rm = TRUE)

#Format the above result so that it only return the value to two decimal places
fatalitiesMean <- format(round(fatalitiesMean), nsmall = 2)


#Assign the aboard column to a variable name
aboardVector <- Airplane_Crashes_and_Fatalities_Since_1908$Aboard

#Get the mean of the above vector and remove in NA fields if found
aboardMean <- mean(aboardVector, na.rm = TRUE)

#Format the above result so that it only return the value to two decimal places
aboardMean <- format(round(aboardMean), nsmall = 2)


#Convert both values as a numeric
aboardAmount <- as.numeric(aboardMean)
fatalitiesAmount <- as.numeric(fatalitiesMean)

#Print the values
print(paste("Average number of people onboard of a plane: ",aboardAmount))
print(paste("Average number of fatalities:",fatalitiesAmount))
print(paste("Difference between people aboard and fatalities:",aboardAmount-fatalitiesAmount))


#Obtaining some summary statistics of the passengers aboard and fatalities that I may need later
summaryStatisticsOfFatalitiesAndAboard <- 
  summary(Airplane_Crashes_and_Fatalities_Since_1908[c("Aboard","Fatalities")])

#Getting the standard deviation of the fatalities column, 
#so that we can see how much on average does the value differ from the mean
#Smaller the standard deviation, means that the values cluster closely to the mean and vice versa
var(fatalitiesVector)
fatalitiesSD <- sd(fatalitiesVector)


differenceBetweenSDandMean.Fatalities <- print(paste("Difference between standard deviation and the mean of the Fatalities: ",
            format(round(fatalitiesSD-fatalitiesAmount),nsmall = 2)))


#Scatter plot so that we can see the relationship between the passengers aboard and the fatalities
plot(x=fatalitiesVector,y=aboardVector,
     main = "Aboard vs Fatalities",xlab="Fatalities (Passenger)",ylab="Aboard (Passenger)")


#Exporting a dataset to possibly have some kind of version control.  For Developer use only
write.csv(Airplane_Crashes_and_Fatalities_Since_1908, "dataset6.csv")



