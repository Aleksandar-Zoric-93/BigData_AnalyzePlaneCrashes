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




#Exporting a dataset to possibly have some kind of version control.  For Developer use only
write.csv(Airplane_Crashes_and_Fatalities_Since_1908, "dataset4.csv")
