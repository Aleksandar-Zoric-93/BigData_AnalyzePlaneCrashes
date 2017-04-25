#import/install libraries and load dataset
install.packages("formattable")
install.packages("readr")
install.packages("stringr")
install.packages("stringi")
install.packages("ggplot2")
install.packages("caret")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("ggmap")

library(readr)
library(stringr)
library(stringi)
library(formattable)
library(ggplot2)
library(caret)
library(tm)
library(SnowballC)
library(wordcloud)
library(ggmap)
Airplane_Crashes_and_Fatalities_Since_1908 <- read_csv("C:/Users/Aleks/Desktop/Big Data/Airplane_Crashes_and_Fatalities_Since_1908.csv")


#New Section______________________________________________________________________________________________________________



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



#New Section______________________________________________________________________________________________________________

#Execute only if the data set is the default.

# #Converting all values to a whole number from both columns
# as.integer(Airplane_Crashes_and_Fatalities_Since_1908$Aboard)
# as.integer(Airplane_Crashes_and_Fatalities_Since_1908$Fatalities)
# as.integer(Airplane_Crashes_and_Fatalities_Since_1908$Ground)
# 
# #Replacing all values with NA in the summary column with a more meanigful message
# Airplane_Crashes_and_Fatalities_Since_1908$Summary <- as.character(Airplane_Crashes_and_Fatalities_Since_1908$Summary)
# Airplane_Crashes_and_Fatalities_Since_1908$Summary <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Summary),
# 'No information on how the accident occured', Airplane_Crashes_and_Fatalities_Since_1908$Summary)
# 
# #Replacing all values with NA in the time column with a more meanigful message
# Airplane_Crashes_and_Fatalities_Since_1908$Time <- as.character(Airplane_Crashes_and_Fatalities_Since_1908$Time)
# Airplane_Crashes_and_Fatalities_Since_1908$Time <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Time),
# 'Unknown time', Airplane_Crashes_and_Fatalities_Since_1908$Time)
# 
# #Replacing all values with NA in the Flight # column with a more meanigful message
# Airplane_Crashes_and_Fatalities_Since_1908$'Flight #' <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$'Flight #'),
# 'Unknown Flight #', Airplane_Crashes_and_Fatalities_Since_1908$'Flight #')
# 
# #Replacing all values with NA in the Route column with a more meanigful message
# Airplane_Crashes_and_Fatalities_Since_1908$Route <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Route),
# 'Unknown Route', Airplane_Crashes_and_Fatalities_Since_1908$Route)
# 
# #Replacing all values with NA in the Type column with a more meanigful message
# Airplane_Crashes_and_Fatalities_Since_1908$Type <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Type),
# 'Unknown Type', Airplane_Crashes_and_Fatalities_Since_1908$Type)
# 
# #Replacing all values with NA in the Registration column with a more meanigful message
# Airplane_Crashes_and_Fatalities_Since_1908$Registration <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Registration),
# 'Unknown Registration', Airplane_Crashes_and_Fatalities_Since_1908$Registration)
# 
# #Replacing all values with NA in the Operator column with a more meanigful message
# Airplane_Crashes_and_Fatalities_Since_1908$Operator <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Operator),
# 'Unknown Operator', Airplane_Crashes_and_Fatalities_Since_1908$Operator)
# 
# #Replacing all values with NA in the Date column with a more meanigful message
# Airplane_Crashes_and_Fatalities_Since_1908$Date <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Date),
# 'Unknown Date', Airplane_Crashes_and_Fatalities_Since_1908$Date)
# 
# #Replacing all values with NA in the Location column with a more meanigful message
# Airplane_Crashes_and_Fatalities_Since_1908$Location <- ifelse(is.na(Airplane_Crashes_and_Fatalities_Since_1908$Location),
# 'Unknown Location', Airplane_Crashes_and_Fatalities_Since_1908$Location)



#New Section______________________________________________________________________________________________________________



#Delete the cn/cl column as it does not appear to be hugely important for what I am trying to achieve
#Also there is only about 400 out of 5268 rows of actual data within that column
Airplane_Crashes_and_Fatalities_Since_1908$'cn/In' <- NULL

#Convert to String
Airplane_Crashes_and_Fatalities_Since_1908$Location <- sapply(Airplane_Crashes_and_Fatalities_Since_1908$Location, as.character)

#replace everything before the ",.*" with a " " 
Airplane_Crashes_and_Fatalities_Since_1908$Location <- gsub(".*,", "", Airplane_Crashes_and_Fatalities_Since_1908$Location)

#Remove white space
Airplane_Crashes_and_Fatalities_Since_1908$Location <- stri_enc_toutf8(Airplane_Crashes_and_Fatalities_Since_1908$Location)



#New Section______________________________________________________________________________________________________________



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


#New Section______________________________________________________________________________________________________________



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
aboardAmountAverage <- as.numeric(aboardMean)
fatalitiesAmountAverage <- as.numeric(fatalitiesMean)

#Print the values
print(paste("Average number of people onboard of a plane: ",aboardAmountAverage))
print(paste("Average number of fatalities:",fatalitiesAmountAverage))
print(paste("Difference between people aboard and fatalities:",aboardAmountAverage-fatalitiesAmountAverage))


#New Section______________________________________________________________________________________________________________



#Obtaining some summary statistics of the passengers aboard and fatalities that I may need later
summaryStatisticsOfFatalitiesAndAboard <- 
  summary(Airplane_Crashes_and_Fatalities_Since_1908[c("Aboard","Fatalities")])

#Getting the standard deviation of the fatalities column, 
#so that we can see how much on average does the value differ from the mean
#Smaller the standard deviation, means that the values cluster closely to the mean and vice versa
var(fatalitiesVector)
fatalitiesSD <- sd(fatalitiesVector)


differenceBetweenSDandMean.Fatalities <- print(paste("Difference between standard deviation and the mean of the Fatalities: ",
            format(round(fatalitiesSD-fatalitiesAmountAverage),nsmall = 2)))




#New Section______________________________________________________________________________________________________________



#Create a new column to determine the survivor to fatalities ratio on each accident that occured.  If the
#ratio is 1.00, everybody died while if it is 0.7, this means that 70% of the passengers died.
Airplane_Crashes_and_Fatalities_Since_1908$SurvivorToFatalitiesRatio <- with(Airplane_Crashes_and_Fatalities_Since_1908,
ifelse(Airplane_Crashes_and_Fatalities_Since_1908$Aboard >= Airplane_Crashes_and_Fatalities_Since_1908$Fatalities, 
Airplane_Crashes_and_Fatalities_Since_1908$Fatalities/Airplane_Crashes_and_Fatalities_Since_1908$Aboard, NA))

#To clarify, I created another column which also gives us a percentage of the amount of passengers that died.
#I also formatted it to 2 decimal places.
Airplane_Crashes_and_Fatalities_Since_1908$DeathRatePercentage <- 
Airplane_Crashes_and_Fatalities_Since_1908$SurvivorToFatalitiesRatio * 100

Airplane_Crashes_and_Fatalities_Since_1908$DeathRatePercentage <- 
format(round( Airplane_Crashes_and_Fatalities_Since_1908$DeathRatePercentage),nsmall = 2)


#Here I just want to combine my values from the DeathRatePercentage column into a vector/list so I can work with them
DeathRatePercentageCharacters <- c(Airplane_Crashes_and_Fatalities_Since_1908$DeathRatePercentage)

#This line counts all accidents where there was 100% fatalities i.e. everybody onboard died
numberOfAccidentsWithAllFatalities <- paste("Number of accidents where all passengers died: ",
                                      length(which(DeathRatePercentageCharacters=="100.00")))

#Remove white space from all strings within this variable.  Was causing an issue in the function
DeathRatePercentageCharacters <- gsub(" ", "", DeathRatePercentageCharacters, fixed = TRUE)





#New Section______________________________________________________________________________________________________________

#A user defined function which returns the number of accidents occured based on the input, 
#which is the death percentage.  The input is a string, put quotes around the parameter when called. 
countAccidentsWithChosenDeathRatePercentage <- function(DeathRatePercent) 
paste("Number of accidents: ",length(which(DeathRatePercentageCharacters==DeathRatePercent)))

#Finding the average percentage of all accidents where all the passengers died
averageOfTotalFatalities <- 
length(which(DeathRatePercentageCharacters=="100.00"))/nrow(Airplane_Crashes_and_Fatalities_Since_1908)

#Formatting the above result to 2 decimal places and adding a message to display the result
averageOfTotalFatalities <- 
paste("The average percentage of total fatalities across all accidents recorded is: ",
formattable(averageOfTotalFatalities,digits = 2, format="f"),"%", sep = " ")


#New Section______________________________________________________________________________________________________________


#Formatting certain columns to 2 decimal places as I have no need for any values after that
Airplane_Crashes_and_Fatalities_Since_1908$SurvivorToFatalitiesRatio <- 
  formattable(Airplane_Crashes_and_Fatalities_Since_1908$SurvivorToFatalitiesRatio,digits = 2, format="f")

Airplane_Crashes_and_Fatalities_Since_1908$Aboard <- 
  formattable(Airplane_Crashes_and_Fatalities_Since_1908$Aboard,digits = 2, format="f")

Airplane_Crashes_and_Fatalities_Since_1908$Fatalities <- 
  formattable(Airplane_Crashes_and_Fatalities_Since_1908$Fatalities,digits = 2, format="f")

Airplane_Crashes_and_Fatalities_Since_1908$Ground <- 
  formattable(Airplane_Crashes_and_Fatalities_Since_1908$Ground,digits = 2, format="f")


#New Section______________________________________________________________________________________________________________

#Create a dataset of all crashes involving Douglas manufacturer i.e. highest ranking crash manufacturer
allCrashesInvolvingDouglas <- 
Airplane_Crashes_and_Fatalities_Since_1908[Airplane_Crashes_and_Fatalities_Since_1908$Manufacturer %in% 'Douglas',]

#Select rows only where the ground deaths are equal to 0.  Otherwise the data is not useful to us
#Because we do not plot the ground deaths for each aircraft
allCrashesInvolvingDouglas<-allCrashesInvolvingDouglas[(allCrashesInvolvingDouglas$Ground=="0"),]
View(allCrashesInvolvingDouglas)

#Assign the fatalities column to a variable name
fatalitiesVectorForDouglas <- allCrashesInvolvingDouglas$Fatalities

#Assign the aboard column to a variable name
aboardVectorForDouglas <- allCrashesInvolvingDouglas$Aboard

#A plot so that we can see the relationship between the passengers aboard and the fatalities for the Douglas
#manufacturer.  This does not include ground fatalities, only fatalities of passengers on the plane as this
#type of data proves to be most useful
plotAboardvsFatalitiesDouglas <- ggplot(allCrashesInvolvingDouglas, aes(x=fatalitiesVectorForDouglas, y=aboardVectorForDouglas,
fill=fatalitiesVectorForDouglas)) + geom_bar(stat="identity") + xlab("Fatalities") + ylab("Passengers Aboard")

#Change title of scale
plotAboardvsFatalitiesDouglas <- plotAboardvsFatalitiesDouglas + guides(fill=guide_legend(title="Fatalities Scale"))

#Change overall title
plotAboardvsFatalitiesDouglas <- plotAboardvsFatalitiesDouglas + ggtitle("Aboard vs Fatalities for the Douglas manufacturer")

#New Section______________________________________________________________________________________________

#Find top 10 planes that crashed the most frequently
planesWhichCrashedTheMost <- tail(names(sort(table(allCrashesInvolvingDouglas$Type))), 10)

#Create slices for the pie chart which consists of the number the 10 Douglas planes have crashed
slices <- c(length(which(allCrashesInvolvingDouglas$Type=="Douglas DC-3A")),
            length(which(allCrashesInvolvingDouglas$Type=="Douglas DC-3C")),
            length(which(allCrashesInvolvingDouglas$Type=="Douglas DC-6")),
            length(which(allCrashesInvolvingDouglas$Type=="Douglas C-47-DL")),
            length(which(allCrashesInvolvingDouglas$Type=="Douglas DC-6B")),
            length(which(allCrashesInvolvingDouglas$Type=="Douglas C-47B")),
            length(which(allCrashesInvolvingDouglas$Type=="Douglas DC-4")),
            length(which(allCrashesInvolvingDouglas$Type=="Douglas C-47")),
            length(which(allCrashesInvolvingDouglas$Type=="Douglas C-47A")),
            length(which(allCrashesInvolvingDouglas$Type=="Douglas DC-3")))

#Create labels for the pie chart which consists of the name of each of the top 10 Douglas planes that crashed
lbls <- c(planesWhichCrashedTheMost)

cols = c("black","blue", "brown", "red","darkcyan","cyan","chocolate","coral", "chartreuse", "cadetblue")

#And finally, graph a pie chart with the above values and apply some extra features so that it is 
#displayed in a neater manner
pie(slices, labels = lbls,col=cols, main="Top 10 Douglas Planes that Crashed",radius = 0.8)
slicesAsChr <- as.character(slices)

#Legend to display the number of crashes per plane type
legend("bottomleft", sort(unique(lbls)),inset=.02, title="No# of Crashes", 
fill=cols,c(slicesAsChr),cex=0.8, pch = 19)

#New Section______________________________________________________________________________________________

#Find the top 10 locations of Douglas Crashes
top10LocationsOfDouglasCrashes <- tail(names(sort(table(allCrashesInvolvingDouglas$Location))), 10)

#Create slices for the pie chart which consists of the number of the top 10 locations Douglas
#planes have crashed at
slicesLocation <- c(length(which(allCrashesInvolvingDouglas$Location=="Mexico")),
                                 length(which(allCrashesInvolvingDouglas$Location=="Alaska")),
                                 length(which(allCrashesInvolvingDouglas$Location=="France")),
                                 length(which(allCrashesInvolvingDouglas$Location=="California")),
                                 length(which(allCrashesInvolvingDouglas$Location=="Philippines")),
                                 length(which(allCrashesInvolvingDouglas$Location=="China")),
                                 length(which(allCrashesInvolvingDouglas$Location=="Canada")),
                                 length(which(allCrashesInvolvingDouglas$Location=="Brazil")),
                                 length(which(allCrashesInvolvingDouglas$Location=="India")),
                                 length(which(allCrashesInvolvingDouglas$Location=="India")),
                                 length(which(allCrashesInvolvingDouglas$Location=="Colombia")))

#Labels for the pie chart of the top 10 crash locations of Douglas planes
lblsForLocation <- c(top10LocationsOfDouglasCrashes)

#Create pie chart which is colour cordinated to display top 10 crash locations for Douglas
pie(slicesLocation, labels = lblsForLocation,col=cols, main="Top 10 Locations Douglas planes have Crashed",radius = 0.8)
slicesAsChrLocation <- as.character(slicesLocation)

#Legend to display the number of crashes in each location
legend("bottomleft", sort(unique(lbls)),inset=.02, title="No# of Crashes", 
       fill=cols,c(slicesAsChrLocation),cex=0.8, pch = 19)

#New Section______________________________________________________________________________________________

#Find the top 10 crash locations overall
top10CrashLocationOverall <- tail(names(sort(table(Airplane_Crashes_and_Fatalities_Since_1908$Location))), 10)

#Create slices for the pie chart which consists of the number of the top 10 locations 
#planes have crashed overall
slicesLocationOverall <- c(length(which(allCrashesInvolvingDouglas$Location=="China")),
                    length(which(allCrashesInvolvingDouglas$Location=="India")),
                    length(which(allCrashesInvolvingDouglas$Location=="England")),
                    length(which(allCrashesInvolvingDouglas$Location=="France")),
                    length(which(allCrashesInvolvingDouglas$Location=="California")),
                    length(which(allCrashesInvolvingDouglas$Location=="Canada")),
                    length(which(allCrashesInvolvingDouglas$Location=="Canada")),
                    length(which(allCrashesInvolvingDouglas$Location=="Colombia")),
                    length(which(allCrashesInvolvingDouglas$Location=="Russia")),
                    length(which(allCrashesInvolvingDouglas$Location=="Alaska")),
                    length(which(allCrashesInvolvingDouglas$Location=="Brazil")))

#Labels for the pie chart of the top 10 crash locations overall
lblsForLocationOverall <- c(top10CrashLocationOverall)

#Create pie chart which is colour cordinated to display top 10 crash locations overall
pie(slicesLocationOverall, labels = lblsForLocationOverall,col=cols, main="Top 10 Locations of Plane Crashes Overall",radius = 0.8)
slicesAsChrLocationOverall <- as.character(slicesLocationOverall)

#Legend to display the number of crashes in each location
legend("bottomleft", sort(unique(lbls)),inset=.02, title="No# of Crashes Overall", 
       fill=cols,c(slicesAsChrLocationOverall),cex=0.8, pch = 19)


#New Section______________________________________________________________________________________________

#Plot the two pie charts side by side to compare
par(mfrow = c(1,2))

pie(slicesLocation, labels = lblsForLocation,col=cols, main="Top 10 Locations Douglas planes have Crashed",radius = 0.8)
pie(slicesLocationOverall, labels = lblsForLocationOverall,col=cols, main="Top 10 Locations of Plane Crashes Overall",radius = 0.8)

#New Section______________________________________________________________________________________________

#A function that return the dates that the specfied amount of fatalities occured
viewonWhichDatesFatalitiesOccured <- function(fatalitiesAmount)
{allCrashesInvolvingDouglas[allCrashesInvolvingDouglas$Fatalities == fatalitiesAmount, "Date"]}

#Get the top 10 crashes where the most fatalities occured
tail(names(table(allCrashesInvolvingDouglas$Fatalities)), 10)

#Create a corpus of the summary column and execute some functions to do some text clean up
#i.e. putting all words to lower case, converting it to a plain text document etc.
corpus = VCorpus(VectorSource(Airplane_Crashes_and_Fatalities_Since_1908$Summary))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
docTermMatrix = DocumentTermMatrix(corpus)
docTermMatrix = removeSparseTerms(docTermMatrix, 0.95)

#Gather the top 50 most frequently used terms and store it in a variable to use later
frequentTerms = findFreqTerms(docTermMatrix,50)

#Print the terms via a for loop
print('50 most frequently used terms:')
for( i in frequentTerms)
 cat(i, " ")

#Must call this function twice since I used the par function earlier to combine plots
#Calling this function just puts us back to the default plot creation
dev.off()
dev.off()

#Graph a word cloud using the terms I found above
wordcloud(corpus, max.words = 50, random.order = FALSE)

#New Section______________________________________________________________________________________________

#I am doing the same as above here but for the Douglas manufacturer.  I will then compare the 50 most frequent terms
#from both the Douglas crashes and crashes overall, so that we can see the comparison.
#Create a corpus of the summary column and execute some functions to do some text clean up
#i.e. putting all words to lower case, converting it to a plain text document etc.
corpusDouglas = VCorpus(VectorSource(allCrashesInvolvingDouglas$Summary))
corpusDouglas = tm_map(corpusDouglas, tolower)
corpusDouglas = tm_map(corpusDouglas, PlainTextDocument)
corpusDouglas = tm_map(corpusDouglas, removePunctuation)
corpusDouglas = tm_map(corpusDouglas, removeWords, stopwords("english"))
docTermMatrixDouglas = DocumentTermMatrix(corpusDouglas)
docTermMatrixDouglas = removeSparseTerms(docTermMatrixDouglas, 0.95)

#Gather the top 50 most frequently used terms and store it in a variable to use later
frequentTermsDouglas = findFreqTerms(docTermMatrixDouglas,50)

#Print the terms via a for loop
print('50 most frequently used terms in Douglas crashes:')
for( i in frequentTermsDouglas)
  cat(i, " ")

#Graph a word cloud using the terms I found above
wordcloud(corpusDouglas, max.words = 50, random.order = FALSE)

#Add the two word clouds side by side for comparison
par(mfrow = c(1,2))
wordcloud(corpus, max.words = 50, random.order = FALSE)
wordcloud(corpusDouglas, max.words = 50, random.order = FALSE)

dev.off()


#New Section______________________________________________________________________________________________

#This section will determine the top 10 locations Douglas DC-3 planes has crashed at and map the 
#points on a map.  By doing so, we may be able to see if there is any pattern in terms of
#where it crashed in the world.
allDC3Crashes <- allCrashesInvolvingDouglas[allCrashesInvolvingDouglas$Type == "Douglas DC-3",]
locations <- tail(names(sort(table(allDC3Crashes$Location))), 10)
CoordsOfTop10DC3Crashes <- geocode(c(tail(names(sort(table(allDC3Crashes$Location))), 10)))
CoordsOfTop10DC3Crashes <- cbind(locations, CoordsOfTop10DC3Crashes)


CoordsLon <- CoordsOfTop10DC3Crashes$lon
CoordsLat <- CoordsOfTop10DC3Crashes$lat

mapWorld <- borders("world", colour="gray50", fill="gray50")
top10DC3CrashLocation <- ggplot() + mapWorld + ggtitle("Top 10 locations Douglas DC-3 Plane crashed")
top10DC3CrashLocation <- top10DC3CrashLocation + geom_point(aes(x=CoordsLon, y=CoordsLat) ,color="red", size=3) + 
  geom_text(data=CoordsOfTop10DC3Crashes, inherit.aes=F, aes(x=CoordsOfTop10DC3Crashes$lon, y=CoordsOfTop10DC3Crashes$lat, 
  label=CoordsOfTop10DC3Crashes$locations), vjust=1.5, colour="blue", alpha=.5)

#Exporting a dataset to possibly have some kind of version control.  For Developer use only
write.csv(Airplane_Crashes_and_Fatalities_Since_1908, "dataset7.csv")



