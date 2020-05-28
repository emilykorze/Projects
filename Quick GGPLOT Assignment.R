#################
#IST 387
#
#Emily Korzendorfer

dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

readStates <- function(){
  urlToRead <- "https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
  dfStates <- read.csv(url(urlToRead), stringsAsFactors = FALSE) 
  dfStates
  
  View(dfStates)
  head(dfStates)
  tail(dfStates)
  
  dfStates[9:59,]
  dfStates <- dfStates[9:59,]
  
  dfStates[,1:5]
  dfStates <- dfStates[,1:5]
  rownames(dfStates) <- 1:51
  
  newNames <- colnames(dfStates)
  newNames[1] <- "stateName"
  newNames[2] <- "Census"
  newNames[3] <- "Estimated"
  newNames[4] <- "Pop2010"
  newNames[5] <- "Pop2011"
  colnames(dfStates) <- newNames
  colnames(dfStates)
  dfStates$Census <- gsub(",", "", dfStates$Census)
  dfStates$Estimated <- gsub(",", "", dfStates$Estimated)
  dfStates$Pop2010 <- gsub(",", "", dfStates$Pop2010)
  dfStates$Pop2011 <- gsub(",", "", dfStates$Pop2011)
  dfStates$stateName <- gsub("\\.", "", dfStates$stateName)
  dfStates$Census <- as.numeric(dfStates$Census)
  dfStates$Estimated <- as.numeric(dfStates$Estimated)
  dfStates$Pop2010 <- as.numeric(dfStates$Pop2010)
  dfStates$Pop2011 <- as.numeric(dfStates$Pop2011)
  
  #mean(dfStates$Census) #6053834  #mean(dfStates$Estimated) #6053834
  #mean(dfStates$Pop2010) #6065298
  #mean(dfStates$Pop2011) #6109645
  return(dfStates)
}
states <- readStates()
arrests <- USArrests
arrests$stateName <- rownames(arrests)
mergeDF <- merge(states, arrests, by = "stateName")
library(ggplot2)

ggplot(mergeDF, aes(x=Pop2011)) + geom_histogram(binwidth = 10000000, color="black", fill="white")
#ggplot to create a histogram from Pop2011 and mergeDF dataframe with the x axis as Pop2011
ggplot(mergeDF, aes(x=Murder)) + geom_histogram()
#uses ggplot to create a histogram on Murder from the mergeDF dataframe with the x axis as Murder
ggplot(mergeDF, aes(x=Assault)) + geom_histogram()
#uses ggplot to create a histogram on Assault from the mergeDF dataframe with the x axis as Assault
ggplot(mergeDF, aes(x=Rape)) + geom_histogram()
#uses ggplot to create a histogram on Rape from the mergeDf data frame with the x axis as Rape

ggplot(mergeDF, aes(y=Pop2011)) + geom_boxplot()
ggplot(mergeDF, aes(y=Murder)) + geom_boxplot()


mergeDF$numMurders <- (mergeDF$Murder * mergeDF$Pop2011)/100000

ggplot(mergeDF) + aes(x=stateName, y=Murder) + geom_col() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  ggtitle("Murders by State")

ggplot(mergeDF) + aes(x=reorder(stateName, Murder), y=Murder) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  ggtitle("Murders by State")

ggplot(mergeDF) + aes(x=reorder(stateName, Murder), y=Murder) + 
  geom_col(aes(color=UrbanPop, fill = UrbanPop)) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  ggtitle("Murders by state in order")

ggplot(mergeDF) + aes(x=Pop2011, y=UrbanPop) +
  geom_point(aes(color=Murder, size=Murder)) +
  ggtitle("2011 Population by Urban Population")
