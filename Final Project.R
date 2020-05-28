#Final Project
#Emilly Korzendorfer

dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

install.packages("dplyr")
install.packages("caret")
library(caret)
library(dplyr)
library(kernlab)
library(ggplot2)

#Phase One...Mitigate Missing Data
dataset <- "smallSurveyWithComments.csv"
df <- read.csv(dataset, sep = ",")
list_na <- colnames(df)[ apply(df, 2, anyNA) ]
list_na
##Column Names with Missing Data: 
#1.Depature Delay in Min
#2.Arrival Delay in Min
#Flight Time in Min
average_missing <- apply(df[,colnames(df) %in% list_na], 2, mean, na.rm =  TRUE)
average_missing
#Means of Missing Cols
#1. 29.284
#2. 29.524
#3. 208.464

#Setting N/As to Mean Values
df$Departure.Delay.in.Minutes[which(is.na(df$Departure.Delay.in.Minutes))] <- 29.284
df$Arrival.Delay.in.Minutes[which(is.na(df$Arrival.Delay.in.Minutes))] <- 29.524
df$Flight.time.in.minutes[which(is.na(df$Flight.time.in.minutes))] <- 208.464
View(df)

#Phase Two...Summerize Varaibles Through Histograms
hist(df$Age)
#The ages of 40 to 50 have the highest frequency
#somewhat bell curve...somewhat ragged plateau
#normal distribution
hist(df$Price.Sensitivity)
#ragged plateau shape...no real pattern
#not neg or pos
hist(df$Year.of.First.Flight)
#skewed left
hist(df$Flights.Per.Year)
#skewed left
#therefore more positive
#uniform shape..with peak in the begining and lowest point at the end
hist(df$Loyalty)
#skewed left
#therefore more positive 
#again a uniform type shape
hist(df$Total.Freq.Flyer.Accts)
#skewed left...majority is 0
#positive histogram because of the skew to the left
hist(df$Shopping.Amount.at.Airport)
#skewed left..vary little shopping done at the airport
#therefore positive
hist(df$Eating.and.Drinking.at.Airport)
#skewed left..minimum eating and drinking done at the airport
#postive
hist(df$Scheduled.Departure.Hour)
#Skewed right
#bimodal features..meaning there is a spike around 7 and spike towards the end at 15
#negative features
hist(df$Departure.Delay.in.Minutes)
#skewed left
#postive
hist(df$Arrival.Delay.in.Minutes)
#skewed left
#positive
hist(df$Flight.time.in.minutes)
#bimodal type shape
#frequency doesnt project a pattern
hist(df$Flight.Distance)
hist(df$Likelihood.to.recommend)
#skewed right
#leaning towards negative

#########################Tables
table(df$Destination.City)
table(df$Origin.City)
table(df$Airline.Status)
table(df$Gender)
table(df$Type.of.Travel)
table(df$Class)
table(df$Partner.Name)
table(df$Origin.State)
table(df$Destination.State)

#Phase Three...Modeling
#number one
model <- train(Likelihood.to.recommend ~ Departure.Delay.in.Minutes ,
               data = df,
               method = "lm")
predict(model, df)

plot(df$Likelihood.to.recommend, df$Departure.Delay.in.Minutes, )
modelplot <- lm(formula = df$Departure.Delay.in.Minutes ~ Likelihood.to.recommend, data = df)
summary(modelplot)
abline(modelplot)

#number two
model2 <- train(Likelihood.to.recommend ~ Shopping.Amount.at.Airport  ,
               data = df,
               method = "lm")

predict(model2, df)

plot(df$Likelihood.to.recommend, df$Shopping.Amount.at.Airport, )
modelplot2 <- lm(formula = df$Shopping.Amount.at.Airport ~ Likelihood.to.recommend, data = df)
summary(modelplot2)
abline(modelplot2)

#number three
model3 <- train(Likelihood.to.recommend ~ Age  ,
                data = df,
                method = "lm")
model3
predict(model3, df)

plot(df$Likelihood.to.recommend, df$Age, )
modelplot3 <- lm(formula = df$Age ~ Likelihood.to.recommend, data = df)
summary(modelplot3)
abline(modelplot3)

#number four
model4 <- train(Likelihood.to.recommend ~ Eating.and.Drinking.at.Airport  ,
                data = df,
                method = "lm")
model4
predict(model4, df)

plot(df$Likelihood.to.recommend, df$Eating.and.Drinking.at.Airport, )
modelplot4 <- lm(formula = df$Eating.and.Drinking.at.Airport ~ Likelihood.to.recommend, data = df)
summary(modelplot4)
abline(modelplot4)

#number five
model5 <- train(Likelihood.to.recommend ~ Flight.Distance  ,
                data = df,
                method = "lm")
model5
predict(model5, df)

plot(df$Likelihood.to.recommend, df$Flight.Distance, )
modelplot5 <- lm(formula = df$Flight.Distance ~ Likelihood.to.recommend, data = df)
summary(modelplot5)
abline(modelplot5)

#number six
plot(df$Likelihood.to.recommend, df$Loyalty, )
modelplot6 <- lm(formula = df$Loyalty ~ Likelihood.to.recommend, data = df)
summary(modelplot6)
abline(modelplot6)

#number seven
plot(df$Likelihood.to.recommend, df$Flight.time.in.minutes, )
modelplot7 <- lm(formula = df$Flight.time.in.minutes ~ Likelihood.to.recommend, data = df)
summary(modelplot7)
abline(modelplot7)


plot(df$Age, df$Flights.Per.Year, )
modelplot7 <- lm(formula = df$Age ~ df$Flights.Per.Year, data = df)
summary(modelplot7)
abline(modelplot7)

plot(df$Departure.Delay.in.Minutes, df$Arrival.Delay.in.Minutes, )
modelplot7 <- lm(formula = df$Arrival.Delay.in.Minutes ~ df$Departure.Delay.in.Minutes, data = df)
summary(modelplot7)
abline(modelplot7)

#number eight
plot(df$Arrival.Delay.in.Minutes, df$Likelihood.to.recommend , )
modelplot8 <- lm(formula = df$Likelihood.to.recommend ~ df$Arrival.Delay.in.Minutes, data = df)
summary(modelplot8)
abline(modelplot8)


#Phase Four..Map Low Satisfaction Routes
library(tidyverse)
library(tm)

df2 <- df[df$Likelihood.to.recommend == 1 | df$Likelihood.to.recommend == 2 | df$Likelihood.to.recommend ==3,]

map <- borders("state", colour = "grey", fill = "white")
ggplot() + map +
  geom_curve(data=df2, aes(x=olong, y=olat, xend=dlong, yend=dlat),
             col="black", size = .5,
             curvature = .3) +
  geom_point(data=df2,
             aes(x=olong, y=olat),
             colour = "blue",
             size = 1.5) +
  geom_point(data=df2,
             aes(x=dlong, y=dlat),
             colour="red")+
  labs(title="Map of Lowest Satisfaction Reveiws")
map

#Phase Five....Sentiment Analysis

charVector <- df$freeText
posWords <- scan("positive-words.txt", character(0), sep="\n")
posWords <- posWords [-1:-34]
head(posWords)
negWords <- scan("negative-words.txt", character(0), sep="\n")
negWords <- negWords [-1:-34]
head(negWords)

head(charVector)
summary(charVector)

words.vec <- VectorSource(charVector)
words.corpus <- Corpus(words.vec)
words.corpus
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
tdm <- TermDocumentMatrix(words.corpus)
inspect(tdm)

m <- as.matrix(tdm)
wordCounts <- rowSums(m)

head(wordCounts)

wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)


length(wordCounts)
#2091 unique words

sum(wordCounts)
sumWords <- sum(wordCounts)


matchedP <- match(names(wordCounts), posWords, nomatch=0)
PosCounts <- wordCounts[which(matchedP !=0)]
length(PosCounts)
PosWords <- names(PosCounts)
SumPos<-sum(PosCounts)
SumPos
#658 positive words 

matchedN <- match(names(wordCounts), negWords, nomatch=0)
NegCount <- wordCounts[which(matchedN !=0)]
length(NegCount)
NegWords <- names(wordCounts)
SumNeg <- sum(NegCount)
SumNeg
#507 negative words

library(ggplot2)

matchplotpos <- data.frame(PosCounts)
matchplotpos$wordName <- rownames(matchplotpos)
posplot <- ggplot(data = matchplotpos, aes(x=reorder(wordName, x = PosCounts), y= PosCounts)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust=2, size=7)) 
posplot

matchedplotneg <- data.frame(NegCount)
matchedplotneg <- head(matchedplotneg, n= 20)
matchedplotneg$wordName <- rownames(matchedplotneg)
negmatchplot <- ggplot(data = matchedplotneg, aes(x=reorder(wordName, NegCount), y=NegCount))+
  geom_col() +
  theme(axis.text = element_text(angle = 90, hjust=1, size=10))
negmatchplot

matchedplotpos = data.frame(PosCounts[which(PosCounts != 1)])
matchedplotpos2$wordName <- rownames(matchedplotpos)

posbarplot <- ggplot(data = matchedplotposs, aes(x=reorder(wordName,(PosCounts[which(pCounts != 1)])), y= (PosCounts[which(pCounts != 1)]))) +
  geom_col() +
  theme(axis.text = element_text(angle=90, hjust=1, size=10)) 
posbarplot  

matchedplotneg = data.frame(NegCounts[which(NegCounts !=1)])
matchedplotnegf2$wordName <- rownames(mathcedplotneg
)
negbarplot <- ggplot(dat = matchedplotneg, aes(x=reorder(wordName, (NegCounts[which(NegCounts !=1)])), y= (nCounts[which(NegCounts !=1)]))) +
  geom_col() +
  theme(axis.text = element_text(angle=90, hjust=1, size=10))
negbarplot

lengthwords <- length(wordCounts)


ratioPos <- SumPos/lengthwords
ratioPos #0.3

ratioNeg <- SumNeg/lengthwords
ratioNeg #0.24

###########################################################################################


