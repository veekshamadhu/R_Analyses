##Final Project - STAT 309
#loading data
setwd("/Users/Veeksha/Documents/Wellesley/Spring '21/STAT 309/final project 309")
alldata <-read.csv(file="all_Data_Cleaned.csv", header = TRUE)

library(party)
library(dplyr)
library(tidyr)
library(ggplot2)

#understanding data
dim(alldata)
summary(alldata)
head(alldata)

#Defining Treatment
alldata$TREAT <- NA
alldata$TREAT[alldata$Mask.Use..Always >= 0.50]<- 1
alldata$TREAT[alldata$Mask.Use..Always < 0.50] <- 0
alldata$TREAT<-as.numeric(alldata$TREAT)

#Variables for mask mandate
alldata$num_mandate<- NA

alldata$num_mandate[alldata$Mandate.Specification.2 == "all public places" |
                                       alldata$Mandate.Specification.2 =="Public and retail places"|
                                       alldata$Mandate.Specification.2 =="Indoor, outdoor, public places and public transportations"|
                                       alldata$Mandate.Specification.2 =="Anyone in any public space throughout the State, including visitors, will need to wear a mask. This includes using public transportation, public facing work environments, when patronizing businesses, or interacting with others in any generally publicly accessible space."|
                                       alldata$Mandate.Specification.2 =="in all types of businesses"|
                                       alldata$Mandate.Specification.2 =="Public places"|
                                       alldata$Mandate.Specification.2=="public places and where social distancing impossible"|
                                       alldata$Mandate.Specification.2 =="People are now required to wear face coverings in public spaces, whether inside or outside, where physical distancing of 6 feet is not possible."] <- "2"

alldata$num_mandate[alldata$Mandate.Specification.2 == "most public places" |
                                       alldata$Mandate.Specification.2 =="Public transportation, retail, food services"|
                                       alldata$Mandate.Specification.2 =="indoor places and indoor businesses, workers need to wear one if outdoor not social distancing impossible"|
                                       alldata$Mandate.Specification.2 =="Indoor public spaces and at any organized outdoor activity where social distancing cannot be maintained, including markets, weddings and parties"|
                                       alldata$Mandate.Specification.2 =="From 04/08/2020, face coverings must be used indoors where social distancing is not applicable, exceptions apply; from 07/08/2020, individuals must wear face coverings in outdoor public spaces when social distancing is not possible;\nin indoor spaces open to the public, including retail, recreational, and entertainment businesses, government buildings open to the public, and on public transportation; and in indoor commercial spaces closed to the public, including office buildings, when individuals are in prolonged proximity to others. "|
                                       alldata$Mandate.Specification.2 =="Face masks are now required when exercising, even outdoors."|
                                       alldata$Mandate.Specification.2 =="Anyone over age 2 who can medically tolerate a face covering when in a public place and unable to maintain social distancing."|
                                       alldata$Mandate.Specification.2 =="Applies to people age 10 and older when in public indoor spaces and outdoors when unable to maintain 6-foot social distancing." |
                                       alldata$Mandate.Specification.2 =="On July 1, the governor required all Oregonians to wear a face mask while inside public spaces. Last week, she expanded the rule to apply to all outdoor activities when social distancing could not be maintained. " |
                                       alldata$Mandate.Specification.2 =="Most people aged 2 and up to cover their faces in public places, indoors and out." |
                                       alldata$Mandate.Specification.2 =="Requires face coverings for people over age 2 in retail outlets, and in other public spaces unless they can easily, continuously, and measurably maintain at least 6 feet of distance from other people" |
                                       alldata$Mandate.Specification.2 =="counties with more than 20 coronavirus cases to wear a face covering over the nose and mouth while in a business or other building open to the public, as well as outdoor public spaces, whenever social distancing is not possible." |
                                       alldata$Mandate.Specification.2 =="Masks for people age 10 and up in indoor settings to which the public has access." |
                                       alldata$Mandate.Specification.2 =="Mask use in indoor public settings and outdoors when 6-foot distancing cannot be maintained, for people age 5 and older"|
                                       alldata$Mandate.Specification.2 =="Mask use in indoor public settings and outdoors when 6-foot distancing cannot be maintained, for people age 5 and older"|
                                       alldata$Mandate.Specification.2 == "Requires people age 9 and up to wear face coverings in confined indoor spaces when unable to adequately social distance, other than at home"]<- "1"

alldata$num_mandate[alldata$Mandate.Specification.2 == "" ] <- "0"

alldata$num_mandate<-as.numeric(alldata$num_mandate)
summary(alldata$num_mandate)
alldata$Mandate.Specification.2[is.na(alldata$num_mandate)]


#pscores
subsetdata <- data.frame(
                alldata$Cases.July.2020,
                alldata$Deaths.July.2020,
                alldata$X2019.Median.Income,
                alldata$X2019.Median.Income.LB,
                alldata$X2019.Median.Income.UB,
                alldata$num_mandate,
                alldata$TREAT)

output <- ctree(alldata.TREAT~.,data=subsetdata)
plot(output)
pscores<-predict(output)[,1]
subsetdata<-data.frame(subsetdata,pscores)
table(subsetdata$pscores,subsetdata$alldata.TREAT)
length(table(pscores))

#graphing
par(mfrow = c(2,1))
hist(pscores[subsetdata$alldata.TREAT == 1],
     main = "Active",
     xlab = "Est. pscores",
     ylim = c(0,500),
     xlim = c(0, 1),
     col="blue",
     breaks=5)
hist(pscores[subsetdata$alldata.TREAT == 0],
     main = "Control",
     ylim = c(0,500),
     xlab = "Est. pscores",
     col="red",
     breaks=5)

#discarding units
dim(subsetdata)
subsetdata<-subsetdata[pscores>=.15,]
dim(subsetdata)

#making subclasses
stable<-table(subsetdata$alldata.num_mandate,subsetdata$alldata.TREAT)
stable

#mean by subclass
XT2<-apply(subsetdata[subsetdata$alldata.num_mandate==2 & subsetdata$alldata.TREAT==1,1:5],2,mean)
XC2<-apply(subsetdata[subsetdata$alldata.num_mandate==2 & subsetdata$alldata.TREAT==0,1:5],2,mean)

XT1<-apply(subsetdata[subsetdata$alldata.num_mandate==1 & subsetdata$alldata.TREAT==1,1:5],2,mean)
XC1<-apply(subsetdata[subsetdata$alldata.num_mandate==1 & subsetdata$alldata.TREAT==0,1:5],2,mean)

XT0<-apply(subsetdata[subsetdata$alldata.num_mandate==0 & subsetdata$alldata.TREAT==1,1:5],2,mean)
XC0<-apply(subsetdata[subsetdata$alldata.num_mandate==0 & subsetdata$alldata.TREAT==0,1:5],2,mean)

#graphing
## july covid cases
julycasesa <- c(XT2[1], XT1[1], XT0[1])
julycasesc <- c(XC2[1], XC1[1], XC0[1])
data <- data.frame(julycasesa,julycasesc)
names(data) <- c("Active Treatment","Control Treatment")

barplot(height=as.matrix(data), main="Average COVID cases in July 2020", 
        ylab="COVID cases", 
        beside=TRUE,
        ylim = c(0,2000),
        col=rainbow(3))
#legends
legend("topright", c("subclass2","subclass1","subclass0"), cex=1.0, bty="n",
       fill=rainbow(3))

## Median Income
medint <- c(XT2[3], XT1[3], XT0[3])
medinc <- c(XC2[3], XC1[3], XC0[3])
data2 <- data.frame(medint,medinc)
names(data2) <- c("Active Treatment","Control Treatment")

barplot(height=as.matrix(data2), main="Median Income of Counties in 2019", 
        ylab="Income ($)", 
        beside=TRUE,
        ylim = c(0,65000),
        col=rainbow(3))
#Add legends
legend("topright", c("subclass2","subclass1","subclass0"), cex=1.0, bty="n",
       fill=rainbow(3))

## Deaths
deatht <- c(XT2[2], XT1[2], XT0[2])
deathc <- c(XC2[2], XC1[2], XC0[2])
data3 <- data.frame(deatht,deathc)
names(data3) <- c("Active Treatment","Control Treatment")

barplot(height=as.matrix(data3), main="Average COVID deaths in July 2020", 
        ylab="COVID deaths", 
        beside=TRUE,
        ylim = c(0,100),
        col=rainbow(3))
#Legends
legend("topright", c("subclass2","subclass1","subclass0"), cex=1.0, bty="n",
       fill=rainbow(3))

#Neyman estimate
t.test(subsetdata$alldata.Deaths.July.2020~subsetdata$alldata.TREAT)
