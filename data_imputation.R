#Reading data file
setwd("/Users/Veeksha/Desktop/github")
titanic<-read.csv(file="titanic3.csv", header = TRUE)

#install.packages("PASWR")
library(PASWR)

dim(titanic3)
head(titanic3)
colnames(titanic3)
summary(titanic3)

#looking for missing values in ticket
mean(is.na(titanic$ticket))

#age is normally distributed
titanic$randomimputation<-titanic$age
set.seed(58483)
titanic$randomimputation[is.na(titanic$randomimputation)]<- 
  sample(titanic$randomimputation[!is.na(titanic$randomimputation)], 263, replace = TRUE, prob= NULL)
summary(titanic$randomimputation)

#Histogram of missing ages
titanic$meanimputation<-titanic$age
titanic$meanimputation[is.na(titanic$meanimputation)]<-mean(titanic$meanimputation, na.rm = TRUE)
summary(titanic$meanimputation)
table(titanic$meanimputation)
hist(titanic$meanimputation,
     col="white",
     main="Histogram of Imputed Ages",
     xlab="Ages")

mean(titanic$meanimputation[is.na(titanic$randomimputation)], 263, replace= TRUE, prob=NULL)
agenew<-mean(titanic$meanimputation, na.rm = TRUE)
summary(agenew)

# plot of age and survival
par(las=1)
boxplot(titanic$age)
boxplot(titanic$survived)
boxplot(titanic$age~titanic$survived,
        xlab="survived",
        ylab="age",
        main="Relationship between age and survival")

# t-tests, cleaning data
t.test(titanic$age, titanic$survived, var.equal = TRUE)

t.test(titanic$fare, titanic$survived, var.equal = TRUE)

titanic$pclass

titanic$nafare<-titanic$fare
nafare<-mean(!is.na(titanic$fare[titanic$pclass=="1st"]))
titanic$nafare[is.na(titanic$nafare)]<-nafare
clean.titanic<-titanic
farecleaned<-clean.titanic$fare

clean.titanic$farecleaned<-rep(NA, length(farecleaned))
clean.titanic$farecleaned[clean.titanic$nafare>=0 & clean.titanic$nafare<11]<-"0-10"
clean.titanic$farecleaned[clean.titanic$nafare>=11 & clean.titanic$nafare<=50]<-"11-50"
clean.titanic$farecleaned[clean.titanic$nafare>50 & clean.titanic$nafare<=100]<-"51-100"
clean.titanic$farecleaned[clean.titanic$nafare>100 & clean.titanic$nafare<=200]<-"101-200"
clean.titanic$farecleaned[clean.titanic$nafare>200 & clean.titanic$nafare<=300]<-"201-300"
clean.titanic$farecleaned[clean.titanic$nafare>300 & clean.titanic$nafare<=400]<-"301-400"
clean.titanic$farecleaned[clean.titanic$nafare>400 & clean.titanic$nafare<=500]<-"401-500"
clean.titanic$farecleaned[clean.titanic$nafare>500]<-"500<"


clean.titanic$farecleaned<-as.factor(clean.titanic$farecleaned)
summary(clean.titanic$farecleaned)
clean.titanic[is.na(clean.titanic$farecleaned),]

hist(as.numeric(clean.titanic$farecleaned),
     ylim=c(0,600),
     xlab= "Fares")  
