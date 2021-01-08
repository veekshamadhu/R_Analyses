setwd("/Users/Veeksha/Documents/Wellesley/QAI Summer '20/assignment5")
tuition<-read.csv(file="TuitionData.csv", header = TRUE)


tuition$logout<-log(tuition$OutOfState)
var(tuition$logout[tuition$Type=="Public"])

vec1 <- c(tuition$logout[tuition$Type=="Public"]-mean(tuition$logout[tuition$Type=="Public"]))
sum(vec1^2)/24

tss<-c(tuition$logout-mean(tuition$logout))
sst<-sum(tss^2)

publicmean<-mean(tuition$logout[tuition$Type=="Public"])
privmean<-mean(tuition$logout[tuition$Type=="Private"])

publicres<-c(tuition$logout[tuition$Type=="Public"]- publicmean)
privres<- c(tuition$logout[tuition$Type=="Private"]- privmean)

public<-sum(publicres^2)
private<-sum(privres^2)
ssw<-public+private

omean<-mean(tuition$logout)
ssb<-(privmean-omean)^2*25+(publicmean-omean)^2*25

#verify that SSW+SSB=SST
print(ssb+ssw)
sst
#they are equal

out<-aov(tuition$logout~tuition$Type)
summary(out)
ssw 
ssb
#The pvalues match, and f-test is the square of t-test

t.test(tuition$logout[tuition$Type=="Public"], tuition$logout[tuition$Type=="Private"], var.equal = TRUE)

