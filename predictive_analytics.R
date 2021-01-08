setwd("/Users/Veeksha/Desktop/github")
election<-read.csv(file="Election.csv", header = TRUE)

dim(election)
head(election)
colnames(election)
summary(election)

# Scatterplot of votes for Buchanan vs Bush in all the counties in Florida
election2<-election[election$County!="Palm Beach",]
plot(election2$Bush2000, election2$Buchanan2000,
     xlab = "Votes for Buchanan in 2000",
     ylab = "Votes for Bush in 2000",
     main="Votes for Bush and Buchanan in Florida Counties",
     ylim = c(0,max(election$Buchanan2000[election$County=="Palm Beach"])))
#red spline
spline1<-smooth.spline(election2$Bush2000, election2$Buchanan2000)
points(spline1$x, spline1$y, type='l',col="red")
#palm beach point
points(election$Bush2000[election$County=="Palm Beach"], election$Buchanan2000[election$County=="Palm Beach"],pch=19, 
col="blue", text(x=153000, y=3200 , labels="Palm Beach", cex=.9))
   
colnames(election)


# Discussing race as a variable to investigate. 
whiteratio= election$white /election$Pop2000
#spline
predict(spline1, election$Bush2000[election$County=="Palm Beach"])
#tree
library(party)
tree<-ctree(election2$Buchanan2000~election2$white)
plot(tree)

#reg
predict(reg, interval="prediction", election$Bush2000[election$County=="Palm Beach"])

election<-election[order(election$Bush2000),]
lm<-lm(log(Buchanan2000)~log(Bush2000), data=election2)
summary(lm)

predictionbands<-predict(lm, new=election, interval = "prediction")
plot(log(election$Bush2000), log(election$Buchanan2000),
     xlab = "Votes for Buchanan in 2000",
     ylab = "Votes for Bush in 2000",
     main="Prediction of Votes for Bush and Buchanan in Florida Counties")
points(log(election$Bush2000), predictionbands[,2], type="l")
points(log(election$Bush2000), predictionbands[,3], type="l")




