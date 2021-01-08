#Reading data file
setwd("/Users/Veeksha/Desktop/github")
profiles<-read.csv(file="profiles.csv", header = TRUE)

# Those who did answer:
IncomeGiven<-profiles[profiles$income>-1,]
# Make training set and testing set
set.seed(123) # Don't change this seed!
indicesTrainingSet<-sample(1:nrow(IncomeGiven), 10000, replace=FALSE)
train<-IncomeGiven[indicesTrainingSet,]
test<-IncomeGiven[-indicesTrainingSet,]

mean(train$income)

# boxplot that compares income of the three sexual orientations in the dataset
train$loginc<-log(train$income) 
test1<- aov(train$loginc~train$orientation, data=train)
summary(test1)
boxplot(train$loginc~train$orientation, data=train,
        col= c("yellow", "green", "red"))  


# boxplot that compares income of the three sexual orientation in the dataset by age
boxplot(loginc~orientation+age, data=train,
        col= c("yellow", "green", "red"))
legend("topright", fill = c("yellow", "green", "red"), legend=c("bisexual", "gay","straight"))

# calculating residue
lm1<-lm(train$loginc~train$age+train$orientation+train$age:train$orientation, data=train)
summary(lm1)

# fitted data vs. residue
residual1 <- lm1$resid
fitted1 <-lm1$fitted.values
plot(residual1, fitted1)


#cleaning -1 to NA
train$loginc[train$income ==-1] <-NA
train$loginc

colnames(train)

#I chose to work with education variable
train$newedu<-as.numeric(as.factor(train$education))
train$newedu
is.factor(train$education)
as.numeric(train$education)

table(train$education)
table(train$newedu)


#1 -  TOTAL (6628) 
#2 - college/university (801)
#3 - dropped out of college/university (995)
#4 - dropped out of high school (102)
#5 - dropped out of law school (18)
#6 - dropped out of masters program (140)
#7 - dropped out of med school (12)
#8 - dropped out of ph.d program (127)
#9 - dropped out of space camp (523)
#10 - dropped out of two-year college (191)
#11 - graduated from college/university (23959)
#12 - graduated from high school (1428)
#13 - graduated from law school (1122)
#14 - graduated from masters program (8961)
#15 - graduated from med school (446)
#16 - graduated from ph.d program  (1272)
#17 - graduated from space camp (657)
#18 - graduated from two-year college (1531)
#19 - high school (96)
#20 - law school (19)
#21 - masters program (136)
#22 - med school (11)
#23 - ph.d program  (26)
#24 - space camp (58)
#25 - two-year college (222)
#26 - working on college/university (5712)
#27 - working on high school (87)
#28 - working on law school (269)
#29 - working on masters program (1683)
#30 - working on med school (212)
#31 - working on ph.d program (983)
#32 - working on space camp  (445)
#33 - working on two-year college (1074) 

train$educationClean[train$newedu ==4 | train$newedu ==27| train$newedu ==19 ] <- "No GED"
train$educationClean[train$newedu ==12 ] <- "GED"
train$educationClean[train$newedu ==3 | train$newedu ==26 | train$newedu ==33 | train$newedu ==25
                        | train$newedu ==2 | train$newedu ==10]<- "No Undergraduate Degree"
train$educationClean[train$newedu == 11| train$newedu ==18] <- "Undergraduate degree"
train$educationClean[train$newedu ==5 | train$newedu ==6 | train$newedu ==7 | train$newedu ==9
                        | train$newedu ==20 | train$newedu ==21 | train$newedu ==22| train$newedu ==24
                        |train$newedu ==28 | train$newedu ==29 | train$newedu == 30 
                        |train$newedu == 32] <- "No Masters Degree/ Specialization"
train$educationClean[train$newedu ==13 | train$newedu ==14| train$newedu ==15
                        |train$newedu ==17] <- "Masters Degree/ Specialization"
train$educationClean[train$newedu ==8 | train$newedu ==23 | train$newedu ==31 ] <- "No PhD"
train$educationClean[train$newedu ==16] <- "Holds PhD"
train$educationClean[train$newedu ==1 ] <- "Missing"
train$educationClean<-as.factor(train$educationClean)

#relationship between income and education
test2<- aov(train$loginc~train$educationClean, data=train)
summary(test2)
boxplot(train$loginc~train$educationClean)

#relationship between income and education with respect to sex
boxplot(loginc~educationClean+sex, data=train,
        col= c("pink", "light blue","pink", "light blue","pink", "light blue","pink", "light blue"))
legend("topleft", fill = c("pink", "lightblue"), legend=c("female", "male"))  

#linear regression that predicts log income in terms of education, sex and the interaction between 
#education and sex
lm2<-lm(train$loginc~train$educationClean+train$sex+train$educationClean:train$sex, data=train)
summary(lm2)

#residual vs. fitted values
residual2 <- lm2$resid
fitted2<-lm2$fitted.values
plot(residual2, fitted2)

#model
#tree
library(party)
traintree<-ctree(loginc~educationClean, data=train)
plot(traintree)
traintree

ssr1<-sum((residual1)^2)
rmse1<-sqrt(ssr1/10000)
rmse1

#Prediction
predict<-predict(lm2, newdata = test)
predict
predictInterval = predict( lm2, newdata = test, interval = "prediction" )
predictInterval<-predict(lm2, newdata = test, interval = "prediction")
exp(predictInterval[1,])

#RMSE
ssr2<-sum((residual2)^2)
rmse2<-sqrt(rmse3/10000)

newincome<- exp(predict[1])
newincome




