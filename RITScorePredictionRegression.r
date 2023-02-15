#########################
#Program: Linear Regression model predicting RIT scores for each subject
#trained on early years and tested on late years
#Author: Mradul Mourya, Ben Micheals
#########################

install.packages("jtools")
install.packages("ggplot2")
install.packages("gridExtra")
library(jtools)
library(ggplot2)
library(gridExtra)

#set the working directory the context of this file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#get math, language arts, and reading test data
#combine every grade for a given subject
mathData3 <- read.csv("HewittData\\Math\\Grade3Maths.csv")
mathData4 <- read.csv("HewittData\\Math\\Grade4Maths.csv")
mathData5 <- read.csv("HewittData\\Math\\Grade5Maths.csv")
mathData6 <- read.csv("HewittData\\Math\\Grade6Maths.csv")
mathData7 <- read.csv("HewittData\\Math\\Grade7Maths.csv")
mathData8 <- read.csv("HewittData\\Math\\Grade8Maths.csv")
mathData9 <- read.csv("HewittData\\Math\\Grade9Maths.csv")

mathData <- merge(mathData3, mathData4, all = TRUE)
mathData <- merge(mathData, mathData5, all = TRUE)
mathData <- merge(mathData, mathData6, all = TRUE)
mathData <- merge(mathData, mathData7, all = TRUE)
mathData <- merge(mathData, mathData8, all = TRUE)
mathData <- merge(mathData, mathData9, all = TRUE)


languageData3 <- read.csv("HewittData\\LanguageArts\\Grade3Langauge.csv")

#hidden characters are showing up in languageData3 column names for some reason
names(languageData3)[1] = "testid"

languageData4 <- read.csv("HewittData\\LanguageArts\\Grade4Langauge.csv")
languageData5 <- read.csv("HewittData\\LanguageArts\\Grade5Langauge.csv")
languageData6 <- read.csv("HewittData\\LanguageArts\\Grade6Langauge.csv")
languageData7 <- read.csv("HewittData\\LanguageArts\\Grade7Langauge.csv")
languageData8 <- read.csv("HewittData\\LanguageArts\\Grade8Langauge.csv")
languageData9 <- read.csv("HewittData\\LanguageArts\\Grade9Langauge.csv")

languageData <- merge(languageData3, languageData4, all = TRUE)
languageData <- merge(languageData, languageData5, all = TRUE)
languageData <- merge(languageData, languageData6, all = TRUE)
languageData <- merge(languageData, languageData7, all = TRUE)
languageData <- merge(languageData, languageData8, all = TRUE)
languageData <- merge(languageData, languageData9, all = TRUE)

ReadingData3 <- read.csv("HewittData\\Reading\\Grade3Reading.csv")
ReadingData4 <- read.csv("HewittData\\Reading\\Grade4Reading.csv")
ReadingData5 <- read.csv("HewittData\\Reading\\Grade5Reading.csv")
ReadingData6 <- read.csv("HewittData\\Reading\\Grade6Reading.csv")
ReadingData7 <- read.csv("HewittData\\Reading\\Grade7Reading.csv")
ReadingData8 <- read.csv("HewittData\\Reading\\Grade8Reading.csv")
ReadingData9 <- read.csv("HewittData\\Reading\\Grade9Reading.csv")

ReadingData <- merge(ReadingData3, ReadingData4, all = TRUE)
ReadingData <- merge(ReadingData, ReadingData5, all = TRUE)
ReadingData <- merge(ReadingData, ReadingData6, all = TRUE)
ReadingData <- merge(ReadingData, ReadingData7, all = TRUE)
ReadingData <- merge(ReadingData, ReadingData8, all = TRUE)
ReadingData <- merge(ReadingData, ReadingData9, all = TRUE)

#retrieve first and last years from each data set. Don't count 2020 and 2021 due to covid-19
mathEarly <- mathData[which(strtoi(substr(mathData$takendate, 1, 4)) < 1999 & mathData$testlevel != "0"),]
mathLate <- mathData[which(strtoi(substr(mathData$takendate, 1, 4)) > 2008 & strtoi(substr(mathData$takendate, 1, 4)) < 2020 & mathData$testlevel != "0"),]
#math data needs to be split when looking at test level because test levels above 30 correspond to a more difficult test
mathEarlyTest1 <- mathEarly[which(strtoi(mathEarly$testlevel) < 30),]
mathEarlyTest2 <- mathEarly[which(strtoi(mathEarly$testlevel) >= 30),]
mathLateTest1 <- mathLate[which(strtoi(mathLate$testlevel) < 30),]
mathLateTest2 <- mathLate[which(strtoi(mathLate$testlevel) >= 30),]

languageEarly <- languageData[which(strtoi(substr(languageData$takendate, 1, 4)) < 1999 & languageData$testlevel != "0"),]
languageLate <- languageData[which(strtoi(substr(languageData$takendate, 1, 4)) > 2008 & strtoi(substr(languageData$takendate, 1, 4)) < 2020 & languageData$testlevel != "0"),]

readingEarly <- ReadingData[which(strtoi(substr(ReadingData$takendate, 1, 4)) < 1999 & ReadingData$testlevel != "0"),]
readingLate <- ReadingData[which(strtoi(substr(ReadingData$takendate, 1, 4)) > 2008 & strtoi(substr(ReadingData$takendate, 1, 4)) < 2020 & ReadingData$testlevel != "0"),]

#create linear regression models for each subject trained on the early years
mathModelGrade <- lm(formula = rit ~ Grade, data = mathEarly)
mathModelTestLevel1 <- lm(formula = rit ~ testlevel, data = mathEarlyTest1)
mathModelTestLevel2 <- lm(formula = rit ~ testlevel, data = mathEarlyTest2)
readingModelGrade <- lm(formula = rit ~ Grade, data = readingEarly)
readingModelTestLevel <- lm(formula = rit ~ testlevel, data = readingEarly)
languageModelGrade <- lm(formula = rit ~ Grade, data = languageEarly)
languageModelTestLevel <- lm(formula = rit ~ testlevel, data = languageEarly)

#test each model on the late years
#based on https://datascienceplus.com/how-to-apply-linear-regression-in-r/
mathTestGrade <- predict(mathModelGrade, newdata = mathLate)
mathTestTestLevel1 <- predict(mathModelTestLevel1, newdata = mathLateTest1)
mathTestTestLevel2 <- predict(mathModelTestLevel2, newdata = mathLateTest2)
readingTestGrade <- predict(readingModelGrade, newdata = readingLate)
readingTestTestLevel <- predict(readingModelTestLevel, newdata = readingLate)
languageTestGrade <- predict(languageModelGrade, newdata = languageLate)
languageTestTestLevel <- predict(languageModelTestLevel, newdata = languageLate)

#calculate root mean square error for each model test
#RMSE equation based on https://stackoverflow.com/questions/43123462/how-to-obtain-rmse-out-of-lm-result
#mathRMSEGrade <- sqrt((c(crossprod(mathModelGrade$residuals)))/(length(mathModelGrade$residuals)))
#mathRMSETestLevel <- sqrt((c(crossprod(mathModelTestLevel$residuals)))/(length(mathModelTestLevel$residuals)))
#summary(mathModelGrade)
#c(MRMSEG = mathRMSEGrade, MR2=summary(mathModelGrade)$r.squared)
#summary(mathModelTestLevel)
#c(MRMSETL = mathRMSETestLevel, MR2=summary(mathModelTestLevel)$r.squared)


#plot each linear model overlayed on early and late years rit scores
plotMathEarlyGrade <- ggplot(mathEarly, aes(x=Grade, y=rit)) +
  geom_point(size=1)+
  ggtitle("Math RIT scores vs. grade trained")+
  xlab("Grade")+
  ylab("RIT")+
  xlim(c(0,9))+
  ylim(c(0,325))+
  geom_abline(intercept=mathModelGrade$coefficients[1], slope=mathModelGrade$coefficients[2])
plot(plotMathEarlyGrade)

plotMathLateGrade <- ggplot(mathLate, aes(x=Grade, y=rit)) +
  geom_point(size=1)+
  ggtitle("Math RIT scores vs. grade tested")+
  xlab("Grade")+
  ylab("RIT")+
  xlim(c(0,9))+
  ylim(c(0,325))+
  geom_abline(intercept=mathModelGrade$coefficients[1], slope=mathModelGrade$coefficients[2])
plot(plotMathLateGrade)

plotMathEarlyTestLevel1 <- ggplot(mathEarlyTest1, aes(x=testlevel, y=rit)) +
  geom_point(size=1)+
  ggtitle("Math RIT scores vs. test level trained for test type 1")+
  xlab("Test Level")+
  ylab("RIT")+
  xlim(c(0,40))+
  ylim(c(0,325))+
  geom_abline(intercept=mathModelTestLevel1$coefficients[1], slope=mathModelTestLevel1$coefficients[2])
plot(plotMathEarlyTestLevel1)

plotMathLateTestLevel1 <- ggplot(mathLateTest1, aes(x=testlevel, y=rit)) +
  geom_point(size=1)+
  ggtitle("Math RIT scores vs. test level tested for test type 1")+
  xlab("Test Level")+
  ylab("RIT")+
  xlim(c(0,40))+
  ylim(c(0,325))+
  geom_abline(intercept=mathModelTestLevel1$coefficients[1], slope=mathModelTestLevel1$coefficients[2])
plot(plotMathLateTestLevel1)

plotMathEarlyTestLevel2 <- ggplot(mathEarlyTest2, aes(x=testlevel, y=rit)) +
  geom_point(size=1)+
  ggtitle("Math RIT scores vs. test level trained for test type 2")+
  xlab("Test Level")+
  ylab("RIT")+
  xlim(c(0,40))+
  ylim(c(0,325))+
  geom_abline(intercept=mathModelTestLevel2$coefficients[1], slope=mathModelTestLevel2$coefficients[2])
plot(plotMathEarlyTestLevel2)

plotMathLateTestLevel2 <- ggplot(mathLateTest2, aes(x=testlevel, y=rit)) +
  geom_point(size=1)+
  ggtitle("Math RIT scores vs. test level tested for test type 2")+
  xlab("Test Level")+
  ylab("RIT")+
  xlim(c(0,40))+
  ylim(c(0,325))+
  geom_abline(intercept=mathModelTestLevel2$coefficients[1], slope=mathModelTestLevel2$coefficients[2])
plot(plotMathLateTestLevel2)

#summary(readingModel)
#readingRMSE <- sqrt((c(crossprod(readingModel$residuals)))/(length(readingModel$residuals)))
#c(RRMSE = readingRMSE, RR2=summary(readingModel)$r.squared) 

plotReadingEarlyGrade <- ggplot(readingEarly, aes(x=Grade, y=rit)) +
  geom_point(size=1)+
  ggtitle("Reading RIT scores vs. grade trained")+
  xlab("Grade")+
  ylab("RIT")+
  xlim(c(0,9))+
  ylim(c(0,325))+
  geom_abline(intercept=readingModelGrade$coefficients[1], slope=readingModelGrade$coefficients[2])
plot(plotReadingEarlyGrade)

plotReadingLateGrade <- ggplot(readingLate, aes(x=Grade, y=rit)) +
  geom_point(size=1)+
  ggtitle("Reading RIT scores vs. grade tested")+
  xlab("Grade")+
  ylab("RIT")+
  xlim(c(0,9))+
  ylim(c(0,325))+
  geom_abline(intercept=readingModelGrade$coefficients[1], slope=readingModelGrade$coefficients[2])
plot(plotReadingLateGrade)

plotReadingEarlyTestLevel <- ggplot(readingEarly, aes(x=testlevel, y=rit)) +
  geom_point(size=1)+
  ggtitle("Reading RIT scores vs. test level trained")+
  xlab("Test Level")+
  ylab("RIT")+
  xlim(c(0,9))+
  ylim(c(0,325))+
  geom_abline(intercept=readingModelTestLevel$coefficients[1], slope=readingModelTestLevel$coefficients[2])
plot(plotReadingEarlyTestLevel)

plotReadingLateTestLevel <- ggplot(readingLate, aes(x=testlevel, y=rit)) +
  geom_point(size=1)+
  ggtitle("Reading RIT scores vs. test level tested")+
  xlab("Test Level")+
  ylab("RIT")+
  xlim(c(0,9))+
  ylim(c(0,325))+
  geom_abline(intercept=readingModelTestLevel$coefficients[1], slope=readingModelTestLevel$coefficients[2])
plot(plotReadingLateTestLevel)

#summary(languageModel)
#languageRMSE <- sqrt((c(crossprod(languageModel$residuals)))/(length(languageModel$residuals)))
#c(LRMSE = languageRMSE, LR2=summary(languageModel)$r.squared) 

plotLanguageEarlyGrade <- ggplot(languageEarly, aes(x=Grade, y=rit)) +
  geom_point(size=1)+
  ggtitle("Language RIT scores vs. grade trained")+
  xlab("Grade")+
  ylab("RIT")+
  xlim(c(0,9))+
  ylim(c(0,325))+
  geom_abline(intercept=languageModelGrade$coefficients[1], slope=languageModelGrade$coefficients[2])
plot(plotLanguageEarlyGrade)

plotLanguageLateGrade <- ggplot(languageLate, aes(x=Grade, y=rit)) +
  geom_point(size=1)+
  ggtitle("Language RIT scores vs. grade tested")+
  xlab("Grade")+
  ylab("RIT")+
  xlim(c(0,9))+
  ylim(c(0,325))+
  geom_abline(intercept=languageModelGrade$coefficients[1], slope=languageModelGrade$coefficients[2])
plot(plotLanguageLateGrade)

plotLanguageEarlyTestLevel <- ggplot(languageEarly, aes(x=testlevel, y=rit)) +
  geom_point(size=1)+
  ggtitle("Language RIT scores vs. test level trained")+
  xlab("Test Level")+
  ylab("RIT")+
  xlim(c(0,30))+
  ylim(c(0,325))+
  geom_abline(intercept=languageModelTestLevel$coefficients[1], slope=languageModelTestLevel$coefficients[2])
plot(plotLanguageEarlyTestLevel)

plotLanguageLateTestLevel <- ggplot(languageLate, aes(x=testlevel, y=rit)) +
  geom_point(size=1)+
  ggtitle("Language RIT scores vs. test level tested")+
  xlab("Test Level")+
  ylab("RIT")+
  xlim(c(0,30))+
  ylim(c(0,325))+
  geom_abline(intercept=languageModelTestLevel$coefficients[1], slope=languageModelTestLevel$coefficients[2])
plot(plotLanguageLateTestLevel)