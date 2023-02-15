###############################################################
# 5/17/2021
# R scripts for final project
# Analysis of trends in the Hewitt Foundations PASS test data
# with the goal of determining if test should be normalized. 
###############################################################

# Set the location to the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# install packages 
install.packages("stringi")
install.packages("ggplot2")
install.packages("ggpmisc")
install.packages("gridExtra");
install.packages("jtools")

# open packages
library(stringi)
library(ggplot2)
library(ggpmisc)
library(gridExtra)
library(jtools)

# Get the data

####### 3rd ########
thirdMath <- read.csv("data_as_csv/Grade3Maths.csv")
thirdLang <- read.csv("data_as_csv/Grade3Langauge.csv")
thirdRead <- read.csv("data_as_csv/Grade3Reading.csv")

# adds a new column to the data set with just the year number 
# https://stringr.tidyverse.org/reference/str_sub.html
thirdMath$year = stri_sub(thirdMath$takendate,-2)
thirdLang$year = stri_sub(thirdLang$takendate,-2)
thirdRead$year = stri_sub(thirdRead$takendate,-2)

# manipulation of columns to add column with full year (ex 1988, or 2019)
thirdMath$yearbin <- ifelse(thirdMath$year > 20, "19", "20")
thirdLang$yearbin <- ifelse(thirdLang$year > 20, "19", "20")
thirdRead$yearbin <- ifelse(thirdRead$year > 20, "19", "20")

# add year full column by combining other columns
thirdMath$yearfull <- strtoi(paste(thirdMath$yearbin, thirdMath$year, sep = ""))
thirdLang$yearfull <- strtoi(paste(thirdLang$yearbin, thirdLang$year, sep = ""))
thirdRead$yearfull <- strtoi(paste(thirdRead$yearbin, thirdRead$year, sep = ""))


######## 4th #####################
fourthMath <- read.csv("data_as_csv/Grade4Maths.csv")
fourthLang <- read.csv("data_as_csv/Grade4Langauge.csv")
fourthRead <- read.csv("data_as_csv/Grade4Reading.csv")

fourthMath$yearfull <- stri_sub(fourthMath$takendate, from = 1, 4)
fourthLang$yearfull <- stri_sub(fourthLang$takendate, from = 1, 4)
fourthRead$yearfull <- stri_sub(fourthRead$takendate, from = 1, 4)


######## 5th ###################
fifthMath <- read.csv("data_as_csv/Grade5Maths.csv")
fifthLang <- read.csv("data_as_csv/Grade5Langauge.csv")
fifthRead <- read.csv("data_as_csv/Grade5Reading.csv")

fifthMath$yearfull <- stri_sub(fifthMath$takendate, from = 1, 4)
fifthLang$yearfull <- stri_sub(fifthLang$takendate, from = 1, 4)
fifthRead$yearfull <- stri_sub(fifthRead$takendate, from = 1, 4)


############## 6th #################
sixthMath <- read.csv("data_as_csv/Grade6Maths.csv")
sixthLang <- read.csv("data_as_csv/Grade6Langauge.csv")
sixthRead <- read.csv("data_as_csv/Grade6Reading.csv")

sixthMath$yearfull <- stri_sub(sixthMath$takendate, from = 1, 4)
sixthLang$yearfull <- stri_sub(sixthMath$takendate, from = 1, 4)
sixthRead$yearfull <- stri_sub(sixthMath$takendate, from = 1, 4)

############## 7th #################
seventhMath <- read.csv("data_as_csv/Grade7Maths.csv")
seventhLang <- read.csv("data_as_csv/Grade7Langauge.csv")
seventhRead <- read.csv("data_as_csv/Grade7Reading.csv")

seventhMath$yearfull <- stri_sub(seventhMath$takendate, from = 1, 4)
seventhLang$yearfull <- stri_sub(seventhLang$takendate, from = 1, 4)
seventhRead$yearfull <- stri_sub(seventhRead$takendate, from = 1, 4)


############## 8th #################
eighthMath <- read.csv("data_as_csv/Grade8Maths.csv")
eighthLang <- read.csv("data_as_csv/Grade8Langauge.csv")
eighthRead <- read.csv("data_as_csv/Grade8Reading.csv")

eighthMath$yearfull <- stri_sub(eighthMath$takendate, from = 1, 4)
eighthLang$yearfull <- stri_sub(eighthLang$takendate, from = 1, 4)
eighthRead$yearfull <- stri_sub(eighthRead$takendate, from = 1, 4)



#####################################################
#Function that creates histograms and linear regression plots for a specified grade
#Input: math data set, language data set reading data set and grade number
#####################################################
gradeMakeCharts <- function(math_data, lang_data, read_data, grade_num) {
  
  
  # create a data frame that holds mean RIT vs year for each subject
  #MATH
  math_mean_rit <- data.frame()
  # create a dataframe for each year
  for (year in 1988:2020){
    yeardata <- math_data[which(math_data$yearfull == year),]
    meanYear <- mean(yeardata$rit)
    yearRow <- c(year, meanYear)
    math_mean_rit <- rbind(math_mean_rit, yearRow)
  }
  
  # rename columns in mean data frame
  colnames(math_mean_rit) <- c("year", "mean_rit")
  
  #LANG
  lang_mean_rit <- data.frame()
  # create a dataframe for each year
  for (year in 1988:2020){
    yeardata <- lang_data[which(lang_data$yearfull == year),]
    meanYear <- mean(yeardata$rit)
    yearRow <- c(year, meanYear)
    lang_mean_rit <- rbind(lang_mean_rit, yearRow)
  }
  
  # rename columns in mean data frame
  colnames(lang_mean_rit) <- c("year", "mean_rit")
  
  #READ
  read_mean_rit <- data.frame()
  # create a dataframe for each year
  for (year in 1988:2020){
    yeardata <- read_data[which(read_data$yearfull == year),]
    meanYear <- mean(yeardata$rit)
    yearRow <- c(year, meanYear)
    read_mean_rit <- rbind(read_mean_rit, yearRow)
  }
  
  # rename columns in mean data frame
  colnames(read_mean_rit) <- c("year", "mean_rit")
  
  # create data frame with mean RIT scores for each year and test 
  mean_rit <- data.frame(math_mean_rit$year, math_mean_rit$mean_rit, lang_mean_rit$mean_rit, read_mean_rit$mean_rit)
  # add column names to mean data frame

  colnames(mean_rit) <- c("year", "mean_rit_math", "mean_rit_lang", "mean_rit_read")
  
  # make a plot of mean RIT vs year for math 
  math_mean_rit_plot <- ggplot(math_mean_rit, aes(x = year, y = mean_rit)) + 
    geom_point(shape=18, color="blue") +
    geom_smooth(method = lm,  se = FALSE) +
    labs(title = paste("Mean RIT Math Score for Grade ", grade_num, sep=""), x = "year", y = "Mean RIT Math score")
  
  # make a plot of mean RIT vs year for lang 
  lang_mean_rit_plot <- ggplot(lang_mean_rit, aes(x = year, y = mean_rit)) + 
    geom_point(shape=18, color="blue") +
    geom_smooth(method = lm,  se = FALSE) + 
    labs(title = paste("Mean RIT Lang Score for Grade ", grade_num, sep=""), x = "year", y = "Mean RIT Lang score")
  
  # make a plot of mean RIT vs year for read 
  read_mean_rit_plot <- ggplot(read_mean_rit, aes(x = year, y = mean_rit)) + 
    geom_point(shape=18, color="blue") +
    geom_smooth(method = lm,  se = FALSE) +
    labs(title = paste("Mean RIT Read Score for Grade ", grade_num, sep=""), x = "year", y = "Mean RIT Read score")
  
  
  # plot of mean RIT for each test per year
  #https://www.sixhat.net/how-to-plot-multpile-data-series-with-ggplot.html
  
  # Make a plot that shows all the mean RIT scores for each test together
  mean_rit_plot <- ggplot(mean_rit, aes(x = year, y = value, color = variable)) + 
    geom_point(aes(y = mean_rit_math, col = "math")) + 
    geom_point(aes(y = mean_rit_lang, col = "lang")) +
    geom_point(aes(y = mean_rit_read, col = "read")) +
    labs(title = paste("Mean RIT Score for Grade ", grade_num, sep=""), x = "year", y = "Mean RIT score") 

  # Arrange the mean rit vs year plots for easier viewing
  grid.arrange(math_mean_rit_plot, lang_mean_rit_plot, read_mean_rit_plot, mean_rit_plot, ncol=2, nrow = 2)
  
  # linear regression models  
  
  # liner regression model for math 
  # Double check our analysis using multi-variate model
  lr_math_mean = lm( formula = mean_rit ~ year, data = math_mean_rit) 
  
  # Plot mean RIT vs Year math
  lr_math_mean_plot <- ggplot(math_mean_rit, aes(x = year, y = mean_rit) ) +
    geom_point( size = 2, color = "green" ) + 
    ggtitle( label = paste("Mean RIT Math Score Grade ", grade_num," \n with Linear Regression", sep = "") ) + 
    xlab("year") + 
    ylab("Mean RIT Math Score") + 
    # xlim( c(1980,2030) ) + 
    # ylim( c(-2, 300) ) +
    geom_abline(intercept = lr_math_mean$coefficients[1], slope =lr_math_mean$coefficients[2] )
  plot(lr_math_mean_plot)
  summary(lr_math_mean)
  
  # logarithmic regresion math test 
  glm_model <- lm(formula = log(mean_rit) ~ year, data = math_mean_rit)
  summary(glm_model)
  
  x_inps <- seq(1988, 2021, 1)
  y_glm_pred <- predict( glm_model, data.frame(year = x_inps),type = "response" )
  ld <- data.frame( year = x_inps, glm_pred = y_glm_pred) 
  
  plot1 <- ggplot() +
    geom_point( data=math_mean_rit, aes(x = year, y = mean_rit), size=2, color = "green" ) +
    ggtitle(label = paste("Mean RIT Math Score Grade ", grade_num," \n with Logarithmic Regression", sep = "") ) +
    xlab("year") +
    ylab("Mean RIT Math Score" ) +
    geom_point(data=ld, aes(x=year, y=glm_pred), color="black") + # This is the glm line from the sequence 
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  plot(plot1)
  
  # liner regression model for math 
  # Double check our analysis using multi-variate model
  lr_lang_mean = lm( formula = mean_rit ~ year, data = lang_mean_rit) 
  
  # Plot mean RIT vs Year math
  lr_lang_mean_plot <- ggplot(lang_mean_rit, aes(x = year, y = mean_rit) ) +
    geom_point( size = 2, color = "red" ) + 
    ggtitle( label = paste("Mean RIT Lang Score Grade ", grade_num, " \n with linear regression", sep = "")) + 
    xlab("year") + 
    ylab("Mean RIT Lang Score") + 
    # xlim( c(1980,2030) ) + 
    # ylim( c(-2, 300) ) +
    geom_abline(intercept = lr_lang_mean$coefficients[1], slope =lr_lang_mean$coefficients[2] )
  plot(lr_lang_mean_plot)
  summary(lr_lang_mean)
  
  # liner regression model for math 
  # Double check our analysis using multi-variate model
  lr_read_mean = lm( formula = mean_rit ~ year, data = read_mean_rit) 
  
  # Plot mean RIT vs Year math
  lr_read_mean_plot <- ggplot(read_mean_rit, aes(x = year, y = mean_rit) ) +
    geom_point( size = 2, color = "blue" ) + 
    ggtitle( label = paste("Mean RIT Read Score Grade ", grade_num, " \n with linear regression", sep = "")) + 
    xlab("year") + 
    ylab("Mean RIT Read Score") + 
    # xlim( c(1980,2030) ) + 
    # ylim( c(-2, 300) ) +
    geom_abline(intercept = lr_read_mean$coefficients[1], slope =lr_read_mean$coefficients[2] )
  plot(lr_read_mean_plot)
  summary(lr_read_mean)
  
  pdf(paste("grade", grade_num, "meanritlm.pdf", sep = ""))
  grid.arrange(lr_math_mean_plot, lr_lang_mean_plot, lr_read_mean_plot, mean_rit_plot, ncol=2, nrow = 2)
  dev.off()
  
  # histogram of RIT score for math all third 
  hist_math <- ggplot(math_data, aes(rit)) + 
    geom_histogram(color = "blue") +
    ggtitle( label = paste("RIT Math Scores for Grade ", grade_num, sep= "" )) + 
    xlab("rit score") + 
    ylab("count") +
    xlim(-2,300)
  plot(hist_math)
  
  # make data frame of thrid math for 1988 
  math_1988 <- math_data[which(math_data$yearfull == 1988),]
  
  # plot histogram of Rit scores for third math in 1988
  hist_math_1988 <- ggplot(math_1988, aes(rit)) + 
    geom_histogram(color = "blue") +
    ggtitle( label = paste("RIT Math Scores for Grade ", grade_num, " 1988", sep= "" )) + 
    xlab("rit score") + 
    ylab("count") +
    xlim(-2,300)
  plot(hist_math_1988)
  
  # make data frame of thrid math for 1998 
  math_1998 <- math_data[which(math_data$yearfull == 1998),]
  
  # plot histogram of Rit scores for third math in 1998
  hist_math_1998 <- ggplot(math_1998, aes(rit)) + 
    geom_histogram(color = "blue") +
    ggtitle( label = paste("RIT Math Scores for Grade ", grade_num, " 1998", sep= "" )) + 
    xlab("rit score") + 
    ylab("count") +
    xlim(-2,300)
  plot(hist_math_1998)
  
  # make data frame of thrid math for 2008 
  math_2008 <- math_data[which(math_data$yearfull == 2008),]
  
  # plot histogram of Rit scores for third math in 2008
  hist_math_2008 <- ggplot(math_2008, aes(rit)) + 
    geom_histogram(color = "blue") +
    ggtitle( label = paste("RIT Math Scores for Grade ", grade_num, " 2008", sep= "" )) + 
    xlab("rit score") + 
    ylab("count") +
    xlim(-2,300)
  plot(hist_math_2008)
  
  # make data frame of thrid math for 2018 
  math_2018 <- math_data[which(math_data$yearfull == 2018),]
  
  # plot histogram of Rit scores for third math in 2018
  hist_math_2018 <- ggplot(math_2018, aes(rit)) + 
    geom_histogram(color = "blue") +
    ggtitle( label = paste("RIT Math Scores for Grade ", grade_num, " 2018", sep= "" )) + 
    xlab("rit score") + 
    ylab("count") +
    xlim(-2,300)
  plot(hist_math_2018)
  
  grid.arrange(hist_math_1988, hist_math_1998, hist_math_2008, hist_math_2018, hist_math, ncol=2, nrow = 3)
  
  ##############################
  # End of function
}

##################
# Lang test scores mean rit vs year Model Linear
#################### 

# Change the data set in the following lines of code to run for different grades 
lang_data <- eighthLang
grade_num <- "8"

lang_mean_rit <- data.frame()
# create a dataframe with mean for each year
for (year in 1988:2020){
  yeardata <- lang_data[which(lang_data$yearfull == year),]
  meanYear <- mean(yeardata$rit)
  yearRow <- c(year, meanYear)
  lang_mean_rit <- rbind(lang_mean_rit, yearRow)
}

# rename columns in mean data frame
colnames(lang_mean_rit) <- c("year", "mean_rit")

# Create normalized rit and year columns for predictors 
lang_mean_rit$norm_rit <- 
  (lang_mean_rit$mean_rit - min(lang_mean_rit$mean_rit))/(max(lang_mean_rit$mean_rit)-min(lang_mean_rit$mean_rit))
lang_mean_rit$norm_year <- lang_mean_rit$year - (min(lang_mean_rit$year) - 1)

# liner regression model for math 
# Double check our analysis using multi-variate model
lr_lang_mean = lm( formula = norm_rit ~ norm_year, data = lang_mean_rit) 
summary(lr_lang_mean)

# Plot mean RIT vs Year math
lr_lang_mean_plot <- ggplot(lang_mean_rit, aes(x = norm_year, y = norm_rit) ) +
  geom_point( size = 2, color = "red" ) + 
  ggtitle( label = paste("Normalized Mean RIT Lang Score Grade ", grade_num, " \n with linear regression", sep = "")) + 
  xlab("year (year 1 is 1988)") + 
  ylab("Normalized Mean RIT Lang Score")+
  # xlim( c(1980,2030) ) + 
  # ylim( c(-2, 300) ) +
  geom_abline(intercept = lr_lang_mean$coefficients[1], slope =lr_lang_mean$coefficients[2] ) +
  theme(text = element_text(size=18))
plot(lr_lang_mean_plot)

# Output linear regression plot to file
pdf(paste("grade", grade_num, "meanritlang.pdf", sep = ""))
plot(lr_lang_mean_plot)
dev.off()

#################################################

#################################################
# Lang model of mean rit vs year quadratic
# This model was not good
#################################################

# Change the following line of code to which data set you want to run 
lang_data <- eighthLang
grade_num <- "8"

lang_mean_rit <- data.frame()
# create a dataframe with mean for each year
for (year in 1988:2020){
  yeardata <- lang_data[which(lang_data$yearfull == year),]
  meanYear <- mean(yeardata$rit)
  yearRow <- c(year, meanYear)
  lang_mean_rit <- rbind(lang_mean_rit, yearRow)
}

# rename columns in mean data frame
colnames(lang_mean_rit) <- c("year", "mean_rit")

# create columns for predictor variables
lang_mean_rit$norm_rit <- 
  (lang_mean_rit$mean_rit - min(lang_mean_rit$mean_rit))/(max(lang_mean_rit$mean_rit)-min(lang_mean_rit$mean_rit))
lang_mean_rit$norm_year <- lang_mean_rit$year - (min(lang_mean_rit$year) - 1)
lang_mean_rit$norm_year_2 <- lang_mean_rit$norm_year*lang_mean_rit$norm_year
summary(lang_mean_rit)

# liner regression model for math 
# Double check our analysis using multi-variate model
quad_lang_mean_model = lm( formula = norm_rit ~ norm_year + norm_year_2, data = lang_mean_rit) 
summary(quad_lang_mean_model)

# With such low r^2 values I did not go through the process of plotting this model because it was not good at all
# and would not have been helpful


######################################################3

#################################################
# Read Model Linear 
#################################################

# Adjust the following line of code to the data set you want to test
read_data <- eightRead
grade_num <- "8"

read_mean_rit <- data.frame()

# create a dataframe for with mean for each year
for (year in 1988:2020){
  yeardata <- read_data[which(read_data$yearfull == year),]
  meanYear <- mean(yeardata$rit)
  yearRow <- c(year, meanYear)
  read_mean_rit <- rbind(read_mean_rit, yearRow)
}

# rename columns in mean data frame
colnames(read_mean_rit) <- c("year", "mean_rit")

# predictor and target variables 
read_mean_rit$norm_rit <- 
  (read_mean_rit$mean_rit - min(read_mean_rit$mean_rit))/(max(read_mean_rit$mean_rit)-min(read_mean_rit$mean_rit))
read_mean_rit$norm_year <- read_mean_rit$year - (min(read_mean_rit$year) - 1)

# liner regression model for read 
lr_read_mean = lm( formula = norm_rit ~ norm_year, data = read_mean_rit) 
summary(lr_read_mean)

# Plot mean RIT vs Year read
lr_read_mean_plot <- ggplot(read_mean_rit, aes(x = norm_year, y = norm_rit) ) +
  geom_point( size = 2, color = "blue" ) + 
  ggtitle( label = paste("Normalized Mean RIT Read Score Grade ", grade_num, " \n with linear regression", sep = "")) + 
  xlab("year (year 1 is 1988)") + 
  ylab("Normalized Mean RIT Read Score")+
  # xlim( c(1980,2030) ) + 
  # ylim( c(-2, 300) ) +
  geom_abline(intercept = lr_read_mean$coefficients[1], slope =lr_read_mean$coefficients[2] )+
  theme(text = element_text(size=18))
plot(lr_read_mean_plot)

pdf(paste("grade", grade_num, "meanritread.pdf", sep = ""))
plot(lr_read_mean_plot)
dev.off()

###########################################

######################################
# Math Log model
######################################
# function that creates a model for a grade levels math scores and predicts the mean rit in the future years base on a logarithmic regression
# Prints and saves chart that shows logarithimc fit
gradeMathLogmodel <- function(math_data, grade_num) {
  # create a data frame that holds mean RIT vs year for each subject
  #MATH
  # math_data <- eighthMath
  math_mean_rit <- data.frame()
  # create a dataframe for each year
  for (year in 1988:2020){
    yeardata <- math_data[which(math_data$yearfull == year),]
    meanYear <- mean(yeardata$rit)
    yearRow <- c(year, meanYear)
    math_mean_rit <- rbind(math_mean_rit, yearRow)
  }
  
  # rename columns in mean data frame
  colnames(math_mean_rit) <- c("year", "mean_rit")
  
  summary(math_mean_rit)
  
  # Predictors
  # normalized rit score predicted by normalized year 
  math_mean_rit$norm_rit <- 
    (math_mean_rit$mean_rit - min(math_mean_rit$mean_rit))/(max(math_mean_rit$mean_rit)-min(math_mean_rit$mean_rit))
  math_mean_rit$norm_year <- math_mean_rit$year - (min(math_mean_rit$year) - 1)
  
  # logarithmic regression math test 
  log_math_mean_model <- lm(formula = norm_rit ~ log(norm_year), data = math_mean_rit)
  
  summary(log_math_mean_model)
  
  # Create sample data set to run the model on (that can be plotted on the chart to show model predictions)
  x_inps <- seq(1, 33, 1)
  y_pred <- predict( log_math_mean_model, data.frame(norm_year = x_inps),type = "response" )
  ld <- data.frame( norm_year = x_inps, log_math_mean_model_pred = y_pred, year = math_mean_rit$year) 
  
  # Plot the model and linear regression
  plot1 <- ggplot() +
    geom_point( data=math_mean_rit, aes(x = year, y = norm_rit), size=2, color = "#00B81F" ) +
    ggtitle(label = paste("Normalized Mean RIT Math Score Grade ", grade_num,"\n with Logarithmic Regression", sep = "") ) +
    xlab("year") +
    ylab("Normalized Mean RIT Math Score" ) +
    geom_point(data=ld, aes(x=year, y=log_math_mean_model_pred), color="black") + # This is the glm line from the sequence 
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      text = element_text(size=18)
    )

  plot(plot1)
  
  # Output to external file
  pdf(paste("grade", grade_num, "meanmathritlm.pdf", sep = ""))
  plot(plot1)
  dev.off()
 
  #######################
  # End of function
}

#####################################
# Math model - quadratic fit
#####################################
# Function that creates a quadratic fit for the math data and plots it and reports metrics in it
gradeMathQuadmodel2 <- function(math_data, grade_num) {
  # create a data frame that holds mean RIT vs year for each subject
  #MATH

  math_mean_rit <- data.frame()
  # create a dataframe for each year
  for (year in 1988:2020){
    yeardata <- math_data[which(math_data$yearfull == year),]
    meanYear <- mean(yeardata$rit)
    yearRow <- c(year, meanYear)
    math_mean_rit <- rbind(math_mean_rit, yearRow)
  }
  
  # rename columns in mean data frame
  colnames(math_mean_rit) <- c("year", "mean_rit")
  
  summary(math_mean_rit)
  
  # Predictors
  math_mean_rit$norm_rit <- 
    (math_mean_rit$mean_rit - min(math_mean_rit$mean_rit))/(max(math_mean_rit$mean_rit)-min(math_mean_rit$mean_rit))
  math_mean_rit$norm_year <- math_mean_rit$year - (min(math_mean_rit$year) - 1)
  math_mean_rit$norm_year_2 <- math_mean_rit$norm_year*math_mean_rit$norm_year
  
  # logarithmic regression math test 
  quad_math_mean_model <- lm(formula = norm_rit ~ norm_year_2 + norm_year, data = math_mean_rit)
  
  summary(quad_math_mean_model)
  
  # Create sample data set to run the model on (that can be plotted on the chart to show model predictions)
  x_inps <- seq(1, 33, 1)
  y_pred <- predict( quad_math_mean_model, data.frame(norm_year = x_inps, norm_year_2 = x_inps*x_inps),type = "response" )
  ld <- data.frame( norm_year = x_inps, quad_math_mean_model_pred = y_pred, year = math_mean_rit$year) 
  
  # add the predictions to the data set for viewing
  math_mean_rit$predicted <- y_pred
  
  # add a predicted error column to the data set for further analysis
  math_mean_rit$prederror <- math_mean_rit$norm_rit - math_mean_rit$predicted 
  
  # Create the plot
  plot1 <- ggplot() +
    geom_point( data=math_mean_rit, aes(x = year, y = norm_rit), size=2, color = "#00B81F" ) +
    ggtitle(label = paste("Normalized Mean RIT Math Score Grade ", grade_num," \n with Quadratic Regression", sep = "") ) +
    xlab("year") +
    ylab("Normalized Mean RIT Math Score" ) +
    geom_point(data=ld, aes(x=year, y=quad_math_mean_model_pred), color="black") + # This is the glm line from the sequence 
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5), 
      text = element_text(size=20)
    )
  # plot 
  plot(plot1)
  # Export plot to file 
  pdf(paste("grade", grade_num, "meanmathritquad.pdf", sep = ""))
  plot(plot1)
  dev.off()
  
}
###################################################
# Function calls for mean_rit vs year models and EDA 
###################################################

# Call function to make histograms of scores and linear regression charts for each
# This was the first function I ran - helpful to get a starting idea of how the data looks
gradeMakeCharts(thirdMath, thirdLang, thirdRead, "3")
gradeMakeCharts(fourthMath, fourthLang, fourthRead, "4")
gradeMakeCharts(fifthMath, fifthLang, fifthRead, "5")
gradeMakeCharts(sixthMath, sixthLang, sixthRead, "6")
gradeMakeCharts(seventhMath, seventhLang, seventhRead, "7")
gradeMakeCharts(eighthMath, eighthLang, eighthRead, "8")

# Run the math logarithmic fit model on each grade
gradeMathLogmodel(thirdMath, "3")
gradeMathLogmodel(fourthMath, "4")
gradeMathLogmodel(fifthMath, "5")
gradeMathLogmodel(sixthMath, "6")
gradeMathLogmodel(seventhMath, "7")
gradeMathLogmodel(eighthMath, "8")

# Run the math quadratic fit on each grade
gradeMathQuadmodel2(thirdMath, "3")
gradeMathQuadmodel2(fourthMath, "4")
gradeMathQuadmodel2(fifthMath, "5")
gradeMathQuadmodel2(sixthMath, "6")
gradeMathQuadmodel2(seventhMath, "7")
gradeMathQuadmodel2(eighthMath, "8")


# Standardize rit score 
# In preparation for the functions that explore distribution
thirdMath$rit_z_score <- scale(x = thirdMath$rit)
thirdRead$rit_z_score <- scale(x = thirdRead$rit)
thirdLang$rit_z_score <- scale(x = thirdLang$rit)

fourthMath$rit_z_score <- scale(x = fourthMath$rit)
fourthRead$rit_z_score <- scale(x = fourthRead$rit)
fourthLang$rit_z_score <- scale(x = fourthLang$rit)

fifthMath$rit_z_score <- scale(x = fifthMath$rit)
fifthRead$rit_z_score <- scale(x = fifthRead$rit)
fifthLang$rit_z_score <- scale(x = fifthLang$rit)

sixthMath$rit_z_score <- scale(x = sixthMath$rit)
sixthRead$rit_z_score <- scale(x = sixthRead$rit)
sixthLang$rit_z_score <- scale(x = sixthLang$rit)

seventhMath$rit_z_score <- scale(x = seventhMath$rit)
seventhRead$rit_z_score <- scale(x = seventhRead$rit)
seventhLang$rit_z_score <- scale(x = seventhLang$rit)

eighthMath$rit_z_score <- scale(x = eighthMath$rit)
eighthRead$rit_z_score <- scale(x = eighthRead$rit)
eighthLang$rit_z_score <- scale(x = eighthLang$rit)


#######################################
# FUNCTION math
# Creates histograms for normalized Rit score for a specified grade and subject 
# Creates a general distribution, a distribution for first 5 years and a distribution for last 5 years 
# Outputs early and late distributions to file 
#######################################
gradeNormHistMath <- function(math_data, grade_num) {
  
  # Create general histogram (all years)
  hist_math_norm <- ggplot(math_data, aes(rit_z_score)) + 
    geom_histogram(color = "blue", bins = 50) +
    ggtitle( label = paste("RIT Math Scores normalized \n for Grade ", grade_num, sep= "" )) + 
    xlab("rit z score") + 
    ylab("count") +
    geom_vline(aes(xintercept = mean(rit_z_score)),col='red',size=1) +
    xlim(-5,5) +
    theme(text = element_text(size=20))

  # make data frame for 1988 
  math_1988 <- math_data[which(math_data$yearfull == 1988),]
  
  # make math data for early 5 years (1988-1992)
  math_early <- math_data[which((math_data$yearfull < 1993) & (math_data$yearfull > 1987)),]
  
  # histogram of RIT z score for early years 
  hist_math_early_norm <- ggplot(math_early, aes(rit_z_score)) + 
    geom_histogram(color = "blue", bins = 50) +
    ggtitle( label = paste("RIT Math Scores normalized \n for Grade ", grade_num, " 1988-1992", sep= "" )) + 
    xlab("rit z score") + 
    ylab("count") +
    geom_vline(aes(xintercept = mean(rit_z_score)),col='red',size=1) +
    xlim(-5,5) +
    theme(text = element_text(size=20))
  
  # xlim(-2,300)
  
  # make math data for late 5 years (2015-2019)
  math_late <- math_data[which(math_data$yearfull > 2014 & math_data$yearfull < 2020),]
  
  # histogram of RIT z score for early years 
  hist_math_late_norm <- ggplot(math_late, aes(rit_z_score)) + 
    geom_histogram(color = "blue", bins = 50) +
    ggtitle( label = paste("RIT Math Scores normalized \n for Grade ", grade_num, " 2015-2020", sep= "" )) + 
    xlab("rit z score") + 
    ylab("count") +
    geom_vline(aes(xintercept = mean(rit_z_score)),col='red',size=1) +
    xlim(-5,5) +
    theme(text = element_text(size=20))
  
  
  # xlim(-2,300)
  
  # # make data frame of thrid math for 1998 
  # math_1998 <- math_data[which(math_data$yearfull == 1998),]
  # 
  # # plot histogram of Rit scores for third math in 1998
  # hist_math_1998_norm <- ggplot(math_1998, aes(rit_z_score)) + 
  #   geom_histogram(color = "blue", bins = 50) +
  #   ggtitle( label = paste("RIT Math Scores normalized for Grade ", grade_num, " 1998", sep= "" )) + 
  #   xlab("rit z score") + 
  #   ylab("count")
  #   # xlim(-2,300)
  # 
  # # make data frame of thrid math for 2008 
  # math_2008 <- math_data[which(math_data$yearfull == 2008),]
  # 
  # # plot histogram of Rit scores for third math in 2008
  # hist_math_2008_norm <- ggplot(math_2008, aes(rit_z_score)) + 
  #   geom_histogram(color = "blue", bins = 50) +
  #   ggtitle( label = paste("RIT Math Scores normalized for Grade ", grade_num, " 2008", sep= "" )) + 
  #   xlab("rit z score") + 
  #   ylab("count")
  #   # xlim(-2,300)
  # 
  # # make data frame of thrid math for 2018 
  # math_2018 <- math_data[which(math_data$yearfull == 2018),]
  # 
  # # plot histogram of Rit scores for third math in 2018
  # hist_math_2018_norm <- ggplot(math_2018, aes(rit_z_score)) + 
  #   geom_histogram(color = "blue", bins = 50) +
  #   ggtitle( label = paste("RIT Math Scores normalized for Grade ", grade_num, " 2018", sep= "" )) + 
  #   xlab("rit z score") + 
  #   ylab("count")
  #   # xlim(-2,300)

  # output to file
  pdf(paste("grade", grade_num, "mathnormhist.pdf", sep = ""))
  grid.arrange( hist_math_early_norm, hist_math_late_norm,ncol=1, nrow = 2)
  dev.off()
  
# end of function
##############
}

#######################################
# FUNCTION LANG
# Creates histograms for normalized Rit score for a specified grade and subject
# Creates a general distribution, a distribution for first 5 years and a distribution for last 5 years 
# Outputs early and late distributions to file 
#######################################
gradeNormHistLang <- function(lang_data, grade_num) {
  
  hist_lang_norm <- ggplot(lang_data, aes(rit_z_score)) + 
    geom_histogram(color = "blue", bins = 50) +
    ggtitle( label = paste("RIT Lang Scores normalized \n for Grade ", grade_num, sep= "" )) + 
    xlab("rit z score") + 
    ylab("count") +
    geom_vline(aes(xintercept = mean(rit_z_score)),col='red',size=1) +
    xlim(-5,5) +
    theme(text = element_text(size=20))
  
  
  # xlim(-2,300)
  
  # make data frame of thrid math for 1988 
  # math_1988 <- math_data[which(math_data$yearfull == 1988),]
  
  # make math data for early 5 years (1988-1992)
  lang_early <- lang_data[which((lang_data$yearfull < 1993) & (lang_data$yearfull > 1987)),]
  
  # histogram of RIT z score for early years 
  hist_lang_early_norm <- ggplot(lang_early, aes(rit_z_score)) + 
    geom_histogram(color = "blue", bins = 50) +
    ggtitle( label = paste("RIT Lang Scores normalized \n for Grade ", grade_num, " 1988-1992", sep= "" )) + 
    xlab("rit z score") + 
    ylab("count") +
    geom_vline(aes(xintercept = mean(rit_z_score)),col='red',size=1) +
    xlim(-5,5) +
    theme(text = element_text(size=20))
  
  
  # xlim(-2,300)
  
  # make math data for late 5 years (2015-2019)
  lang_late <- lang_data[which(lang_data$yearfull > 2014 & lang_data$yearfull < 2020),]
  
  # histogram of RIT z score for early years 
  hist_lang_late_norm <- ggplot(lang_late, aes(rit_z_score)) + 
    geom_histogram(color = "blue", bins = 50) +
    ggtitle( label = paste("RIT Lang Scores normalized \n for Grade ", grade_num, " 2015-2020", sep= "" )) + 
    xlab("rit z score") + 
    ylab("count") +
    geom_vline(aes(xintercept = mean(rit_z_score)),col='red',size=1) +
    xlim(-5,5) +
    theme(text = element_text(size=20))
    
  
  # Export to file
  pdf(paste("grade", grade_num, "langnormhist.pdf", sep = ""))
  grid.arrange( hist_lang_early_norm, hist_lang_late_norm,ncol=1, nrow = 2)
  dev.off()
  
  # end of function
  ##############
}


#######################################
# FUNCTION Read
# Creates histograms for normalized Rit score for a specified grade and subject 
# Creates a general distribution, a distribution for first 5 years and a distribution for last 5 years 
# Outputs early and late distributions to file 
#######################################
gradeNormHistRead <- function(read_data, grade_num) {
  
  # histogram of distribution for all years
  hist_read_norm <- ggplot(read_data, aes(rit_z_score)) + 
    geom_histogram(color = "blue", bins = 50) +
    ggtitle( label = paste("RIT Read Scores normalized \n for Grade ", grade_num, sep= "" )) + 
    xlab("rit z score") + 
    ylab("count") +
    geom_vline(aes(xintercept = mean(rit_z_score)),col='red',size=1) +
    xlim(-5,5) +
    theme(text = element_text(size=20)) 
  # xlim(-2,300)
  
  # make data frame of thrid math for 1988 
  # math_1988 <- math_data[which(math_data$yearfull == 1988),]
  
  # make math data for early 5 years (1988-1992)
  read_early <- read_data[which((read_data$yearfull < 1993) & (read_data$yearfull > 1987)),]
  
  # histogram of RIT z score for early years 
  hist_read_early_norm <- ggplot(read_early, aes(rit_z_score)) + 
    geom_histogram(color = "blue", bins = 50) +
    ggtitle( label = paste("RIT Read Scores normalized \n for Grade ", grade_num, " 1988-1992", sep= "" )) + 
    xlab("rit z score") + 
    ylab("count") +
    geom_vline(aes(xintercept = mean(rit_z_score)),col='red',size=1) +
    xlim(-5,5) +
    theme(text = element_text(size=20)) 
    
  
  # xlim(-2,300)
  
  # make math data for late 5 years (2015-2019)
  read_late <- read_data[which(read_data$yearfull > 2014 & read_data$yearfull < 2020),]
  
  # histogram of RIT z score for early years 
  hist_read_late_norm <- ggplot(read_late, aes(rit_z_score)) + 
    geom_histogram(color = "blue", bins = 50) +
    ggtitle( label = paste("RIT Read Scores normalized \n for Grade ", grade_num, " 2015-2020", sep= "" )) + 
    xlab("rit z score") + 
    ylab("count") +
    geom_vline(aes(xintercept = mean(rit_z_score)),col='red',size=1) +
    xlim(-5,5) + 
    theme(text = element_text(size=20))

  
  # Export to file
  pdf(paste("grade", grade_num, "readnormhist.pdf", sep = ""))
  grid.arrange( hist_read_early_norm, hist_read_late_norm,ncol=1, nrow = 2)
  dev.off()
  
  # end of function
  ##############
}


# Creating Histograms for distribution by calling functions
# Math distribution
gradeNormHistMath(thirdMath, 3)
gradeNormHistMath(fourthMath, 4)
gradeNormHistMath(fifthMath, 5)
gradeNormHistMath(sixthMath, 6)
gradeNormHistMath(seventhMath, 7)
gradeNormHistMath(eighthMath, 8)
# Read distribution
gradeNormHistRead(thirdRead, 3)
gradeNormHistRead(fourthRead, 4)
gradeNormHistRead(fifthRead, 5)
gradeNormHistRead(sixthRead, 6)
gradeNormHistRead(seventhRead, 7)
gradeNormHistRead(eighthRead, 8)
# Lang Distribution
gradeNormHistLang(thirdLang, 3)
gradeNormHistLang(fourthLang, 4)
gradeNormHistLang(fifthLang, 5)
gradeNormHistLang(sixthLang, 6)
gradeNormHistLang(seventhMath, 7)
gradeNormHistLang(eighthLang, 8)


##########################################
# Standard Deviation Graphs
#########################################

# Change these 4 lines of code to run the analysis and make the plots of other grade's data sets 
math_data <- eighthMath
read_data <- eighthLang
lang_data <- eighthRead
grade_num <- "8"

# Create data frame with standard deviation for math rit scores
math_stdev_df <- data.frame()

# create a dataframe with standard deviation for each year math
for (year in 1988:2020){
  yeardata <- math_data[which(math_data$yearfull == year),]
  stdevYear <- sd(yeardata$rit)
  yearRow <- c(year, stdevYear)
  math_stdev_df <- rbind(math_stdev_df, yearRow)
}

# Rename columns 
colnames(math_stdev_df) <- c("year", "stdev_rit")


# Create data frame with standard deviation for lang rit scores
lang_stdev_df <- data.frame()
# create a dataframe with standard deviationfor each year
for (year in 1988:2020){
  yeardata <- lang_data[which(lang_data$yearfull == year),]
  stdevYear <- sd(yeardata$rit)
  yearRow <- c(year, stdevYear)
  lang_stdev_df <- rbind(lang_stdev_df, yearRow)
}

# Rename columns 
colnames(lang_stdev_df) <- c("year", "stdev_rit")


# Create data frame with standard deviation for read rit scores
read_stdev_df <- data.frame()
# create a dataframe with standard deviationfor each year
for (year in 1988:2020){
  yeardata <- read_data[which(read_data$yearfull == year),]
  stdevYear <- sd(yeardata$rit)
  yearRow <- c(year, stdevYear)
  read_stdev_df <- rbind(read_stdev_df, yearRow)
}

# Rename columns 
colnames(read_stdev_df) <- c("year", "stdev_rit")

# Create one data frame that includes all the standard deviations 
stdev_rit_df <- data.frame(math_stdev_df$year, math_stdev_df$stdev_rit, lang_stdev_df$stdev_rit, read_stdev_df$stdev_rit, grade_num)

colnames(stdev_rit_df) <- c("year", "stdev_math", "stdev_lang", "stdev_read", "grade")

# Create columns in each data set that show how far the standard deviation for each year is off the 
# average standard deviation
stdev_rit_df$pts_from_mean_math <- abs(stdev_rit_df$stdev_math - mean(stdev_rit_df$stdev_math))
stdev_rit_df$pts_from_mean_read <- abs(stdev_rit_df$stdev_read - mean(stdev_rit_df$stdev_read))
stdev_rit_df$pts_from_mean_lang <- abs(stdev_rit_df$stdev_lang - mean(stdev_rit_df$stdev_lang))

# View the average difference from the mean in standard deviation
mean(stdev_rit_df$pts_from_mean_math)
mean(stdev_rit_df$pts_from_mean_read)
mean(stdev_rit_df$pts_from_mean_lang)

# Make Plot of the standard deviations on one graph
stdev_rit_plot <- ggplot() +
  geom_point(data = math_stdev_df, aes(x = year, y = stdev_rit), color = "#00B81F", size = 3) +
  geom_point(data = lang_stdev_df, aes(x = year, y = stdev_rit), color = "blue", size = 3) +
  geom_point(data = read_stdev_df, aes(x = year, y = stdev_rit), color = "red", size = 3) +
  labs(title = paste("Standard Deviation of RIT Score \n for Grade ", grade_num, sep=""), x = "Year", y = "Standard Deviation of RIT score") +
  theme(text = element_text(size=20)) +
  ylim(0,30)

# plot 
plot(stdev_rit_plot)

# plot to external file 
pdf(paste("grade", grade_num, "stdev.pdf", sep = ""))
plot(stdev_rit_plot)
dev.off()

##############################################

# Everything beyond this point was used experimentally or exploratativly and is not included in the final research

math_1988 <- thirdMath[which(thirdMath$yearfull == 1988),]
math_1989 <- thirdMath[which(thirdMath$yearfull == 1989),]
math_1990 <- thirdMath[which(thirdMath$yearfull == 1990),]
math_1991 <- thirdMath[which(thirdMath$yearfull == 1991),]
math_1992 <- thirdMath[which(thirdMath$yearfull == 1992),]
math_2018 <- thirdMath[which(thirdMath$yearfull == 2018),]


# Linear model of math rit scores vs year 

# Change the following line of code to change the data set you are working with for the model
data <- thirdMath

# Create the data set with normalized rit scores 
mean_rit_norm <- data.frame()
for (year in 1988:2020){
  yeardata <- data[which(data$yearfull == year),]
  meanYearNorm <- mean(yeardata$rit_z_score)
  yearRow <- c(year, meanYearNorm)
  mean_rit_norm <- rbind(mean_rit_norm, yearRow)
}

# lable columns 
colnames(mean_rit_norm) <- c("year", "mean_rit_z_score")


# liner regression model for math 
lr_mean_rit_norm = lm( formula = mean_rit_z_score ~ year, data = mean_rit_norm) 

summary(lr_mean_rit_norm)

# Plot mean RIT vs Year math with linear regression model
lr_mean_rit_norm_plot <- ggplot(mean_rit_norm, aes(x = year, y = mean_rit_z_score) ) +
  geom_point( size = 2, color = "blue" ) + 
  ggtitle( label = paste("Mean RIT z score Math Grade ", "3", " with linear regression", sep = "")) + 
  xlab("year") + 
  ylab("Mean RIT z math Score") + 
  # xlim( c(1980,2030) ) + 
  # ylim( c(-2, 300) ) +
  geom_abline(intercept = lr_mean_rit_norm$coefficients[1], slope =lr_mean_rit_norm$coefficients[2] )
# plot
plot(lr_mean_rit_norm_plot)

# 
# mean(math_1988$rit_z_score)
# mean(math_1989$rit_z_score)
# mean(math_1990$rit_z_score)
# mean(math_1991$rit_z_score)
# mean(math_1992$rit_z_score)
# mean(math_2018$rit_z_score)
# 
# 
# mean(thirdMath$rit_z_score)
# mean(fourthMath$rit_z_score)

# More modeling attempts

thirdLang <- thirdLang[-c(11,12)]

data_lang <- rbind(thirdLang, fourthLang, fifthLang, sixthLang, seventhLang, eighthLang)

# Double check our analysis using multi-variate model
linear_modelL = lm( formula = rit ~ yearfull + Grade + testlevel, data = data_lang) 
summary(linear_modelL)

## above four variables gives the highest r-squared values

data_lang$predicted <- predict(linear_modelL,data_lang)
data_lang$PredictionError <- data_lang$predicted - data_lang$rit



thirdRead <- thirdRead[-c(11,12)]

data_read <- rbind(thirdRead, fourthRead, fifthRead, sixthRead, seventhRead, eighthRead)

# Double check our analysis using multi-variate model
linear_modelR = lm( formula = rit ~ yearfull + Grade + testlevel, data = data_read) 
summary(linear_modelR)

## above four variables gives the highest r-squared values

data_lang$predicted <- predict(linear_modelR,data_lang)
data_lang$PredictionError <- data_lang$predicted - data_lang$rit

# observations - math test scores seem to have jumped (probably due to people)
# getting more information on the test and better knowing how to prepare, 
# but reading and language tests should be redesigned because the
# distribution of scores is shifting up

