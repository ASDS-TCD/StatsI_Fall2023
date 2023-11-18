#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
lapply(c('stargazer'), pkgTest)
# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

#QUESTION 1

#Running a regression where the outcome variable is 'voteshare' and the explanatory variable is 'difflog'
m1 <- lm(voteshare ~ difflog, data = inc.sub)
#Exploring the model's summary and using 'stargazer' for formatting results into a table in Latex format
summary(m1)
stargazer(m1, title = "Model 1: Incumbent's Vote Share vs Campaign Spending Difference", type = "latex")

#Creating a scatterplot + saving as PDF
pdf("01.PS03_Skrypnyk_Plot1.pdf")
plot(inc.sub$difflog, 
     inc.sub$voteshare, 
     pch = 1, 
     col = "coral", 
     main = "Model 1: Incumbent's Vote Share vs Campaign Spending Difference",
     xlab = "Campaign spending difference between incumbent and challenger (difflog)",
     ylab = "Incumbent's vote share (voteshare)")
abline(m1, col = "black")

#Storing residuals into a separate object 
m1res <- m1$residuals

#QUESTION 2

#Running a regression where the outcome variable is 'presvote' and the explanatory variable is 'difflog'
m2 <- lm(presvote ~ difflog, data = inc.sub)

#Exploring the model's summary and using 'stargazer' for formatting results into a table in Latex format
summary(m2)
stargazer(m2, title = "Model 2: Presidential Candidate (Incumbent Party) Vote Share vs Campaign Spending Difference", type = "latex")

#Creating a scatterplot + saving as PDF
pdf("02.PS03_Skrypnyk_Plot2.pdf")
plot(inc.sub$difflog,
     inc.sub$presvote, 
     pch = 5, 
     col = "green", 
     main = "Model 2: Presidential Candidate (Incumbent Party) Vote Share vs Campaign Spending Difference",
     cex.main = 0.8,
     xlab = "Campaign spending difference between incumbent and challenger (difflog)", 
     ylab = "Presidential candidate (incumbent party) vote share (presvote)")
abline(m2, col = "black")

#Storing residuals into a separate object
m2res <- m2$residuals

#QUESTION 3:

#Running a regression where the outcome variable is 'voteshare' and the explanatory variable is 'presvote'
m3 <- lm(voteshare ~ presvote, data = inc.sub)

#Exploring the model's summary and using 'stargazer' for formatting results into a table in Latex format
summary(m3)
stargazer(m3, title = "Model 3: Incumbent's Vote Share vs. Presidential Candidate (Incumbent Party) Vote Share", type = "latex")

#Creating a scatterplot + saving as PDF
pdf("03.PS03_Skrypnyk_Plot3.pdf")
par(mar = c(4, 4, 4, 4))
plot(inc.sub$presvote, 
     inc.sub$voteshare, 
     pch = 16, 
     col = "cyan", 
     main = "Model 3: Incumbent's Vote Share vs. Presidential Candidate (Incumbent Party) Vote Share",
     cex.main = 0.7, 
     xlab = "Presidential candidate (incumbent party) vote share (presvote)", 
     ylab = "Incumbent's vote share (voteshare)")
abline(m3, col = "black") 

#QUESTION 4:

#Running a regression where the outcome variable is the residuals from Question 1 and the explanatory variable is the residuals from Question 2
m4 <- lm(m1res ~ m2res)

#Exploring the model's summary and using 'stargazer' for formatting results into a table in Latex format
summary(m4)
stargazer(m4, title = "Model 4: Model 1 Residuals vs. Model 2 Residuals", type = "latex")

#Creating a scatterplot + saving as PDF
pdf("04.PS03_Skrypnyk_Plot4.pdf")
par(mar = c(6, 6, 6, 6))
plot (m2res, 
     m1res, 
     pch = 8, 
     col = "grey", 
     main = "Model 4: \n Model 1 Residuals (variation in voteshare not explained by the difference in spending between incumbent and challenger) vs. \n Model 2 Residuals (variation in presvote not explained by the difference in spending between incumbent and challenger)",
     cex.main = 0.7,
     xlab = "Model 2 Residuals",
     ylab = "Model 1 Residuals")
abline(m4, col = "black") 

#QUESTION 5
  
#Running a regression where the outcome variable is the incumbent’s voteshare and the explanatory variables are difflog and presvote.
m5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)

#Exploring the model's summary and using 'stargazer' for formatting results into a table in Latex format
summary(m5)
stargazer(m5, title = "Model 5: Incumbent's Vote Share vs. Campaign Spending Difference + Presidential Candidate (Incumbent Party) Vote Share", type = "latex")

#Storing residuals from Model 4 and Model 5 in separate objects
m4res <- m4$residuals
m5res <- m5$residuals

#Using 'all.equal' function to compare the residuals:
all.equal(m4res, m5res) #TRUE


