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

install.packages(car)
library(car)
data(Prestige)
help(Prestige)

#QUESTION 1: ECONOMICS

Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
Prestige$professional <- factor(Prestige$professional)
head(Prestige, 30)

m1 <- lm(prestige ~ income + professional + income:professional, data = Prestige)
summary(m1)
stargazer(m1)

#QUESTION 2: POLITICAL SCIENCE 

#(a)
t1 <- 2.625
df <- 128
alpha <- 0.05

#Calculating the critical t-value for a two-tailed test
critical_t1 <- qt(1 - alpha/2, df)

#Calculating the p-value
p_value1 <- 2 * (1 - pt(abs(t1), df))

#(b)
t2 <- 3.23

#Calculating the critical t-value for a two-tailed test
critical_t2 <- qt(1 - alpha/2, df)

#Calculating the p-value
p_value2 <- 2 * (1 - pt(abs(t2), df))