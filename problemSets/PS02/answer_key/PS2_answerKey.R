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

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# create matrix to conduct chi-square test
trafficViolations <- matrix(c(14, 6, 7, 7, 7, 1), byrow=T, nrow=2)
rownames(trafficViolations) <- c("Upper class", "Lower class")
colnames(trafficViolations) <- c("Not stopped", "Bribe", "Stopped/warned")
# by hand approach
# create function from chi-square test github.io
byHandChiSquare <- function(table){
  # turn into table
  observedValues <- as.table(table)
  # create sums (row, column, and total)
  grandSum <- sum(observedValues)
  sumRow <- rowSums(observedValues)
  sumCol <- colSums(observedValues)
  # calculate expected values for each observation
  # check "?outer" to see that this takes the outer product
  # of the row and col sum divided by the total sum
  expectedValues <- outer(sumRow, sumCol, "*") / grandSum
  v <- function(r, c, n) c * r * (n - r) * (n - c)/n^3
  V <- outer(sumRow, sumCol, v, grandSum)
  
  dimnames(expectedValues) <- dimnames(observedValues)
  # create function that calculates each cell residual variance
  # essentially formula on p. 225 in Agresti and Finlay(2009)
  test_statistic <- sum((abs(table - expectedValues))^2 / expectedValues)
  df <- (nrow(observedValues) - 1L) * (ncol(observedValues) - 1L)
  p_value <- pchisq(test_statistic, df, lower.tail = FALSE)
  adjusted_residuals <- (observedValues - expectedValues)/sqrt(expectedValues * (1-sumRow/grandSum) * (1-sumCol/grandSum))
  standardized_residuals <- (observedValues - expectedValues)/sqrt(V)
  # return values
  return(list(statistic = test_statistic,
              df = df,
              p.value = p_value,
              observed = observedValues,
              expected = expectedValues, 
              adj_res = adjusted_residuals,
              std_res = standardized_residuals))  
}
byHandChiSquare(table=trafficViolations)

# run chi square test with built in function
chisq.test(trafficViolations)

# use function to extract standardized residuals
chisq.test(trafficViolations)$stdres

#####################
# Problem 2
#####################

# read in women data from online .csv
women <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
# run regression model with water regressed on whether there are reserved seats for women
regression_model_problem2 <- lm(water ~ reserved, data=women)
# get summary of model with coefficient estimates 
summary(regression_model_problem2)