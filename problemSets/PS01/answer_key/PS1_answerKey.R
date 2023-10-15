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

lapply(c("ggplot2"),  pkgTest)

# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data as vector
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
# capture the number of observations
n <- length(y)

# (a) Calculate the 90% confidence interval for the student IQ
# Step 1: get t-score
t <- qt(0.05, n-1, lower.tail = F)

# Step 2: Calculate lower and upper parts for the 90%
lower_CI <- mean(y)-(t*(sd(y)/sqrt(n)))
upper_CI <- mean(y)+(t*(sd(y)/sqrt(n)))

# print CIs with mean
c(lower_CI, mean(y), upper_CI)

# double check our answer
t.test(y, conf.level = 0.9)$"conf.int"

# (b) Step 1: Calculate the standard error
SE <- sd(y)/sqrt(n)
# Step 2: Calculate the test statistic for this hypothesis testing of mean
t <- (mean(y) - 100)/SE
# Get the p-value from t-distribution
pvalue <- pt(t, n-1, lower.tail = F)

# Or another way to do this hypothesis testing is to use the function t.test directly
t.test(y, mu = 100, conf.level = 0.95, alternative = "greater")

#####################
# Problem 2
#####################

# read in expenditure data
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)
# inspect data through summary
summary(expenditure)

# create a matrix scatter plot to 
# visualize the relationship among Y, X1, X2 and X3
# so not the first column of expenditure
pdf("plot_2a.pdf")
pairs(expenditure[,2:5], main = "")
dev.off()

# generate boxplot with comparisons for different values of Region
pdf("plot_2b.pdf")
boxplot(expenditure$Y~expenditure$Region, xlab="Region", ylab="Y", main="")
dev.off()

# create scatterplot of Y and X1 
# basic and then differentiate color by region
pdf("plot_2c1.pdf", width=8)
ggplot(expenditure, aes(x=X1, y=Y)) + 
  geom_point() + labs(y="Y\n", x="\nX1") +
  theme_bw() + 
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size=15)) 
dev.off()

# make sure that Region is categorical
expenditure$Region <- as.factor(expenditure$Region)

pdf("plot_2c2.pdf", width=8)
plot(expenditure$X1, expenditure$Y,
     pch = as.integer(expenditure$Region),
     xlab="X1", ylab="Y", main="",
     col = expenditure$Region)
dev.off()

pdf("plot_2c3.pdf", width=8)
ggplot(expenditure, aes(x=X1, y=Y, colour=Region, shape=Region)) + 
  geom_point()  + 
labs(y="Y\n", x="\nX1") +
  theme_bw() + 
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size=15),
        legend.title = element_text(size=17),
        legend.text = element_text(size=15)) 
dev.off()

