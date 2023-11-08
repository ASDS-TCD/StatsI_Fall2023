# **Adopted from**: 
# Monte Carlo Simulation and Resampling
# Methods for Social Science
# Authors: Thomas M. Carsey and
# Jeffrey J. Harden

# Install and load packages
# Adopted from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
if(!require(mvtnorm)){
  install.packages("mvtnorm")
  library(mvtnorm)}

# Two examples from the book
# (a.) Omitted variable bias 
# (b.) Regression with random variables 

# **Some terminology**

# Monte Carlo simulation and the data-generating process (DGP)
# "the typical Monte Carlo simulation involves 
# drawing multiple random samples of data from 
# an assumed DGP that describes the unobserved 
# process in the larger population of how a 
# phenomenon of interest is produced" (p. 20). 

# And in the 'real world':
# "Of course, we rarely know what the true DGP 
# is in the real world—we just see the sample 
# data it produces. Most of our research is about
# trying to uncover the underlying DGP" (p. 21).

# (a.) Omitted Variable (Chapter 5.3.4) ------

# Adopted from p. 153
set.seed(37943) # Set the seed for reproducible results

reps <- 1000 # Set the number of repetitions at the top of the script

b0 <- .2 # True value for the intercept
b1 <- .5 # True value for the slopes
b2 <- .75
n <- 1000 # Sample size

# Level of IV correlation
cor.level <- c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, .99) 

# Empty matrix to store the estimates 
par.est.ov <- matrix(NA, nrow = reps, ncol = length(cor.level)) 

for(j in 1:length(cor.level)){ # Start the j loop
  for(i in 1:reps){ # Start the loop
    #i <- 1
    #j <- 1
    X.corr <- matrix(c(1, cor.level[j], cor.level[j], 1), nrow = 2, ncol = 2)
    X <- rmvnorm(n, mean = c(0, 0), sigma = X.corr) # Create two correlated 
    X1 <- X[ , 1]                                   # independent variables
    X2 <- X[ , 2]
    Y <- b0 + b1*X1 + b2*X2 + rnorm(n, 0, 1) # The true DGP, with N(0, 1) error
    model <- lm(Y ~ X1) # Estimate OLS model
    par.est.ov[i, j] <- model$coef[2] # Put the estimate for the coefficient on
                                      # X1 in column j
  } # End the i loop
} # End the j loop

# Print estimates
par.est.ov

# Get mean of estimates
# True b1 is 0.5
mean(par.est.ov[ , 1]) # r=0
mean(par.est.ov[ , 2]) # r=0.1
mean(par.est.ov[ , 3]) # r=0.2
mean(par.est.ov[ , 4]) # r=0.3
mean(par.est.ov[ , 5]) # r=0.4
mean(par.est.ov[ , 6]) # r=0.5
mean(par.est.ov[ , 7]) # r=0.6
mean(par.est.ov[ , 8]) # r=0.7
mean(par.est.ov[ , 9]) # r=0.8
mean(par.est.ov[ , 10]) # r=0.9
mean(par.est.ov[ , 11]) # r=0.99

# Plot 
plot(density(par.est.ov[ , 1]), xlim=c(0,1.5), ylim=c(0,12))
lines(density(par.est.ov[ , 3]), col="gray")
lines(density(par.est.ov[ , 6]), col="orange")
lines(density(par.est.ov[ , 11]), col="red")
abline(v=b1, col="black")
legend(0, 12, # Add legend
       legend=c("r=0","r=0.2","r=0.5","r=0.99"),
       col=c("black","gray","orange","red"),
       pch=1) 

# Cross-validation (Chapter 9.3) ---------

# Cross-validation (CV):
# "the general goal is to assess the fit of a 
# statistical model based on how well it performs 
# in making predictions of “new” observations" (p.344).

# "Because researchers rarely have multiple data 
# sets to employ, CV involves partitioning the 
# one data set a researcher has such that the 
# model is fit on one portion of the sample 
# (often called the “training” or “estimation” data) 
# and evaluated on another portion “testing” or 
# “evaluation” data)" (p. 244-245).

# Overfitting: 
# "As a result, CV helps guard against overfitting 
# the model to the particular sample of data at hand" (p. 345). 

# "any sample of data is made up of “signal” (systematic),
# which reflects the true relationship between
# variables in the DGP that is present in all samples, 
# and its own unique oddities, which we often label 
# “noise” (stochastic)" (p. 345). 

# "However, this noise can also “masquerade” as signal (p. 345). 

# Adopted from p. 345
set.seed(843749) # Set the seed for reproducible results

# Create correlation matrix, (r=0)
rand.vcv <- matrix(0, nrow = 20, ncol = 20) 
diag(rand.vcv) <- 1 

# Create 20 uncorrelated variables 
rand.data <- as.data.frame(rmvnorm(1000, mean = rep(0, times = 20),
 sigma = rand.vcv))

# Rename columns
colnames(rand.data) <- c("y", "x1", "x2", "x3", "x4", "x5", "x6", "x7", 
 "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17",
 "x18", "x19")

# Estimate model
rand.model <- lm(y~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
 x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19, data = rand.data)
summary(rand.model)

# "Of the 20 estimated parameters (intercept and 19 slope coefficients), 
# exactly one of them, x3, is statistically significant 
# at the 0.05 level. This is despite the fact that 
# we know in the true DGP there is no covariance between 
# y and x3. However, this is not a surprising result. 
# By the very definition of a p value, we know that 
# under the null hypothesis such an estimate should 
# appear about 5% of the time" (p. 346). 

# What we see in this example is a case in which 
# the stochastic component of the model (noise) 
# happened by chance to produce a statistically 
# significant estimate. There is nothing systematic 
# that connects y and x3. Rather, the result is 
# just an odd quirk of this particular sample of data" (p. 347).

# "Thus, it is easy to be tricked into thinking 
# that a significant estimate automatically implies
# that something systematic is going on. CV is one 
# way of guarding against that problem" (p. 347).

# How to implement CV? --> Next term 

