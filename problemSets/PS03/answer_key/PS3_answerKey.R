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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

# (a) run regression where the outcome variable is \texttt{voteshare} 
# and the explanatory variable is \texttt{difflog}
model_a <- lm(voteshare ~ difflog, data= inc.sub)
#texreg(list(model_a), digits=3)

# (b) scatterplot and reg line
pdf("Q1_b.pdf")
plot(inc.sub$difflog, inc.sub$voteshare, 
     ylab="Vote share", xlab="Difference in spending logged")
abline(model_a, col="red", lty=2)
dev.off()

# (c) scatterplot and reg line
model_a.resid <- resid(model_a)



#####################
# Problem 2
#####################

# (a) run regression where the outcome variable is \texttt{presvote} 
# and the explanatory variable is \texttt{difflog}
model_b <- lm(presvote ~ difflog, data= inc.sub)

# (b)
pdf("Q2_b.pdf")
plot(inc.sub$difflog, inc.sub$presvote, 
     ylab="Pres share", xlab="Difference in spending logged")
abline(model_b, col="red", lty=2)
dev.off()
# (c)
model_b.resid <- resid(model_b)

#####################
# Problem 3
#####################

# (a) run regression where the outcome variable is \texttt{voteshare} 
# and the explanatory variable is \texttt{presvote}
model_c <- lm(voteshare ~ presvote, data= inc.sub)

# (b)
pdf("Q3_b.pdf")
plot(inc.sub$presvote, inc.sub$voteshare,
     ylab="Vote share", xlab="Pres share")
abline(model_c, col="red", lty=2)
dev.off()

# (c)
model_c.resid <- resid(model_c)

#####################
# Problem 4
#####################


# (a) run regression where the outcome variable is \texttt{model_a.resid} 
# and the explanatory variable is \texttt{model_b.resid}
model_d <- lm(model_a.resid ~ model_b.resid)

# (b)
pdf("Q4_b.pdf")
plot(model_b.resid, model_a.resid,
     ylab="Residuals from model_a", xlab="Residuals from model_b")
abline(model_d, col="red", lty=2)
dev.off()

#####################
# Problem 5
#####################

# (a) run regression where the outcome variable is \texttt{voteshare} 
# and the explanatory variable is \texttt{difflog} and presvote
model_e <- lm(voteshare ~ difflog + presvote, data= inc.sub)

