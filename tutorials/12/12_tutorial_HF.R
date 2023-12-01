# Applied Statistical Analysis I      
# Tutorial 12: Multiple regression, Regression diagnostics  

# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
    basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
    package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
    package.list <- setdiff(package.list, basic.packages)
    if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
    }
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg,  dependencies = TRUE)
    sapply(pkg,  require,  character.only = TRUE)
    }

# Load any necessary packages
lapply(c("car"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Agenda
# (1) Influential cases/outliers
# (2) OLS assumptions
#     - Normality
#     - Constant variance
#     - Linearity
#     - Multicollinearity 

# Research question: 
# What is the relationship between education and Euroscepticism?

# Load data
df <- read.csv("../../datasets/ess_euroscepticism.csv", row.names="X")
View(df)

# Convert categorical variables into factor 
df$edu_cat <- factor(df$edu_cat)
df$gndr <- ifelse(df$gndr == 2, 1, 0)
df$gndr <- factor(df$gndr, labels = c("Male", "Female"))
df$brncntr <- ifelse(df$brncntr == 2, 1, 0)
df$brncntr <- factor(df$brncntr, labels = c("Born in country", "Not born in country"))

# Complete case analysis
df_na <- df[complete.cases(df), ] 

# Reset index
rownames(df_na) <- 1:nrow(df_na) 

# Final model
model_final <- lm(euftf_re~eduyrs + 
                           hinctnta + 
                           trstplt + 
                           imwbcnt + 
                           gndr + 
                           agea + 
                           brncntr, data=df_na)
summary(model_final)

# (1) Influential cases/outliers ---------------

### Cook's Distance ###
# Difference in predicted values when observation
# i is included and not included
# Threshold > 4/(n-k-1)

# Get Cook's Distance for all observations
cooks_d <- cooks.distance(model_final)
cooks_d

# Plot 
par(mar=c(5,4,3,3)) # Reset figure margins
plot(model_final, which=4)

# Get top 10 highest Cook's Distance values
head(sort(cooks_d, decreasing=TRUE),10)
  
# Calculate threshold
thres <- 4/(nobs(model_final)-(length(coef(model_final))-1)-1)

# Get observations above threshold
which(sort(cooks_d, decreasing=TRUE)>thres)

# What to do now?
# Investigate case by case. Coding error? Omitted variables?

# Subsetting data frames, df[row,column]
df_na[159,c("euftf_re","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr")]
model_final$fitted.values[159] # Predicted outcome

df_na[458,c("euftf_re","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr")]
model_final$fitted.values[458] # Predicted outcome

df_na[263,c("euftf_re","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr")]
model_final$fitted.values[263] # Predicted outcome

### Difference in betas ####
# Difference in coefficients when observation 
# i is included and not included

# We repeat the same process

# Get DFBeta for all observations
dfbeta <- dfbeta(model_final)
View(dfbeta)

# Print results for some observations
dfbeta[1, c("eduyrs")]
dfbeta[2, c("eduyrs")]
sprintf("%.10f", dfbeta[1, c("eduyrs")])

# Find maximum absolute values for each coefficient 
dfbeta[,c("eduyrs")][which.max(abs(dfbeta[,c("eduyrs")]))]
dfbeta[,c("hinctnta")][which.max(abs(dfbeta[,c("hinctnta")]))]
dfbeta[,c("trstplt")][which.max(abs(dfbeta[,c("trstplt")]))]
dfbeta[,c("imwbcnt")][which.max(abs(dfbeta[,c("imwbcnt")]))]

# What to do now?
# Investigate case by case. Coding error? Omitted variables?

# Subsetting data frames, df[row,column]
df_na[404,c("euftf_re","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr")]
model_final$fitted.values[404] # Predicted outcome

df_na[344,c("euftf_re","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr")]
model_final$fitted.values[344] # Predicted outcome

### Leverage versus residual plot ###
# Leverage: Unusual value on X
# Discrepancy: Unusual value on Y, given value on X
# Influence = Leverage x Discrepancy
# --> unusual value on X and Y 

# Plot 
plot(model_final, which=5)

# Look at case with very high leverage out of curiosity
# but has low discrepancy, so it is not an influential case
which(hatvalues(model_final)>0.13)

df_na[352,c("euftf_re","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr")]
model_final$fitted.values[352] # Predicted outcome

# What to do now?
# Investigate case by case. Coding error? Omitted variables?

# (2) OLS assumptions ---------------

### Normality ###
# The error is normally distributed 

# Histogram of error
hist(model_final$residuals)

# QQ (Quantile-quantile) plot
plot(model_final, which=2)

### Constant variance ###
# The error has a constant variance (homoscedasticity)

# Residual versus fitted plot
plot(model_final, which=1)

# What to do if labels of observations are overlapping?
which(model_final$residuals>6.35 & model_final$fitted.values<4.5)

### Linearity ###
# The effect between X and Y is linear

# Scatter plots 
plot(df_na$eduyrs,jitter(df_na$euftf_re,2))
plot(df_na$hinctnta,jitter(df_na$euftf_re,2))
plot(df_na$trstplt,jitter(df_na$euftf_re,2))
plot(df_na$imwbcnt,jitter(df_na$euftf_re,2))
plot(df_na$agea,jitter(df_na$euftf_re,2))

# Residual plot
residualPlots(model_final)

# Add a quadratic term for trust in politics
df_na$trstplt_trstplt <- df_na$trstplt^2

# Fit model
model_quad <- lm(euftf_re~eduyrs + 
                   hinctnta + 
                   trstplt + 
                   trstplt_trstplt +
                   imwbcnt +         
                   gndr + 
                   agea + 
                   brncntr, data=df_na)
summary(model_quad)

# Compare residual plot for quadratic model
residualPlots(model_quad)

# We might also want to log-transform education years. 
# This variable is right/positively skewed. 
hist(df_na$eduyrs) 

# Log-transform education years
# +1 because log(0) = -Inf
hist(log(df_na$eduyrs+1)) 
min(df_na$eduyrs)
log(0)

# Fit model
model_log <- lm(euftf_re~log(eduyrs+1) + 
                         hinctnta + 
                         trstplt + 
                         imwbcnt +         
                         gndr + 
                         agea + 
                         brncntr, data=df_na)
summary(model_log)

# Compare residual plot for log model
residualPlots(model_log)

# But be careful, if we transform X we need to 
# adjust interpretation. There is a trade-off between
# fit and interpretability. 

### Multicollinearity ###
# Independent variables are strongly correlated

# Correlation matrix
cor(df_na[, c("eduyrs","hinctnta","trstplt","imwbcnt","agea")])

# Variance Inflation Factor
vif(model_final)

# Create a variable with high correlation
cor(df_na$trstplt,df_na$imwbcnt)
df_na$trust_att <- df_na$trstplt + df_na$imwbcnt
cor(df_na$trust_att,df_na$trstplt)
cor(df_na$trust_att,df_na$imwbcnt)

# Refit model with highly correlated variables
model_collin <- lm(euftf_re~eduyrs + 
                   hinctnta + 
                   trstplt + 
                   imwbcnt +
                   trust_att, data=df_na)
summary(model_collin)


