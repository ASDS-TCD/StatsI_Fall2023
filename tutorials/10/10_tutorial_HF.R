# Applied Statistical Analysis I      
# Tutorial 10: Multiple regression                   

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
lapply(c("stargazer","vioplot","arm","broom","ggplot2","fastDummies"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Research questions: 
# What is the relationship between education and Euroscepticism?

# Subsetting data -----------

# Make sure to download ESS data first and 
# add to the datasets folder in your repository. 
# Download the ESS10 - integrated file, edition 3.2 here:
# https://ess-search.nsd.no/en/study/172ac431-2a06-41df-9dab-c1fd8f3877e7

# Look at the Codebook: 
# **DV** (euftf), European unification go further or gone too far
# 0: Unification already gone too far, 10: Unification go further
# **IV** (edlvdie), Highest level of education, Ireland
# **IV** (eduyrs), Years of full-time education completed

# Z1 (hinctnta), Household's total net income, all sources
# Unit is deciles (ranging between 1 and 10th deciles)
# Z2 (trstplt), Trust in politicians
# 0: No trust at all, 10: Complete trust
# Z3 (imwbcnt), Immigrants make country worse or better place to live
# 0: Worse place to live, 10: Better place to live

# Some general socio-demographic controls
# Gender (gndr), 1: Male, 2: Female
# Age (agea), Age of respondent, calculated
# Born in country (brncntr), 1: Yes, 2: No

# Only include Ireland and relevant variables. 
df <- read.csv("../../datasets/ESS10.csv")
df_s <- df[df$cntry=="IE", c("euftf","edlvdie","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr")]
View(df_s)

# Reverse euftf, to measure euroscepticism more intuitively
df_s["euftf_re"] = 10 - df_s[ ,c("euftf")]

# Categorize education levels
df_s["edu_cat"] <- NA
df_s[(df_s$edlvdie==1) | (df_s$edlvdie==2) | (df_s$edlvdie==3) | (df_s$edlvdie==4), c("edu_cat")] <- 1 # Junior Cycle
df_s[(df_s$edlvdie==5) | (df_s$edlvdie==6) | (df_s$edlvdie==7) | (df_s$edlvdie==8) | (df_s$edlvdie==9), c("edu_cat")] <- 2 # Leaving Certificate 
df_s[(df_s$edlvdie==10) | (df_s$edlvdie==11) | (df_s$edlvdie==12), c("edu_cat")] <- 3 # Advanced Certificate
df_s[(df_s$edlvdie==13) | (df_s$edlvdie==14) | (df_s$edlvdie==15), c("edu_cat")] <- 4 # Bachelor Degree
df_s[(df_s$edlvdie==16) | (df_s$edlvdie==17) | (df_s$edlvdie==18), c("edu_cat")] <- 5 # Postgraduate Degree

# Convert into factor variable
df_s$edu_cat <- factor(df_s$edu_cat, 
                       levels = c(1,2,3,4,5),
                       labels = c("Junior Cycle",
                                  "Leaving Certificate",
                                  "Advanced Certificate",
                                  "Bachelor Degree",
                                  "Postgraduate Degree"))
levels(df_s$edu_cat)
typeof(df_s$edu_cat)

# Record missing values
df_s[(df_s == -67) | (df_s == -78) | (df_s == -89) | (df_s == 77) | (df_s == 88) | (df_s == 99) | (df_s == 999) | (df_s == 5555) | (df_s == 7777) | (df_s == 8888) | (df_s == 9999)] <- NA

# Save dataset
write.csv(df_s, "../../datasets/ess_euroscepticism.csv")

# Initial investigation ----------
df <- read.csv("../../datasets/ess_euroscepticism.csv", row.names="X")
View(df)
is.factor(df$edu_cat)

# Convert into factor variable
df$edu_cat <- factor(df$edu_cat)
is.factor(df$edu_cat)

# Descriptive plots
par(mar = c(5, 5, 2, 2)) # Change margins in plot manually
vioplot(df$euftf_re ~ df$edu_cat)
plot(df$eduyrs,df$euftf_re)
plot(jitter(df$eduyrs,2),jitter(df$euftf_re,2))

# Simple model only considering socio-demographic variables
model_base <- lm(euftf_re~gndr + agea + brncntr, data=df)
summary(model_base)

# Visualizing regression models with categorical independent variables 

# Simple linear regression
model_base <- lm(euftf_re~brncntr, data=df)
summary(model_base)

# Plot 
plot(df$agea, df$euftf_re)
abline(h=model_base$coefficients[1],col="black") # Regression line brncntr=0, Born in country
abline(h=model_base$coefficients[1]+model_base$coefficients[2],col="blue") # Regression line brncntr=1, Not born in country
legend(70, 10, # Legend
       legend=c("Born in country", "Not born in country"),
       col=c("black","blue"),
       pch=1) 

# Multiple linear regression
model_base <- lm(euftf_re~brncntr + gndr + agea, data=df)
summary(model_base)

# Plot
plot(df$agea, df$euftf_re)
abline(model_base$coefficients[1], model_base$coefficients[4],col="black") # Regression line brncntr=0, Born in country
abline(model_base$coefficients[1]+model_base$coefficients[2], model_base$coefficients[4],col="blue") # Regression line brncntr=1, Not born in country
legend(70, 10, # Legend
       legend=c("Born in country", "Not born in country"),
       col=c("black","blue"),
       pch=1) 

# (1) Hypothesis 1 --------------

# The higher the years of education, 
# the lower the level of Euroscepticism

# Continuous independent variable
model1 <- lm(euftf_re~eduyrs,data=df)
summary(model1)

# What is the prediction equation?

# Categorical independent variable (manually)

# Create dummy variables 
df$JuniorCycle <- ifelse(df$edu_cat == "Junior Cycle", 1, 0)
df$LeavingCertificate <- ifelse(df$edu_cat == "Leaving Certificate", 1, 0)
df$AdvancedCertificate <- ifelse(df$edu_cat == "Advanced Certificate", 1, 0)
df$Bachelor <- ifelse(df$edu_cat == "Bachelor Degree", 1, 0)
df$Postgraduate <- ifelse(df$edu_cat == "Postgraduate Degree", 1, 0)

# Faster way to create dummy set
df <- dummy_cols(df, select_columns = "edu_cat")

# Fit model
model1 <- lm(euftf_re~LeavingCertificate+AdvancedCertificate+Bachelor+Postgraduate,data=df)
summary(model1)

# Change reference category to leaving certificate 
model1 <- lm(euftf_re~JuniorCycle+AdvancedCertificate+Bachelor+Postgraduate,data=df)
summary(model1)

# Categorical independent variable, using factor variales
is.factor(df$edu_cat)
model1 <- lm(euftf_re~edu_cat,data=df)
summary(model1)

# Change reference category
# Which one should we select?
plot(df$edu_cat)
levels(df$edu_cat) # First level, "Advanced Certificate"

# Change reference category to leaving certificate 
df$edu_cat <- relevel(df$edu_cat, ref = 4)
levels(df$edu_cat) # First level, "Leaving Certificate"

# Refit model
model1 <- lm(euftf_re~edu_cat,data=df)
summary(model1) 

# Which hypothesis test can we interpret?
# What is the t-test for individual coefficients?
# What is the F-test for all coefficients?

# (2) Hypothesis 2 --------------

# The higher the income, 
# the lower the level of Euroscepticism.

model2 <- lm(euftf_re~hinctnta,data=df)
summary(model2)

# What is the prediction equation?
# Which interpretations can we make?

# (3) Hypothesis 3 --------------

# The higher the trust in politics, 
# the lower the level of Euroscepticism.

model3 <- lm(euftf_re~trstplt,data=df)
summary(model3)

# What is the prediction equation?
# Which interpretations can we make?

# (4) Hypothesis 4 --------------

# The more positive attitudes towards immigration, 
# the lower the level of Euroscepticism.

model4 <- lm(euftf_re~imwbcnt,data=df)
summary(model4)

# What is the prediction equation?
# Which interpretations can we make?

# (5) Putting it all together ------------

# Education--Continuous independent variable
model1 <- lm(euftf_re~eduyrs,data=df)
summary(model1)
nobs(model1) # Number of observations in model

# Add economic dimension
model_eco <- lm(euftf_re~eduyrs + hinctnta,data=df)
summary(model_eco)
nobs(model_eco) # Number of observations in model
# Default in lm to handle missing values is
# to remove incomplete cases

# When comparing models, always make sure models
# are comparable aka use the same sample.

# The easiest solution is to remove rows with missing values
df_na <- df[complete.cases(df), ] 

# Substantial reduction of missing values
# Anything else we could do?

# Let's start again, using only complete cases (df_na)
# Education--Continuous independent variable
model1 <- lm(euftf_re~eduyrs,data=df_na)
summary(model1)

# Add economic dimension
model_eco <- lm(euftf_re~eduyrs + hinctnta,data=df_na)
summary(model_eco)

# What is the prediction equation?
# Which interpretations can we make?

# Add political dimension
model_pol <- lm(euftf_re~eduyrs + hinctnta + trstplt, data=df_na)
summary(model_pol)

# Which interpretations can we make?

# Add cultural dimension
model_cul <- lm(euftf_re~eduyrs + hinctnta + trstplt + imwbcnt, data=df_na)
summary(model_cul)

# Which interpretations can we make?

# Add socio-economic variables 
model_final <- lm(euftf_re~eduyrs + hinctnta + trstplt + imwbcnt + gndr + agea + brncntr, data=df_na)
summary(model_final)

# Which interpretations can we make?

# Get Latex table
stargazer(model1,model_eco,model_pol,model_cul,model_final)

# How to visualize results?
coefplot(model_final) # 95% Confidence intervals (and 50% CIs)
coefplot(model1, add=TRUE, col.pts="gray")

# F test for some coefficients -----

# Does adding economic dimension improve fit?
anova(model1, model_eco, test='F')
summary(model_eco)

# Does adding political dimension improve fit?
anova(model1, model_pol, test='F')
summary(model_pol)

# What about political dimension alone?
model3 <- lm(euftf_re~eduyrs+trstplt,data=df_na) # Refit with df_na
anova(model1, model3, test='F')

# Does adding cultural dimension improve fit?
anova(model1, model_cul, test='F')
summary(model_cul)

# What about cultural dimension alone?
model4 <- lm(euftf_re~eduyrs+imwbcnt,data=df_na) # Refit with df_na
anova(model1, model4, test='F')

# Visualizing regression models with categorical independent variables 

model1 <- lm(euftf_re~edu_cat+imwbcnt,data=df_na)
summary(model1)
levels(df_na$edu_cat)

# Plot
par(mar = c(5, 5, 2, 2)) # Change margins in plot manually
plot(df_na$imwbcnt, df_na$euftf_re)
abline(model1$coefficients[1], model1$coefficients[6],col="black") # edu_cat=0, "Leaving Certificate"
abline(model1$coefficients[1]+model1$coefficients[2], model1$coefficients[6],col="blue") # edu_cat=1, "Advanced Certificate"
abline(model1$coefficients[1]+model1$coefficients[3], model1$coefficients[6],col="green") # edu_cat=2, "Bachelor"
abline(model1$coefficients[1]+model1$coefficients[4], model1$coefficients[6],col="gray") # edu_cat=3, "Junior Cycle"
abline(model1$coefficients[1]+model1$coefficients[5], model1$coefficients[6],col="orange") # edu_cat=4, "Postgraduate"
legend(7, 10, # Legend
       legend=c("Leaving Certificate","Advanced Certificate","Bachelor","Junior Cycle","Postgraduate"),
       col=c("black","blue","green","gray","orange"),
       pch=1) 

# In ggplot
# Obtain predictions and residuals
preds <- augment(model1)
model1$fitted.values # predictions
model1$residuals # residuals

# Plot 
ggplot(df_na, aes(imwbcnt, euftf_re, group = edu_cat)) +
  geom_point(aes(colour = edu_cat)) + # Add points
  geom_line(data = df_na, aes(y = model1$fitted.values, colour = edu_cat)) # Add regression lines
