# Applied Statistical Analysis I      
# Tutorial 8: Repetition of bivariate regression                   

# Set working directory as path where file is located
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Agenda 
# (a.) Gathering data
# (b.) Introduction to tidyverse
# (c.) Descriptive analysis
# (d.) Regression analysis

# Research questions: 
# Is there a relationship between income and child mortality?

# Install and load packages
# Adopted from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
if(!require(wbstats)){
  install.packages("wbstats")
  library(wbstats)}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)}

if(!require(stargazer)){
  install.packages("stargazer")
  library(stargazer)}

# Load zipped data from url
if(!require(readr)){
  install.packages("readr")
  library(readr)}

# (a.) Data wrangling ----------

# What is an API?
# https://medium.com/geekculture/a-beginners-guide-to-apis-9aa7b1b2e172

# Load data from World Bank API
wb <- wb(country=c("AF","BRA","ITA","NGA","SWE","UGA"), 
         indicator=c("NY.GDP.PCAP.CD", # GDP per capita (current US$)
                     "SP.POP.TOTL", # Population, total 
                     "SE.SEC.ENRR", #  School enrollment, secondary (% gross) 
                     "SH.DYN.MORT"), # Mortality rate, under-5 (per 1,000 live births) 
         startdate = 2000, enddate = 2020)

# Data formats--wide and long
# https://www.statology.org/long-vs-wide-data/

# Reshape data from long to wide
wb_re <- reshape(wb[, c("country","iso3c","date","indicatorID","value")], # df
                 timevar = "indicatorID",
                 idvar = c("country","date","iso3c"),
                 direction = "wide")

# Load Quality of Government data
qog <- read_csv("https://www.qogdata.pol.gu.se/data/qog_bas_ts_jan23.csv")

# How can we combine data from different sources?
# https://guides.nyu.edu/quant/merge

# Merge 
df <- merge(wb_re, # Left df
            qog[, c("ccodealp","year","bmr_dem")], # Right df
            by.x=c("date","iso3c"), # merge variables in left
            by.y=c("year","ccodealp"), # merge variables in right
            all.x=TRUE, # merge operation, only keep left
            sort=FALSE) # Do not sort observations

# Rename columns
names(df)
names(df)[4] <- "gdp_per_cap"
names(df)[5] <- "pop_size"
names(df)[6] <- "sec_enrol"
names(df)[7] <- "mort"
names(df)[8] <- "democracy"
View(df)

# Save df
write.csv(df, "df_income_mortality.csv")

# Exercise A -----------------
# Add another variable either from the World Bank data or 
# the Quality of Government dataset. 
# Alternatively, add a variable from a new data source. 

# Load zipped data from url
temp <- tempfile(fileext = ".zip") # Initiate temporary file
# Download zip as temporary file
download.file("https://ucdp.uu.se/downloads/ged/ged231-csv.zip", temp, mode="wb")
ucdp <- read_csv(temp) # Read data
View(ucdp)

# Load some data that you find interesting. 
# What is the level of analysis? 
# Or rather, what is the 'unit' of each row?

# Data on armed conflicts: 
# UCDP Georeferenced Event Dataset (GED)
# https://ucdp.uu.se/downloads/index.html#ged_global
View(ucdp)

# Other common data sources:  
# Afrobarometer (survey), https://www.afrobarometer.org/
# Example, Sierra Leone Round 8 data (2021) 
# Codebook: https://www.afrobarometer.org/wp-content/uploads/2022/06/afrobarometer_codebook_srl_r8_en_2021-06-18.pdf

# Load sav data file 

# Install required package
if(!require(haven)){
  install.packages("haven")
  library(haven)}

# Load sav file
afro <- read_sav("https://www.afrobarometer.org/wp-content/uploads/2023/06/SRL_R9.data_.final_.wtd_release.14Feb23.sav")

# Look at data
View(afro)
afro$Q1

# European Social Survey (ESS), https://www.europeansocialsurvey.org/
# European Commission, https://data.europa.eu/en
# American National Election Survey (ANES), https://electionstudies.org/
# Information on api: https://github.com/jamesmartherus/anesr
# Comparative Legislators Database, https://complegdatabase.com/
# Information api: https://cran.r-project.org/web/packages/legislatoR/vignettes/legislatoR.html
# Irish Polling Indicator, https://pollingindicator.com/#data
# General Social Survey Cumulative Data, Codebook: https://gss.norc.org/documents/codebook/gss_codebook.pdf
# Information on api: https://kjhealy.github.io/gssr/

# How can we merge data on different levels of analysis?

# What is the level of analysis?
View(ucdp)
View(df)

# How to aggregate data?
# Which variables do we need?
ucdp[,c("country", "year", "best")] 

# Aggregate ucdp from event to the country-year level. 
ucdp_agg <- aggregate(ucdp$best, # Variable to aggregate
                      list(ucdp$country, ucdp$year), # Group variables
                      FUN=sum) # How to aggregate, sum
View(ucdp_agg)

# Merge 
df <- merge(df, # Left df
            ucdp_agg, # Right df
            by.x=c("date","country"), # merge variables in left
            by.y=c("Group.2","Group.1"), # merge variables in right
            all.x=TRUE, # merge operation, only keep left
            sort=FALSE) # Do not sort observations

# Rename column
names(df)[9] <- "best"
View(df)

# Why do we see missing values?
# Missing values mean that country-year was not in UCDP data, 
# so there were no fatalities. 
# We can replace with 0s. 
df$best[is.na(df$best)] = 0

# Step by step
df$best # Select variable
is.na(df$best) # Check if value is na
df$best[is.na(df$best)] # Subset rows which are na
df$best[is.na(df$best)] = 0 # Replace with 0

# Save df
write.csv(df, "df_income_mortality_best.csv")

# (b.) Data wrangling -------

# Load df
df <- read_csv("df_income_mortality_best.csv")
View(df)

# Get unique countries in df
df_uni <- select(df, country) # Select variable
df_uni <- distinct(df_uni, country) # Get unique values
df_uni

# Get unique countries in df, using the pipe
df %>% 
  select(country) %>% 
  distinct(country) 

# Filter (subset is base R)
df_s <- filter(df, country %in% c("Afghanistan","Italy")) 
df_s

# Get the mean income and max child mortality for each year
df_grouped <- group_by(df, date) # Group by year
df_mean_inc <- summarize(df_grouped, 
                         n=n(), # Counts
                         mean_inc=mean(gdp_per_cap), # Mean
                         max_mort=max(mort)) # Max
df_mean_inc

# What about missing values?
?mean
# Get the mean income and max child mortality for each year
df_grouped <- group_by(df, date) # Group by year
df_mean_inc <- summarize(df_grouped, 
                         n=n(), # Counts
                         mean_inc=mean(gdp_per_cap, na.rm=TRUE), # Mean
                         max_mort=max(mort)) # Max
df_mean_inc

# Check if df has missing values
sum(is.na(df$gdp_per_cap))
sum(is.na(df$mort))

# Replace missing values 

# Option I: Replace missing values with zero, but be careful!
df_na <- replace(df, is.na("gdp_per_cap"), 0) # one variable
df_na <- df %>% replace(is.na(.), 0) # all variables

# Option II: Replace missing values with mean
df_na <- df # Copy
?replace_na
df_na$gdp_per_cap <- replace_na(data=df_na$gdp_per_cap, 
                                replace=mean(df_na$gdp_per_cap, # Value to replace NA with
                                             na.rm = TRUE))

# Option III: Replace missing values with group mean
df_na <- group_by(df_na, country) # Group
df_na <- mutate(df_na, # Replace with mean if value is missing
                sec_enrol = ifelse(is.na(sec_enrol), 
                                   mean(sec_enrol, na.rm = TRUE), 
                                   sec_enrol))

# Step by step:
# ifelse(test, yes, no) 
# --> if is.na is True, replace with mean,
# if is.na is False, replace with value

# Re-coding variables, in Base R
# Create categorical income variable
df_na$income_cat <- 0 # Create empty variable
summary(df_na$gdp_per_cap) # Check quantile
summary(df_na$gdp_per_cap)[2]
df_na$income_cat[df_na$gdp_per_cap>summary(df_na$gdp_per_cap)[2]] <- 1 # Replace step by step
df_na$income_cat[df_na$gdp_per_cap>summary(df_na$gdp_per_cap)[3]] <- 2
df_na$income_cat[df_na$gdp_per_cap>summary(df_na$gdp_per_cap)[5]] <- 3

# Convert into factor
typeof(df_na$income_cat)
df_na$income_cat <- factor(df_na$income_cat, 
                           labels = c("low","medium_low","medium_high","high"))
typeof(df_na$income_cat)
is.factor(df_na$income_cat)
levels(df_na$income_cat)

# Re-coding variables, in tidyverse
# Create categorical income variable
quantile(df_na$gdp_per_cap) # Check quantiles
df_na <- mutate(df_na, income_cat2=cut(gdp_per_cap, # Variable
                                       breaks=quantile(df_na$gdp_per_cap), # Breaks
                                       labels=c("low","medium_low","medium_high","high"))) # Labels
is.factor(df_na$income_cat2)

# Step by step: 
cut(df_na$gdp_per_cap,breaks=c(0,600,800,Inf)) # Define breaks
cut(df_na$gdp_per_cap,breaks=quantile(df_na$gdp_per_cap)) # Use quantiles as breaks
cut(df_na$gdp_per_cap,breaks=quantile(df_na$gdp_per_cap),labels=c("low","medium_low","medium_high","high")) # Add labels

# Drop missing values 
df <- df[complete.cases(df), ]

# Exercise B -----------------

# Get the mean income for democracies and autocracies. 

# Base R
mean(df_na[df_na$democracy==0,]$gdp_per_cap)
mean(df_na[df_na$democracy==1,]$gdp_per_cap)

# Step by step:
df_na[df_na$democracy==0,] # Subset rows
df_na[df_na$democracy==0,]$gdp_per_cap # Subset columns 
mean(df_na[df_na$democracy==0,]$gdp_per_cap) # Calculate mean

# Tidyverse:
df_grouped <- group_by(df_na, democracy) # Group by regime type
df_mean_inc <- summarize(df_grouped, 
                         mean_inc=mean(gdp_per_cap)) # Mean
df_mean_inc

# Get the mean income for Afghanistan. 

# Base R
mean(df_na[df_na$country=="Afghanistan",]$gdp_per_cap)

# Tidyverse: 
df_sub <- filter(df_na, country=="Afghanistan") 
df_sub <- summarise(df_sub, mean = mean(gdp_per_cap))
df_sub

# Create a variable, which measures the t-1 lag of GDP per capita.

# What is a lagged variable?
# "A lagged value of a variable is the value of the variable 
# from a previous time period. For instance, a lagged value 
# from one period previous to the current time is referenced
# as being from time t−1" (Kellstedt and Whitten 2018, p.257).

# Group data, lagged variable needs to be country specific
df_na_g <- arrange(df_na, country, date) # Important, sort rows beforehand!
df_na_g <- group_by(df_na_g, country) # Group by country
df_na <- mutate(df_na_g, 
                income_lag = lag(gdp_per_cap, n = 1)) # Calculate t-1 lag

# Use piping
df_na <- 
  df_na %>%
  arrange(country, date)  %>%
  group_by(country) %>%
  mutate(income_lag = lag(gdp_per_cap, n = 1))

# Create a variable, which measure regime change. 

# Step 1: Create 'help' variable, lagged regime type
View(df_na) # Make sure data is sorted
df_na_g <- group_by(df_na, country) # Group data by country
df_na <- mutate(df_na_g, 
                democracy_lag = lag(democracy, n = 1)) # Calculate t-1 lag

# Step 2: Recode
df_na$regime_change <- 0 # Create empty variable
# Replace with 1 if current regime type is the same as t-1 regime type
df_na$regime_change[df_na$democracy_lag!=df_na$democracy] <- 1 

# (c.) Descriptive analysis -------

# Key components of a ggplot:
# 1. data 
# 2. aesthetic mappings --> aes()
# 3. layers, each layer has geometric object --> geoms

# For example: 
# ggplot(data = <data>, mapping = aes(<mappings>)) + --> data, with aesthetic mapping
#     <geom_function>() + --> geometric object

# Scatter plot
scatter <-
  ggplot(data = df_na, # --> data
         mapping = aes(x = gdp_per_cap, 
                       y = mort)) +  # --> aesthetic mapping
  geom_point() # --> geometric object, scatter plot

# Print plot object
scatter

# Scatter plot, log-transform income
hist(df_na$gdp_per_cap)
hist(log(df_na$gdp_per_cap))

# Scatter plot
scatter <-
  ggplot(data = df_na, 
         mapping = aes(x = log(gdp_per_cap), # log-transform
                       y = mort)) + 
  geom_point() 
scatter

# Scatter plot 
scatter <-
  ggplot(data = df_na, 
         mapping = aes(x = log(gdp_per_cap), # log-transform
                       y = mort,
                       size = sec_enrol)) + 
  geom_point() 
scatter

# Improve visualization and save
scatter <- ggplot(data = df_na, 
                  mapping = aes(x = log(gdp_per_cap), # log-transform
                                y = mort,
                                size = sec_enrol)) + 
  geom_point()  +
  labs(x = "GDP per capita (log)", # Add labels
       y = "Child mortality",
       size = "Population size") +
  theme_classic() + # Change theme
  theme(legend.box.background = element_rect(size = 0.1), # Change background
        legend.position = c(0.85, 0.85)) # Change position of legend
ggsave(scatter, file = "scatter.png")
scatter

# Exercise C -----------------

# Add a regression line to your scatter plot. 

# Scatter plot
scatter <-
  ggplot(data = df_na, 
         mapping = aes(x = log(gdp_per_cap),
                       y = mort)) + 
  geom_point() +
  geom_smooth(method='lm',col="black") # Add regression line
scatter

# Create a new "scatter plot", which differentiates between
# democracies and autocracies. Add the regression lines. 

# Scatter plot
scatter <-
  ggplot(data = df_na, 
         mapping = aes(x = factor(democracy), # Over regime type
                       y = mort, 
                       col = factor(democracy))) + 
  geom_point() 
scatter 

# Access regression coefficients
model <- lm(mort ~ democracy, data=df_na)
summary(model)
model$coefficients
model$coefficients[1]

# Add regression lines
scatter <-
  ggplot(data = df_na, 
         mapping = aes(x = factor(democracy), # Over regime type
                       y = mort, 
                       col = factor(democracy))) + 
  geom_point() +
  geom_hline(yintercept=model$coefficients[1],col="red") + # Autocracies
  geom_hline(yintercept=model$coefficients[1]+model$coefficients[2],col="turquoise") # Democracies
scatter

# Equal to the mean differentiated by regime type
mean(df_na[df_na$democracy==0,]$mort)
mean(df_na[df_na$democracy==1,]$mort)
model$coefficients[1]+model$coefficients[2]

# (d.) Regression analysis -----

# Fit model
model <- lm(mort ~ gdp_per_cap, data=df_na)
summary(model)

# Re-scale income
df_na$gdp_per_cap_1000 <- df_na$gdp_per_cap/1000
model <- lm(mort ~ gdp_per_cap_1000, data=df_na)
summary(model)

# Export Latex table 
stargazer(model)

# How to include a categorical independent variable?

# Fit model
model <- lm(mort ~ democracy, data=df_na)
summary(model)

# Exercise D -----------------

# Estimate a regression model, which interests you. 
# What is you research question? How would you present
# your results?

# What is the relationship between income and armed conflict?
model <- lm(best~gdp_per_cap, data=df_na)
summary(model)

model <- lm(best~gdp_per_cap_1000, data=df_na)
summary(model)

# Export regression table
stargazer(model)

# Scatter plot
scatter <-
  ggplot(data = df_na, 
         mapping = aes(x = gdp_per_cap_1000,
                       y = best)) + 
  geom_point() +
  geom_smooth(method='lm',col="black")
scatter


