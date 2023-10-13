# Applied Statistical Analysis I      
# Tutorial 5: Bivariate regression                   

# Get working directory
getwd()

# Set working directory 
setwd("/Users/hannahfrank/StatsI_Fall2023_prep")
getwd()

# Agenda 
# (a.) Gathering data
# (b.) Data wrangling
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

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)}

if(!require(stargazer)){
  install.packages("stargazer")
  library(stargazer)}

# (a.) Gathering data ----------

# What is an API?
# https://medium.com/geekculture/a-beginners-guide-to-apis-9aa7b1b2e172

# Load data from World Bank API
wb <- wb(country=c("AF","BRA","ITA","NGA","SWE","UGA"), 
         indicator=c("NY.GDP.PCAP.CD", # GDP per capita (current US$)
                     "SP.POP.TOTL", # Population, total 
                     "SE.SEC.ENRR", #  School enrollment, secondary (% gross) 
                     "SH.DYN.MORT"), # Mortality rate, under-5 (per 1,000 live births) 
         startdate = 2000, enddate = 2020)

# Data formats--Wide and long
# https://www.statology.org/long-vs-wide-data/

# Reshape data from long to wide (put rows in columns)
wb_re <- reshape(wb[, c("country","iso3c","date","indicatorID","value")], # df
                 timevar = "indicatorID", # New columns
                 idvar = c("country","date","iso3c"), # Identifiers for rows
                 direction = "wide")

# Load Quality of Government data
qog <- read_csv("https://www.qogdata.pol.gu.se/data/qog_bas_ts_jan23.csv")

# How can we combine data from different sources?
# https://guides.nyu.edu/quant/merge

# Merge 
df <- merge(wb_re, # Left df
            qog[, c("ccodealp","year","bmr_dem")], # Right df
            by.x=c("date","iso3c"), # Merge variables in left
            by.y=c("year","ccodealp"), # Merge variables in right
            all.x=TRUE, # Merge operation, only keep left
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
write.csv(df, "datasets/df_income_mortality.csv")

# (b.) Data wrangling -------

# Load df
df <- read_csv("datasets/df_income_mortality.csv")
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

# Check if df has missing values
sum(is.na(df$gdp_per_cap))
sum(is.na(df$mort))

# Replace missing values 

# Option I: Replace missing values with zero, but be careful!
df_na <- df %>% replace(is.na(.), 0)

# Option II: Replace missing values with mean
df_na <- df # Copy
?replace_na
df_na$gdp_per_cap <- replace_na(data=df_na$gdp_per_cap, 
                                replace=mean(df_na$gdp_per_cap, # Value to replace NA with
                                             na.rm = TRUE))
# Step by step: 
mean(df_na$gdp_per_cap, na.rm = TRUE)

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
df_na$income_cat[df_na$gdp_per_cap>756.8] <- 1 # Place step by step
df_na$income_cat[df_na$gdp_per_cap>3171.6] <- 2
df_na$income_cat[df_na$gdp_per_cap>31768.3] <- 3

# Convert into factor
typeof(df_na$income_cat)
df_na$income_cat <- factor(df_na$income_cat, 
                           labels = c("low","medium_low","medium_high","high"))

# Re-coding variables, in tidyverse
# Create categorical income variable
quantile(df_na$gdp_per_cap) # Check quantiles
df_na <- df_na # Copy
df_na <- mutate(df_na, income_cat2=cut(gdp_per_cap, 
                                       breaks=quantile(df_na$gdp_per_cap),
                                       labels=c("low","medium_low","medium_high","high")))
typeof(df_na$income_cat)

# Step by step: 
cut(df_na$gdp_per_cap,breaks=c(0,600,800,Inf)) # Define breaks
cut(df_na$gdp_per_cap,breaks=quantile(df_na$gdp_per_cap)) # Use quantiles as breaks
cut(df_na$gdp_per_cap,breaks=quantile(df_na$gdp_per_cap),labels=c("low","medium_low","medium_high","high")) # Add labels

# Drop missing values 
df <- df[complete.cases(df), ]

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
ggsave(scatter, file = "tutorials/05/scatter.png")
scatter

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
