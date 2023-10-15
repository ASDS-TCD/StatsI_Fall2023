# Applied Statistical Analysis I      
# Tutorial 3: Contingency tables, correlation & bivariate regression                      

# Get working directory
getwd()

# Set working directory 
setwd("C://Users//Administrator//Desktop//TCD ASDS//Applied Statistical Analysis I//StatsI_Fall2023//")
getwd()

# Agenda
# (a.) Contingency tables
# (b.) Chi-square test
# (c.) Correlation
# (d.) Bivariate regression 

# Load data 
df_not_tidy <- read.csv("datasets/movies.csv")

# First step, look at data
View(df_not_tidy)
str(df_not_tidy)
head(df_not_tidy)
summary(df_not_tidy)

# Research questions: 
# Do different genres receive varying critical appreciation?

# (a.) Contingency tables -------

# Load tidy version of data
# The data is prepared using the data_wraning.R script.
df <- readRDS("datasets/movies.rds")
str(df)

# First step, look at data
View(df)
class(df$genre)
levels(df$genre)

# Contingency table 
table(df$genre, # Genre
      df$critics_rating) # Rating

# Subset data, only consider Comedy, Documentary, Drama

# Option 1: 
# Dataframe subsetting: df[rows, columns]
df_s <- df[df$genre=="Comedy" |
           df$genre=="Drama" |
           df$genre=="Documentary", ]
View(df_s)

# Step by step
df$genre # Select column
# For each row, value=="Comedy" or "Drama" or "Documentary"?
df$genre=="Comedy" | df$genre=="Drama" | df$genre=="Documentary" 
# Select rows with value=="Comedy" or "Drama" or "Documentary"
df[df$genre=="Comedy" |
   df$genre=="Drama" |
   df$genre=="Documentary", ] 

# Option 2: Tidyverse subset

# Install and load tidyverse
# Adopted from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

df_s <- subset(df, df$genre %in% c("Comedy","Documentary","Drama"))
View(df_s)
?subset

# Step by step
df$genre # Select column
"Horror" %in% c("Comedy","Documentary","Drama") # Horror is in vector?
"Comedy" %in% c("Comedy", "Documentary","Drama") # Comedy is in vector?
df$genre  %in% c("Comedy","Documentary","Drama") # For each row, value==Comedy or Documentary or Drama?

# Contingency table 
table(df_s$genre, # Genre
      df_s$critics_rating) # Rating

# Problem: Although we filtered our data 
# the underlying levels still exist. Getting rid of
# these, we use the droplevels-function.
class(df_s$genre)
levels(df_s$genre)
df_s$genre <- droplevels(df_s$genre)

# Contingency table 
table(df_s$genre, # Genre
      df_s$critics_rating) # Rating

# Add marginal distributions
addmargins(table(df_s$genre, # Genre
                 df_s$critics_rating)) # Rating

# Joint probability 
prop.table(table(df_s$genre, 
                 df_s$critics_rating))
63/444
# Interpret as estimated probabilities of two specific
# values of each of the variables co-occurring together

# What is the probability of a Comedy and "Rotten"?

# (A) Conditional probability 
# What is the probability of "Rotten", 
# conditional on Comedy?
?prop.table()

# Over rows --> Rating conditional on genre
prop.table(table(df_s$genre, # rows
                 df_s$critics_rating), # columns
           margin = 1) # over rows
63/87

# Add marginal distributions
# Over rows --> Rating conditional on genre
addmargins(prop.table(table(df_s$genre, 
                            df_s$critics_rating), 
                      margin = 1)) # over rows

# Round
round(addmargins(prop.table(table(df_s$genre, 
                       df_s$critics_rating), 
                 margin = 1)), 2)

# Step by step 
round(0.72413793, 2) # Round to two decimals

# (B) Conditional probability 
# What is the probability of Comedy, 
# conditional on "Rotten?

# Over columns --> Genre conditional on rating
addmargins(prop.table(table(df_s$genre, # rows
                            df_s$critics_rating), # columns
                      margin = 2)) # over columns
63/190 

# Bar plot
barplot(prop.table(table(df_s$genre, 
                         df_s$critics_rating), margin=1),
        xlab="Ranking",
        ylab="Proportions",
        main="Critics Rating by Genre",
        beside=TRUE,
        legend.text = TRUE,
        args.legend = list(x=12, 
                           y=0.7, 
                           cex = 0.8, 
                           box.col = "white"))

png(filename = "tutorials/03/barplot.png",
    width = 600,
    height = 350)
barplot(prop.table(table(df_s$genre, 
                         df_s$critics_rating),margin=1),
        xlab="Ranking",
        ylab="Proportions",
        main="Critics Rating by Genre",
        beside=TRUE,
        legend.text = TRUE,
        args.legend = list(x=12, 
                           y=0.7, 
                           cex = 0.8, 
                           box.col = "white"))
dev.off()

# (b) Chi (kai) square test ------------

# Run Chi square test
chisq.test(df_s$genre, 
           df_s$critics_rating)

# Check p-value
sprintf("%.20f",1.097e-12)

# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# A little side note, look at residuals
chi <- chisq.test(df_s$genre, 
                  df_s$critics_rating)
# Returns the Pearson residuals, (observed - expected) / sqrt(expected)
?chisq.test 
chi$residuals

# (c.) Correlation -----

# Is there an association between education and income?

# Load data 
df <- read.csv("datasets/fictional_data.csv")

# Scatter plot 
plot(df$income,df$edu)
plot(df$income,df$edu,
     col=df$cap+1) # Color over third variable (+1, because first color in R is white)

# Improve visualization and save
png(file="tutorials/03/scatter_plot.png")
plot(df$income,
     df$edu,
     col=df$cap+1,
     xlab="Monthly net income (in Euro)",
     ylab="University level education (in years)",
     main="The Relationship between education and income")
# Add legend
legend(1000, 8, # x and y position of legend
       legend=c("Non capital", "Capital"),
       col=c("black","red"),
       pch=1) # Marker type (1 is default)
dev.off()

# Calculate correlation
cor(df$income,df$edu)

# Add to scatter plot
plot(df$income,df$edu)
text(1200, 7, sprintf("Correlation=%s", round(cor(df$income,df$edu),4)))

# (d.) Bivariate regression  -----

# Is there a relationship between education and income?

summary(lm(df$income~df$edu))



