#QUESTION 1: POLITICAL SCIENCE

#Let's create a contingency table (as a matrix) with the given data:
matrix <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)
data_frame <- as.data.frame(matrix)
rownames(data_frame) <- c('Upper class', 'Lower class')
colnames(data_frame) <- c('Not stopped', 'Bribe requested', 'Stopped/given warning')

data_frame
View(data_frame)

#(a) Calculate the x^2 test statistic by hand/manually (in R):
#fe=(row total/grand total) * column total
upper_class <- rowSums(data_frame['Upper class', ]) #27
lower_class <- rowSums(data_frame['Lower class', ]) #15
not_stopped <-  sum(data_frame$'Not stopped') #21
bribe_requested <- sum(data_frame$'Bribe requested') #13
stopped <- sum(data_frame$'Stopped/given warning') #8

row_totals <- c (upper_class, lower_class)
column_totals <- c(not_stopped, bribe_requested, stopped)

grand_total <- sum(row_totals) 

fo1 <- 14
fo2 <- 6
fo3 <- 7
fo4 <- 7
fo5 <- 7
fo6 <- 1

fe1 <- (upper_class/grand_total)*not_stopped #13.5
fe2 <- (upper_class/grand_total)*bribe_requested #8.36
fe3 <- (upper_class/grand_total)*stopped #5.14
fe4 <- (lower_class/grand_total)*not_stopped #7.5
fe5 <- (lower_class/grand_total)*bribe_requested #4.64
fe6 <- (lower_class/grand_total)*stopped #2.86 

x21 <- (fo1-fe1)^2/fe1 #0.0185
x22 <- (fo2-fe2)^2/fe2 #0.665
x23 <- (fo3-fe3)^2/fe3 #0.671
x24 <- (fo4-fe4)^2/fe4 #0.0333
x25 <- (fo5-fe5)^2/fe5 #1.2
x26 <- (fo6-fe6)^2/fe6 #1.21

chisq_result <- sum(x21, x22, x23, x24, x25, x26) #3.791168
chisq_result

#My result by hand was 3.79, R-calculations also gave us 3.79.
#That's great, but let's also check via an embedded formula:
chi_square <- chisq.test(data_frame)
chi_square 
chi_square$statistic #brilliant, X-squared = 3.791168 (same result).

#(b) Now calculate the p-value from the test statistic you just created (in R).
p_value <- chi_square$p.value #0.1502306
p_value

# (c) Calculate the standardized residuals for each cell 
#and put them in the table below.

#If we extract the 'residuals' from our R-run Chi-squared test, 
#we will get results that are Raw residuals/Pearson's residuals 
#p.3 here: https://scholarworks.umass.edu/cgi/viewcontent.cgi?article=1269&context=pare#:~:text=There%20are%20at%20least%20four,the%20chi%2Dsquare%20test%20result.
#(F observed - F expected) / square root of F expected

chi_square$residuals

#I calculated them by hand and have the same numbers,
#BUT! 
#They are not what we are looking for.

#From my understanding, we are looking for
#what Agresti and Finlay call standardized residuals as well, 
#but they are also called Adjusted Standardized Residuals (ASR):
#(F observed - F expected) / square root of (F expected * (1-row proportion) * (1-column proportion)
#We can also extract them from our Chi-squared test results, 
#but they are stored under 'stdres':

chi_square$stdres

#And let's now calculate them manually:

rp1 <- upper_class/grand_total #row proportion for row 1: 0.643
rp2 <- lower_class/grand_total #row proportion for row 2: 0.357
cp1 <- not_stopped/grand_total #column proportion for column 1: 0.5
cp2 <- bribe_requested/grand_total #column proportion for column 2: 0.3095
cp3 <- stopped/grand_total #column proportion for column 3: 0.19

asr1 <- (fo1-fe1)/sqrt(fe1*(1-rp1)*(1-cp1)) #0.322
asr2 <- (fo2-fe2)/sqrt(fe2*(1-rp1)*(1-cp2)) #-1.64
asr3 <- (fo3-fe3)/sqrt(fe3*(1-rp1)*(1-cp3)) #1.52
asr4 <- (fo4-fe4)/sqrt(fe4*(1-rp2)*(1-cp1)) #-0.322
asr5 <- (fo5-fe5)/sqrt(fe5*(1-rp2)*(1-cp2)) #1.64
asr6 <- (fo6-fe6)/sqrt(fe6*(1-rp2)*(1-cp3)) #-1.52

#Let's create a similar contingency table but with ASRs:
matrix2 <- matrix(c(asr1, asr2, asr3, asr4, asr5, asr6), nrow = 2, byrow = TRUE)
asr_table <- as.data.frame(matrix2)
rownames(asr_table) <- c('Upper class', 'Lower class')
colnames(asr_table) <- c('Not stopped', 'Bribe requested', 'Stopped/given warning')

asr_table

#QUESTION 2: ECONOMICS

#You need to estimate the effect
#of the reservation policy on the number 
#of new or repaired drinking water facilities 
#in the villages.

data <- read.csv('https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv')

#(b) Run a bivariate regression to test this hypothesis

#I will use the lm() function first (R won't lie to us re: calculations!):

model <- lm(data$water~data$reserved)
summary(model)

#Now, I will calculate the bivariate regression 'by hand'
#(being guided by pp. 58-60 in Week 3 slides):

#1) Calculating means:
meanY <- mean(data$water)
meanX <- mean(data$reserved)
#2) Calculating sums
sumY <- sum(data$water)
sumX <- sum(data$reserved)
sum1 <- sum((data$water - mean(data$water)) * (data$reserved - mean(data$reserved)))
sum2 <- sum((data$reserved - mean(data$reserved))^2)
#3) Calculating an estimator for beta (slope):
beta <- sum1/sum2 #9.252423
#4) Calculating an estimator for alpha (intercept):
alpha <- meanY-(beta*meanX) #14.73832

beta
#Our slope = 9.252423.
alpha
#Our intercept = 14.73832.
#Exactly like R calculated for us with lm() function.
