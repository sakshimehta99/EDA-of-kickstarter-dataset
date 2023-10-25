install.packages("FSA")
library("FSA")
install.packages("FSAdata")
library("FSAdata")
install.packages("magrittr")
library("magrittr")
install.packages("dplyr")
library("dplyr")
install.packages("plotrix")
library("plotrix")
install.packages("ggplot2")
library("ggplot2")
install.packages("psych")
library("psych")
install.packages("tidyverse")
library("tidyverse")
install.packages("stringr")
library(stringr)
install.packages("tidyverse", dependencies = TRUE)
install.packages("moments")
library(moments)
install.packages("dyno")
library(dyno)
install.packages("ggthemes")
library(ggthemes)
install.packages("lubridate")
library(lubridate)
install.packages("rworldmap")
library(rworldmap)
install.packages("gplots")
library(gplots)
install.packages("knitr")
library(knitr)


#loading dataset

kicks <- read.csv(choose.files())

kicks

#checking if there are any missing values
sapply(kicks, function(x) sum(is.na(x)))

#dropping columns that have NA or missing values like usd.pledged

kicks <- kicks[,-13]

colnames(kicks)[13] <- "usd.pledged"

# Filtering the data with required main_category as Film & Video

kicks_FV <- filter(kicks, main_category == "Film & Video")

#finding population mean
mean_pledge <- mean(kicks$usd.pledged, na.rm=TRUE)
mean_pledge


# One Sample t-test
# The one-sample t-test is a statistical hypothesis test used to determine whether an unknown population mean is different from a specific value.

# null hypothesis: There is no difference between the amount of USD pledged in sample and population 

#alternative hypothesis : Mean USD pledge is different in both sample selected and the population data.

# As out data is large enough we do not need to check whether the data follow a normal distribution.

#Computing one-sample t-test where we want to know if the avg usd pledged for films and video differs from the usd pledged for entire population 
t.test(kicks_FV$usd.pledged, mu = mean_pledge)

# Results : 
# One Sample t-test
# 
# data:  kicks_FV$usd.pledged
# t = -17.67, df = 63584, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 9058.924
# 95 percent confidence interval:
#   5836.194 6479.763
# sample estimates:
#   mean of x 
# 6157.978 

#Interpretation :
# The p-value of the test is less than 2.2e-16, which is less than the significance level alpha = 0.05. 
# We can conclude that the mean usd pledged for films and videos category is significantly different from mean of entire population.



##### UNPAIRED two sample t-test
#The unpaired two-samples t-test is used to compare the mean of two independent groups.


# Research  question 2:

#Whether the mean Duration of projects that are successful is less than the mean duration of projects that failed.
#Null hypothesis : success_duration is equal to fail_duration
#Alternative hypothesis : fail_duration not equal success_duration'

#We can find the length of each project by taking the difference between the project deadline and the project launch date, then dividing by the number of whole days. Now let's plot the success rate of projects based on their length in days.



kicks$duration <- interval(ymd_hms(kicks$launched), ymd(kicks$deadline)) %/% days(1)
kicks
#fetching and separating successful and failed state
kick_success <- filter(kicks, state=="successful")
kick_failure <- filter(kicks, state=="failed")

#getting the vectors
suc_vec <- c(kick_success$duration)
suc_vec
fail_vec <- c(kick_failure$duration)
fail_vec

# Compute unpaired two-samples t-test
t_test <- t.test(suc_vec,fail_vec,)
t_test

# Result:
#   Welch Two Sample t-test
# 
# data:  suc_vec and fail_vec
# t = -68.989, df = 307678, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -3.102592 -2.931174
# sample estimates:
#   mean of x mean of y 
# 31.15647  34.17335 

# Interpretation:
# The p-value of the test is  < 2.2e-16
# which is less than the significance level alpha = 0.05. 
# We can conclude that average duration of successful project is significantly different from average duration of failed project. That is we reject the null hypothesis. 


# Question 3

#If the funding in Technology and art similar or not!

#Null Hypothesis : There is no difference in funding.
#Alternative hypothesis : There is difference in funding. 

#filtering out and selecting the required data

kick_tech <- filter(kicks, main_category=="Technology")
kick_tech

kick_art <- filter(kicks, main_category =="Art")
kick_art

#getting the vectors


tech_pledge <- c(kick_tech$usd.pledged)
art_pledge <- c(kick_art$usd.pledged)

#Two sample T.test
t_test2 <- t.test(tech_pledge,art_pledge)
t_test2
# Result
# Welch Two Sample t-test
# 
# data:  tech_pledge and art_pledge
# t = 25.106, df = 34800, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   16530.36 19330.01
# sample estimates:
#   mean of x mean of y 
# 21151.171  3220.987


# Interpretation of the result
#p-value of the test is < 2.2e-16, which is less than the significance level alpha = 0.05. 
#We can conclude that there is significance difference in the investment amount of art and technology. 
#Null hypothesis is rejected. 