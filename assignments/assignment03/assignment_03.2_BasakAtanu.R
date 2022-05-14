# Assignment: ASSIGNMENT 3.2
# Name: Basak, Atanu
# Date: 2022-03-29

## Load the ggplot2 package
install.packages("ggplot2")
library(ggplot2)


## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/atanu/Documents/BellevueUniversity_MSDS/DSC520/Repository/dsc520")

## Load the `data/r4ds/heights.csv` to
us_census_df <- read.csv("data/acs-14-1yr-s0201.csv")
head(us_census_df)

#What are the elements in your data (including the categories and data types)?
str(us_census_df)
#Please provide the output from the following functions: str(); nrow(); ncol()
str(us_census_df)
nrow(us_census_df)
ncol(us_census_df)
#Create a Histogram of the HSDegree variable using the ggplot2 package.

#Set a bin size for the Histogram.
#Include a Title and appropriate X/Y axis labels on your Histogram Plot.
ggplot(us_census_df, aes(HSDegree)) + geom_histogram(bins=20) + ggtitle('Histogram for HSDegree') + xlab('Marks Obtained') + ylab('Frequency')

# Answer the following questions based on the Histogram produced:
#   
#   Based on what you see in this histogram, is the data distribution unimodal? No, Its not Unimodal
#   Is it approximately symmetrical? No.
#   Is it approximately bell-shaped? No.
#   Is it approximately normal? No.
#   If not normal, is the distribution skewed? If so, in which direction? Its negatively skewed.
#   Include a normal curve to the Histogram that you plotted.

ggplot(data = us_census_df) + 
  geom_histogram(mapping = aes(x = HSDegree, y=..density..), bins=20, fill="steelblue", colour="black") +
  ggtitle("Histogram for HSDegree") + xlab('Marks Obtained') + ylab('Frequency') +
  stat_function(fun = dnorm, args = list(mean = mean(us_census_df$HSDegree), sd = sd(us_census_df$HSDegree)))

# Explain whether a normal distribution can accurately be used as a model for this data. No

#Create a Probability Plot of the HSDegree variable.
install.packages("qqplotr")
library(qqplotr)
ggplot(mapping = aes(sample = us_census_df$HSDegree)) + stat_qq_point(size = 2) + stat_qq_line(color="green")

# Answer the following questions based on the Probability Plot:
#   
# Based on what you see in this probability plot, is the distribution approximately normal? Explain how you know.
# No the distribution is not approximately normal and the probability plot for QQ point is not straight line.
# If not normal, is the distribution skewed? If so, in which direction? Explain how you know.
# Yes its skewed in negative direction.

#Now that you have looked at this data visually for normality, you will now quantify normality with numbers using the stat.desc() function. 
#Include a screen capture of the results produced.
install.packages ("pastecs")
install.packages("psych")
library (pastecs)
library (psych)

stat.desc(us_census_df$HSDegree)
#mean is less than median so its skewed to the left, i.e negative skewness.

#In several sentences provide an explanation of the result produced for skew, kurtosis, and z-scores. 
describe(us_census_df$HSDegree)

# The skewness of this attribute is -1.67 which means its negavitely skewed,i.e more values are concentrated on the right 
# side, the Kurtosis is 4.35 which is positive, that means its heavy tailed distrution.

# calculate z
degree.z <- (us_census_df$HSDegree - mean(us_census_df$HSDegree)) / sd(us_census_df$HSDegree)

# plot z-score
plot(degree.z, type="o", col="green")
# The Z score tells about how many standard deviation the values are apart from the mean, overall the values are 
# consistent, but there are two students who did exceptionally bad compared to the average.

#In addition, explain how a change in the sample size may change your explanation?

# Z Score can be obtained by deviding skewness by standard error. As standard error reduces by increasing the sample size.
# so z-test under null hypothisis of normal distribution tend to be easily rejected for larger sample.

