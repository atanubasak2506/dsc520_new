# Assignment: ASSIGNMENT 4.2
# Name: Basak, Atanu
# Date: 2022-04-06

install.packages('sqldf')
library(sqldf)

install.packages('dplyr')
install.packages('plyr')
library(dplyr)
library(plyr)
library(reshape2)

# 1. What are the observational units in this study?
scores <- read.csv("data/scores.csv")
head(scores)
str(scores)

# Identify the variables mentioned in the narrative paragraph and determine which are categorical and quantitative?
# Total points earned is couse : Score (quantitative)
# course grades : Count (quantitative)
# section : section (categorical)

# Create one variable to hold a subset of your data set that contains only the Regular Section and one 
# variable for the Sports Section.

regular <- filter(scores, Section=='Regular')
head(regular)
sports <- filter(scores, Section=='Sports')
head(sports)

#Use the Plot function to plot each Sections scores and the number of students achieving that score. 
#Use additional Plot Arguments to label the graph and give each axis an appropriate label. 
#Once you have produced your Plots answer the following questions:
install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
install.packages("reshape2")
library(reshape2)

plot1 <- ggplot(regular, aes(Score)) + ggtitle('Histogram for Regular') + geom_histogram(bins=10, fill="steelblue", colour="black") + xlab('Score') + ylab('Frequency') + scale_y_continuous(limits = c(0, 5)) + scale_x_continuous(limits = c(0, 500))

plot2 <- ggplot(sports, aes(Score)) + ggtitle('Histogram for Sports') + geom_histogram(bins=10, fill="steelblue", colour="black") + xlab('Score') + ylab('Frequency') + scale_y_continuous(limits = c(0, 5)) + scale_x_continuous(limits = c(0, 500))

require(gridExtra)
grid.arrange(plot1, plot2, nrow=1)

ggplot(data=scores, aes(x=Score, y=Count, group=Section)) +  geom_line(aes(color=Section)) +  geom_point(aes(color=Section))

#Comparing and contrasting the point distributions between the two section, looking at 
#both tendency and consistency: Can you say that one section tended to score more points than the other? 
#Justify and explain your answer.

#the sports section tended to score more, because right side of the histogram have higher frequency.

#Did every student in one section score more points than every student in the other section? If not, 
#explain what a statistical tendency means in this context.

#every students from sports score equal or more compared to regular section.


#What could be one additional variable that was not mentioned in the narrative that could be 
#influencing the point distributions between the two sections?
#If it would be a grade wise attribute that will help onfurther analysis.




acs <- read.csv("data/acs-14-1yr-s0201.csv")
head(acs,15)

#Use the apply function on a variable in your dataset
fn <- function(x) {ifelse(any(x >= 80), 'good grade', 'bad grade')}
grades <- apply(acs['HSDegree'], MARGIN=1, FUN=fn) 
grades

#Use the aggregate function on a variable in your dataset
apply(acs['HSDegree'], MARGIN=2, FUN=sum) 

#Use the plyr function on a variable in your dataset - more specifically, 
#I want to see you split some data, perform a modification to the data, and then bring it back together
head(acs)
# want to see the state wise average HSDegree
library(stringr)
library(dplyr)
acs['state'] <- word(acs$Geography,2,sep=",")
acs %>%
  group_by(state) %>%
  summarise(mean = mean(HSDegree), n = n())

#Check distributions of the data
head(acs)
library(gridExtra)
qqnorm(acs$HSDegree, main = "Normal Q-Q plot HSDegree")
qqnorm(acs$BachDegree, main = "Normal Q-Q plot BachDegree")
#Both HSDegree and Bachelor Degree follow normal distribution

#Identify if there are any outliers
boxplot(acs$HSDegree, main = "Boxplot HSDegree") #have outlier which is below 65
boxplot(acs$BachDegree, main = "Boxplot BachDegree") #This does not have any outlier.

#Create at least 2 new variables

acs['state'] <- word(acs$Geography,2,sep=",")
head(acs$state)

fn <- function(x) {ifelse(any(x >= 80), 'good grade', 'bad grade')}
acs['grades'] <- apply(acs['HSDegree'], MARGIN=1, FUN=fn) 
head(acs$grades)
