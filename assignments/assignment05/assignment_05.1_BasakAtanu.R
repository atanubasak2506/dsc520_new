# Assignment: ASSIGNMENT 5.1
# Name: Basak, Atanu
# Date: 2022-04-13
install.packages('pander')

install.packages("readxl")
library("readxl")

setwd("C:\\Users\\atanu\\Documents\\BellevueUniversity_MSDS\\DSC520\\Repository\\dsc520")
housing <- read_excel("data\\week-7-housing.xlsx")
head(housing)
ncol(housing)
str(housing, list.len=24)

#Using the dplyr package, use the 6 different operations to analyze/transform the data - GroupBy, 
#Summarize, Mutate, Filter, Select, and Arrange - Remember this isn't just modifying data, you are 
#learning about your data also - so play around and start to understand your dataset in more detail
install.packages("dplyr")
library(dplyr)
head(housing$year)
housing$'sale year'=substr(housing$`Sale Date`,1,4)
head(housing)
#Group by 
# Group by and summarize function for dataframe in R using pipe operator 
housing %>% group_by(housing$'sale year') %>% summarise(Mean_sales = mean(housing$`Sale Price`),n=n())
housing <- mutate(housing, sqmt_lot = housing$sq_ft_lot*0.092903)
housing[,c("sq_ft_lot","sqmt_lot")]

housing_ <- select(housing, 'sq_ft_lot')
head(housing_)

housing_2006 <- filter(housing, year_built=='2006')
head(housing_2006)

housing %>% arrange(housing, desc(housing$`Sale Price`))

#Using the purrr package - perform 2 functions on your dataset.  You could use zip_n, keep, discard, compact, etc.
install.packages("purrr")
library(purrr)
#use of keep
keep(housing$`Sale Price`,function(x) x>500000)
#use of discard
discard(housing$`Sale Price`, housing$`Sale Price`<500000)

#Use the cbind and rbind function on your dataset

head(housing)

house_build_2006 <- filter(housing, year_built == '2006')
head(house_build_2006)
nrow(house_build_2006)
house_build_2007 <- filter(housing, year_built == '2007')
head(house_build_2007)
nrow(house_build_2007)

house_build_2006_2007 <- rbind(house_build_2006, house_build_2007)
nrow(house_build_2006_2007)

housing_sd <- select(housing, 'Sale Date')
ncol(housing_sd)
housing_reason = select(housing, sale_reason)
ncol(housing_reason)

sales <- cbind(housing_sd, housing_reason)
head(sales)

#Split a string, then concatenate the results back together
library(stringr)
install.packages('tidyr')
library(tidyr)
housing_stno <- cbind(housing, stringr::str_split_fixed(housing$addr_full, " ", 4))
head(housing_stno)
housing$new_address = paste(housing_stno$`1`, housing_stno$"2", housing_stno$"3", housing_stno$"4")
head(housing$new_address)
