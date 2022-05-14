# Assignment: ASSIGNMENT 6
# Name: Lastname, Firstname
# Date: 2022-05-02

## Set the working directory to the root of your DSC 520 directory

setwd("C:\\Users\\atanu\\Documents\\BellevueUniversity_MSDS\\DSC520\\Repository\\dsc520")

## Load the `data/r4ds/heights.csv` to
heights_df <- read.csv("data\\r4ds\\heights.csv")

## Load the ggplot2 library
library(ggplot2)

## Fit a linear model using the `age` variable as the predictor and `earn` as the outcome
head(heights_df)
age_lm <-  lm(earn~age, data=heights_df)

## View the summary of your model using `summary()`
summary(heights_df)

## Creating predictions using `predict()`

age_predict_df <- data.frame(earn = predict(age_lm, data.frame(age=heights_df$age)), age=heights_df$age)
head(age_predict_df)


## Plot the predictions against the original data
ggplot(data = heights_df, aes(y = earn, x = age)) +
  geom_point(color='blue') +
  geom_line(color='red',data = age_predict_df, aes(y=earn, x=age))

mean_earn <- mean(heights_df$earn)
## Corrected Sum of Squares Total
sst <- sum((mean_earn - heights_df$earn)^2)
## Corrected Sum of Squares for Model
ssm <- sum((mean_earn - age_predict_df$earn)^2)
## Residuals
residuals <- heights_df$earn - age_predict_df$earn
residuals
## Sum of Squares for Error
sse <- sum(residuals^2)
## R Squared R^2 = SSM\SST
summary(age_lm)
r_squared <- summary(age_lm)$r.squared
r_squared
## Number of observations
n <- nrow(heights_df)
## Number of regression parameters
p <- 2
## Corrected Degrees of Freedom for Model (p-1)
dfm <- p-1
## Degrees of Freedom for Error (n-p)
dfe <- n-p
## Corrected Degrees of Freedom Total:   DFT = n - 1
dft <- n-1

## Mean of Squares for Model:   MSM = SSM / DFM
msm <- ssm/dfm
msm
## Mean of Squares for Error:   MSE = SSE / DFE
mse <- sse / dfe
mse
## Mean of Squares Total:   MST = SST / DFT
mst <- sst / dft
mst
## F Statistic F = MSM/MSE
f_score <- msm/mse
f_score
## Adjusted R Squared R2 = 1 - (1 - R2)(n - 1) / (n - p)
adjusted_r_squared <- 1 - (1 - r_squared)*(n - 1) / (n - p)
adjusted_r_squared
## Calculate the p-value from the F distribution
p_value <- pf(f_score, dfm, dft, lower.tail=F)

