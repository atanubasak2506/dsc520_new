# Assignment: ASSIGNMENT 7
# Name: Basak, Atanu
# Date: 2022-05-03

## Set the working directory to the root of your DSC 520 directory
setwd("C:\\Users\\atanu\\Documents\\BellevueUniversity_MSDS\\DSC520\\Repository\\dsc520")


## Load the `data/r4ds/heights.csv` to
heights_df <- read.csv("data\\r4ds\\heights.csv")
head(heights_df)
# Fit a linear model
earn_lm <-  lm(earn ~ height + sex + ed + age + race, data=heights_df)

# View the summary of your model
summary(earn_lm)
age_predict_df <- heights_df[,c('age','ed','race','height','sex')]

head(newdata)
predicted_df <- data.frame(
  earn = predict(earn_lm, newdata),
  ed=newdata$ed, race=newdata$race, height=newdata$height,
  age=newdata$age, sex=newdata$sex
  )
head(predicted_df)

## Compute deviation (i.e. residuals)
mean_earn <- mean(predicted_df$earn)
mean_earn
## Corrected Sum of Squares Total
sse <- sum((fitted(earn_lm) - heights_df$earn)^2)
sse
ssr <- sum((fitted(earn_lm) - mean(heights_df$earn))^2)
ssr
sst <- ssr + sse
sst
## Corrected Sum of Squares for Model
ssm <- sum((mean_earn - predicted_df$earn)^2)
ssm
## Residuals
residuals <- earn_lm$residuals
residuals
## Sum of Squares for Error
sse <- sum((fitted(earn_lm) - heights_df$earn)^2)
sse
## R Squared
r_squared <- summary(earn_lm)$r.square
r_squared
## Number of observations
n <- nrow(heights_df)
n
## Number of regression paramaters
p <- 8
## Corrected Degrees of Freedom for Model
dfm <- p-1
## Degrees of Freedom for Error
dfe <- n-p
## Corrected Degrees of Freedom Total:   DFT = n - 1
dft <- n-1

## Mean of Squares for Model:   MSM = SSM / DFM
msm <-  ssm/dfm
## Mean of Squares for Error:   MSE = SSE / DFE
mse <- sse / dfe
## Mean of Squares Total:   MST = SST / DFT
mst <- sst / dft
## F Statistic
f_score <-  msm/mse
f_score
## Adjusted R Squared R2 = 1 - (1 - R2)(n - 1) / (n - p)
adjusted_r_squared <- 1 - (1 - r_squared)*(n - 1) / (n - p)
adjusted_r_squared
