install.packages('tidyverse')
install.packages('readxl')

library(tidyverse)
library(readxl)

#Load Dataframe
df <- read_excel(file.choose(),sheet = 1)

#See dataset
glimpse(df)

#Give unique column
unique(df$Country)

#Create Linear Regression
reg.fit <- lm(Rating~Price + Alcohol + Residual_Sugar + Sulphates + pH + Country, df)
summary(reg.fit)

#Plot into one, instead of 4
par(mfrow=c(2,2))

#Plot it
plot(reg.fit)

#PImport new data
wine.data <- read_excel(file.choose(),sheet = 2)
glimpse(wine.data)

#Predict
predicted.rating <- predict(reg.fit, wine.data)
glimpse(predicted.rating)

#Actual rating from sheet
actual.rating <- read_excel(file.choose(),sheet = 3)
glimpse(actual.rating)

#Mean Absolute Percentage Error (MAPE)
percent.errors <-abs((actual.rating$Rating-predicted.rating)/predicted.rating)*100
mape <- mean(percent.errors)
mape

#Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((actual.rating$Rating-predicted.rating)^2))
rmse

