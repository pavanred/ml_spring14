library(MASS)
library(e1071)

#csv to dataframe
#data <- read.csv(file='../data/hour.csv', header=T, sep=',')
#17379 obs of 17 variables

data <- read.csv(file='../data/hour_dummied.csv', header=T, sep=',')
#17379 obs of 66 variables

boxplot(data$count)

bound <- floor(0.70 * nrow(data))         #define % of training and test set
#df <- df[sample(nrow(df)), ]           #sample rows 
data.train <- data[1:bound, ]              #get training set
data.test <- data[(bound+1):nrow(data), ]    #get test set

#Linear Regression
#model <- lm(cnt ~ season + mnth + holiday + weekday + workingday + weathersit + atemp + hum + windspeed, data.train)
summary(model)
#Multiple R-squared:  0.269,  Adjusted R-squared:  0.2685 
#F-statistic: 532.5 on 9 and 13024 DF,  p-value: < 2.2e-16

#Linear Regression - dummies
#model <- lm(count ~ season_spring + season_summer + season_fall + season_winter + jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + hour0 + hour1 + hour2 + hour3 + hour4 + hour5 + hour6 + hour7 + hour8 + hour9 + hour10 + hour11 + hour12 + hour13 + hour14 + hour15 + hour16 + hour17 + hour18 + hour19 + hour20 + hour21 + hour22 + hour23 + holiday + weekday0 + weekday1 + weekday2 + weekday3 + weekday4 + weekday5 + weekday6 + workingday + weather_clear + weather_cloudy + weather_lightprecipitation + weather_heavyprecipitation + atemp + humidity + windspeed, data.train)

#model <- lm(count ~ season_spring + season_summer + season_fall + season_winter + jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + hour0 + hour1 + hour2 + hour3 + hour4 + hour5 + hour6 + hour7 + hour8 + hour9 + hour10 + hour11 + hour12 + hour13 + hour14 + hour15 + hour16 + hour17 + hour18 + hour19 + hour20 + hour21 + hour22 + hour23 + holiday + weekday0 + weekday1 + weekday2 + weekday3 + weekday4 + weekday5 + weekday6 + workingday + weather_clear + weather_cloudy + weather_lightprecipitation + weather_heavyprecipitation + atemp**2 + sqrt(humidity) + sqrt(windspeed), data.train)

# final model
#model <-  lm(count ~ season_spring + season_summer + season_fall + jan + feb + mar + apr + may + jun + jul + aug + nov + hour0 + hour1 +  hour2 + hour3 + hour4 + hour5 + hour7 + hour8 + hour9 + hour10 +   hour11 + hour12 + hour13 + hour14 + hour15 + hour16 + hour17 +   hour18 + hour19 + hour20 + hour21 + hour22 + holiday + weekday0 +   weekday1 + weekday2 + weekday3 + weekday4 + weather_clear +   weather_lightprecipitation + atemp + humidity + windspeed, data.train)


summary(model)
#Multiple R-squared:  0.647,  Adjusted R-squared:  0.6456 
#F-statistic: 475.9 on 50 and 12983 DF,  p-value: < 2.2e-16

#feature selection - stepwise regression
step <- stepAIC(model, direction="both")
step$anova # display results 
#FinalModel
#count ~ season_spring + season_summer + season_fall + jan + feb + mar + apr + may + jun + jul + aug + nov + hour0 + hour1 +  hour2 + hour3 + hour4 + hour5 + hour7 + hour8 + hour9 + hour10 +   hour11 + hour12 + hour13 + hour14 + hour15 + hour16 + hour17 +   hour18 + hour19 + hour20 + hour21 + hour22 + holiday + weekday0 +   weekday1 + weekday2 + weekday3 + weekday4 + weather_clear +   weather_lightprecipitation + atemp + humidity + windspeed

#Linear Regression - stepwise
model <- lm(count ~ season_spring + season_summer + season_fall + season_winter + jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + hour0 + hour1 + hour2 + hour3 + hour4 + hour5 + hour6 + hour7 + hour8 + hour9 + hour10 + hour11 + hour12 + hour13 + hour14 + hour15 + hour16 + hour17 + hour18 + hour19 + hour20 + hour21 + hour22 + hour23 + holiday + weekday0 + weekday1 + weekday2 + weekday3 + weekday4 + weekday5 + weekday6 + workingday + weather_clear + weather_cloudy + weather_lightprecipitation + weather_heavyprecipitation + atemp + humidity + windspeed, data.train)
summary(model)
#Multiple R-squared:  0.647,  Adjusted R-squared:  0.6456 
#F-statistic: 475.9 on 50 and 12983 DF,  p-value: < 2.2e-16

error <-  test - data.test$count
sqrt(mean(error**2)) 


#Influence Matrix
influence(model)
#Residuals
resid(model)
#plots
plot(model)

#test set
test = predict(model, data.test)
plot(data.test$count~test)
summary(test)

#SVM
svm01 <- svm(count ~ season_spring + season_summer + season_fall + season_winter + jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + hour0 + hour1 + hour2 + hour3 + hour4 + hour5 + hour6 + hour7 + hour8 + hour9 + hour10 + hour11 + hour12 + hour13 + hour14 + hour15 + hour16 + hour17 + hour18 + hour19 + hour20 + hour21 + hour22 + hour23 + holiday + weekday0 + weekday1 + weekday2 + weekday3 + weekday4 + weekday5 + weekday6 + workingday + weather_clear + weather_cloudy + weather_lightprecipitation + weather_heavyprecipitation + atemp + humidity + windspeed, data.train)
summary(svm01)
test = predict(svm01,data.test)

#cost 1000 gamma 0.0001 - radial kernel
svm.model <- svm(count ~ season_spring + season_summer + season_fall + season_winter + jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + hour0 + hour1 + hour2 + hour3 + hour4 + hour5 + hour6 + hour7 + hour8 + hour9 + hour10 + hour11 + hour12 + hour13 + hour14 + hour15 + hour16 + hour17 + hour18 + hour19 + hour20 + hour21 + hour22 + hour23 + holiday + weekday0 + weekday1 + weekday2 + weekday3 + weekday4 + weekday5 + weekday6 + workingday + weather_clear + weather_cloudy + weather_lightprecipitation + weather_heavyprecipitation + atemp + humidity + windspeed, data = data.train, cost = 1000, gamma = 0.0001)
svm.pred <- predict(svm.model, data.test)
crossprod(svm.pred - data.test$count) / nrow(data.test)
error <- svm.pred - data.test$count
sqrt(mean(error**2)) 
#RMSE 139.6982

summary(svm.model)  
table(svm.pred,data.test$count)
plot(data.test$count~svm.pred)
tmp <- lm(data.test$count~svm.pred)
abline(tmp,col='red')

#non linear kernels - cubic
svm.model <- svm(count ~ season_spring + season_summer + season_fall + season_winter + jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + hour0 + hour1 + hour2 + hour3 + hour4 + hour5 + hour6 + hour7 + hour8 + hour9 + hour10 + hour11 + hour12 + hour13 + hour14 + hour15 + hour16 + hour17 + hour18 + hour19 + hour20 + hour21 + hour22 + hour23 + holiday + weekday0 + weekday1 + weekday2 + weekday3 + weekday4 + weekday5 + weekday6 + workingday + weather_clear + weather_cloudy + weather_lightprecipitation + weather_heavyprecipitation + atemp + humidity + windspeed, data = data.train, cost = 10, gamma = 0.01, kernel='polynomial', cross=3)
svm.pred <- predict(svm.model, data.test)
crossprod(svm.pred - data.test$count) / nrow(data.test)
error <- svm.pred - data.test$count
sqrt(mean(error**2)) 
#RMSE 128.6375
summary(svm.model)


#non linear kernels - cubic
svm6.model <- svm(count ~ season_spring + season_summer + season_fall + jan + feb + mar + apr + may + jun + jul + aug + nov + hour0 + hour1 +  hour2 + hour3 + hour4 + hour5 + hour7 + hour8 + hour9 + hour10 +   hour11 + hour12 + hour13 + hour14 + hour15 + hour16 + hour17 +   hour18 + hour19 + hour20 + hour21 + hour22 + holiday + weekday0 +   weekday1 + weekday2 + weekday3 + weekday4 + weather_clear +   weather_lightprecipitation + atemp + humidity + windspeed, data = data.train, cost = 10, gamma = 0.01, kernel='polynomial', cross=3)
svm6.pred <- predict(svm6.model, data.test)
error <- svm6.pred - data.test$count
sqrt(mean(error**2)) 
#RMSE 128.6375
summary(svm6.model)


#non linear kernels - quad
svm2.model <- svm(count ~ season_spring + season_summer + season_fall + season_winter + jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + hour0 + hour1 + hour2 + hour3 + hour4 + hour5 + hour6 + hour7 + hour8 + hour9 + hour10 + hour11 + hour12 + hour13 + hour14 + hour15 + hour16 + hour17 + hour18 + hour19 + hour20 + hour21 + hour22 + hour23 + holiday + weekday0 + weekday1 + weekday2 + weekday3 + weekday4 + weekday5 + weekday6 + workingday + weather_clear + weather_cloudy + weather_lightprecipitation + weather_heavyprecipitation + atemp + humidity + windspeed, data = data.train, cost = 10, gamma = 0.01, kernel='polynomial', cross=3, degree=5)
svm2.pred <- predict(svm2.model, data.test)
error <- svm2.pred - data.test$count
sqrt(mean(error**2)) 
#RMSE 133.0355
summary(svm2.model)


#non linear kernels - quad
svm3.model <- svm(count ~ season_spring + season_summer + season_fall + season_winter + jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + hour0 + hour1 + hour2 + hour3 + hour4 + hour5 + hour6 + hour7 + hour8 + hour9 + hour10 + hour11 + hour12 + hour13 + hour14 + hour15 + hour16 + hour17 + hour18 + hour19 + hour20 + hour21 + hour22 + hour23 + holiday + weekday0 + weekday1 + weekday2 + weekday3 + weekday4 + weekday5 + weekday6 + workingday + weather_clear + weather_cloudy + weather_lightprecipitation + weather_heavyprecipitation + atemp + humidity + windspeed, data = data.train, cost = 10, gamma = 0.01, kernel='polynomial', cross=3, degree=4)
svm3.pred <- predict(svm3.model, data.test)
crossprod(svm3.pred - data.test$count) / nrow(data.test)
error <- svm3.pred - data.test$count
sqrt(mean(error**2)) 
#RMSE 129.9472
summary(svm3.model)


#3-fold cross-validation on training data  
#Total Mean Squared Error: 4227.01 
#Squared Correlation Coefficient: 0.8365405 
#Mean Squared Errors:
#  4163.479 4286.923 4230.613 

#Test set
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-57.46   51.71  149.90  170.50  247.70  658.70 

#non linear kernels - sigmoid
svm4.model <- svm(count ~ season_spring + season_summer + season_fall + jan + feb + mar + apr + may + jun + jul + aug + nov + hour0 + hour1 +  hour2 + hour3 + hour4 + hour5 + hour7 + hour8 + hour9 + hour10 +   hour11 + hour12 + hour13 + hour14 + hour15 + hour16 + hour17 +   hour18 + hour19 + hour20 + hour21 + hour22 + holiday + weekday0 +   weekday1 + weekday2 + weekday3 + weekday4 + weather_clear +   weather_lightprecipitation + atemp + humidity + windspeed, data = data.train, cost = 10, gamma = 0.01, kernel='sigmoid')
svm4.pred <- predict(svm4.model, data.test)
crossprod(svm4.pred - data.test$count) / nrow(data.test)

error <- svm4.pred - data.test$count
sqrt(mean(error**2)) 
#RMSE 128.6375
summary(svm4.model)


svm5.model <- svm(count ~ season_spring + season_summer + season_fall + season_winter + jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + hour0 + hour1 + hour2 + hour3 + hour4 + hour5 + hour6 + hour7 + hour8 + hour9 + hour10 + hour11 + hour12 + hour13 + hour14 + hour15 + hour16 + hour17 + hour18 + hour19 + hour20 + hour21 + hour22 + hour23 + holiday + weekday0 + weekday1 + weekday2 + weekday3 + weekday4 + weekday5 + weekday6 + workingday + weather_clear + weather_cloudy + weather_lightprecipitation + weather_heavyprecipitation + atemp + humidity + windspeed, data = data.train, cost = 10, gamma = 0.01, kernel='sigmoid')
svm5.pred <- predict(svm5.model, data.test)
error <- svm5.pred - data.test$count
sqrt(mean(error**2)) 
#RMSE 128.6375
summary(svm5.model)


#poission regression
m1 <- glm(count ~ season_spring + season_summer + season_fall + season_winter + jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + hour0 + hour1 + hour2 + hour3 + hour4 + hour5 + hour6 + hour7 + hour8 + hour9 + hour10 + hour11 + hour12 + hour13 + hour14 + hour15 + hour16 + hour17 + hour18 + hour19 + hour20 + hour21 + hour22 + hour23 + holiday + weekday0 + weekday1 + weekday2 + weekday3 + weekday4 + weekday5 + weekday6 + workingday + weather_clear + weather_cloudy + weather_lightprecipitation + weather_heavyprecipitation + (atemp^2) + humidity + windspeed, family = "poisson", data = data.train)
summary(m1)
test = predict.glm(m1,data.test)
plot(data.test$count~test)
summary(test)
error <- test - data.test$count
sqrt(mean(error**2)) 
#RMSE 332.3749
