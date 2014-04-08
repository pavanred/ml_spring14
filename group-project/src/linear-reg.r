data <- read.csv(file='/Users/landes/view/uic/2014/pavan/group-project/data/day.csv', header=T, sep=',')
#data
model <- lm(cnt ~ season + yr + mnth + holiday + weekday + workingday + weathersit + temp + atemp + hum + windspeed, data)
#summary(model)

#coefficients(model) # model coefficients
#confint(model, level=0.95) # CIs for model parameters
#fitted(model) # predicted values
#residuals(model) # residuals
#anova(model) # anova table
#vcov(model) # covariance matrix for model parameters
#influence(model) # regression diagnostics


plot(data$cnt, resid(model), ylab="Residuals", xlab="Bike Rentals", main="Residuals vs Bike Rental Counts")
abline(0,0)

#Diagnostics - heteroscedasticity, normality, and influential observerations.
#layout(matrix(c(1,2,3,4),2,2))
#plot(model)

#data$res <- resid(model)
#boxplot(data$cnt~data$res,data=data, main="Residuals vs Fitted
#Boxplot", xlab="Residuals", ylab="Bike Rental Counts")

model_weather <- lm(cnt ~ weathersit + temp , data)
summary(model_weather)
plot(data$cnt, resid(model_weather), ylab="Residuals", xlab="Bike Rentals", main="Residuals vs Bike Rental (weather + temp)")
abline(0,0)

plot(data$cnt~as.Date(data$dteday),ylab="counts",xlab="Date",type='l', col='blue')
lines(predict(model)~as.Date(data$dteday),col='red')
lines(predict(model_weather)~as.Date(data$dteday),col='green')
