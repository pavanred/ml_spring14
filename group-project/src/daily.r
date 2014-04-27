options("width"=300)

byday <- read.csv(file='../data/day.csv', header=T, sep=',')

byday$dteday <- as.Date(byday$dteday)
#byday$season <- factor(byday$season)
byday$season <- as.integer(byday$season)
byday$clear_weather <- ifelse(byday$weathersit ==1, 1, 0)
byday$cloudy_weather <- ifelse(byday$weathersit ==2, 1, 0)
byday$lightprecp_weather <- ifelse(byday$weathersit ==3, 1, 0)
#byday$heavyprecp_weather <- ifelse(byday$weathersit ==4, 1, 0)

cormat <- data.frame(byday$cnt, byday$registered, byday$casual, byday$clear_weather, byday$cloudy_weather, byday$windspeed, byday$lightprecp_weather, byday$holiday, byday$workingday, byday$temp, byday$season)
round(cor(cormat),2)
#View(byday)

cm2 <- data.frame(byday$cnt, byday$temp + byday$windspeed)
round(cor(cm2),2)

counttemp <- lm(byday$cnt ~ byday$temp)
plot(byday$cnt ~ byday$temp)
abline(counttemp)
plot(byday$cnt, resid(counttemp))
abline(resid(counttemp))

model <- lm(cnt ~ season + yr + mnth + holiday + weekday + workingday + weathersit + temp + atemp + hum + windspeed, byday)
plot(model)
summary(model)
