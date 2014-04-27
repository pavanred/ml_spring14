data$clear_weather <- ifelse(data$weathersit ==1, 1, 0)
data$cloudy_weather <- ifelse(data$weathersit ==2, 1, 0)
data$lightprecp_weather <- ifelse(data$weathersit ==3, 1, 0)
data$heavyprecp_weather <- ifelse(data$weathersit ==4, 1, 0)
