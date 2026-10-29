install.packages("forecast")
library(forecast)
y <- co2

class(y)
start(y)
end(y)
frequency(y)
length(y)
summary(y)
sum(is.na(y))


plot(y,main = "MOnthly atmospheric CO2",
     xlab = "Year",
    ylab = "CO2(ppm)",
    col= "blue")

monthplot(y,
          main="monthly seasonal pattern",
          xlab="mOnth",
          ylab="CO2 ppm")

dec <- decompose(y,type = "additive")
plot(dec)

seasonal_effect <- setNames(dec$figure, month.abb)
round(seasonal_effect,2)

names(which.max(seasonal_effect))
names(which.min(seasonal_effect))




h <- 12
n <- length(y)

train <- window(y,end=time(y)[n-h])
test <- window(y, start=time(y)[n-h+1])
length(train)
length(test)
start(test)
end(test)

plot(y,
     main="Training and Testing periods",
     xlab="Year",
     ylab="CO2 ppm")

lines(test,col='red',lwd=2)

legend(
  "topleft",
  legend = c("Training","Testing"),
  col = c("black","red"),
  lty = 1,
  bty = "n"
)


#

fc_Sc <- snaive(train, h=h)

fit_hw <- HoltWinters(train,seasonal = "additive")
fc_hw <- forecast(fit_hw, h=h)

fit_arima <- auto.arima(train,seasonal = TRUE,
                        approximation = FALSE)
fc_arima <- forecast(fit_arima, h=h)

fit_arima
?snaive
?HoltWinters

plot(
  fc_arima,
  main = "arima forecast vs test obs",
  xlab = "year",
  ylab = "co2 ppm"
)
lines(test,col="red",lwd=2)

legend("topleft",
       legend = c("Forecast", "Actual test values"),
       col = c("blue", "red"),
       lty = 1,
       bty = "n")

accuracy(fc_Sc,test)

metrics <- c("MAE","RMSE","MAPE")

results <- rbind(
  Seasonal_Naive = accuracy(fc_Sc,test)["Test set",metrics],
  Holt_Winters = accuracy(fc_hw,test)["Test set",metrics],
  ARIMA = accuracy(fc_arima,test)["Test set", metrics]
)

round(results,3)


results[order(results[,"RMSE"]), ]

best_name <- rownames(results)[which.min(results[,"RMSE"])]
best_name
results[best_name,]


if(best_name == "Seasonal_Naive"){
  future <- snaive(y, h=12, level = c(80,95))

}else if (best_name == "Holt_Winters"){
  final_model <- HoltWinters(y, seasonal = "additive")
  future <- forecast(final_model,h = 12, level =c(80,95))
}else{
  final_model <- auto.arima(y,
                            seasonal = TRUE,
                            approximation = FALSE)
  future <- forecast(final_model, h=12, level=c(80,95))
}

print(future)

plot(future,
     include = 60,
     main = paste("12-month forecast:", best_name),
     xlab = "year",
     ylab = "co2 ppm")
