setwd("C:/Users/liuco/Downloads/ubc 2019-2020 wt2/STAT 443 202/project")
VRTX <- read.csv("VRTX.csv")
v = as.ts(VRTX$Adj.Close)

plot.ts(v)
## kinda of a weird plot, but still show an increasing trend, 
## may be some cyclic effect or local variation
acf(v)
## acf decays to  0 slowly, indicates long-term dependency in data points,
## may be caused by the existance of an increasing trend

dv = diff(v)
## differencing int once to remove the trend
plot.ts(dv)
acf(dv)
pacf(dv) 
## white noise after differencing it once. The time series plot seems to be of white noise,
## show no pattern. The acf is only significant at lag 1, indication of white noise.
## The pacf shows no pattern, with 2 significant lags, but it may be caused by the error


test = function(M, data) {
  N = length(data)
  dacf = acf(data, plot = F, lag.max = M)$acf 
  dacf = dacf[-1]
  Q = N* sum(dacf[1:M]^2)
  p = pchisq(Q,M, lower.tail = F)
  p < 0.05
}
## my funciton from assignment 1, for the lack-of-fit test for the residual of the model
## Ho: the process is white noise
## Ha: the process is not white noise
## True -> rejuect null
M = 25

sigma = c(); aic = c()

# fit white noise
fit1 =  arima(dv, c(0,0,0)); fit1
sigma = append(sigma, fit1$sigma2)
aic = append(aic, fit1$aic)
test(M, fit1$residuals)
## False, do not reject null, the residual is white noise, good fit
tsdiag(fit1)
## Support the conclusion. ..... ..... ....

# fit AR(12)
fit2 =  arima(dv, c(12,0,0)); fit2
sigma = append(sigma, fit2$sigma2)
aic = append(aic, fit2$aic)
test(M, fit2$residuals)
## False, do not reject null, the residual is white noise, good fit
tsdiag(fit2)
## Support the conclusion. ..... ..... ....

# fit AR(22)
fit3 =  arima(dv, c(22,0,0)); fit3
sigma = append(sigma, fit3$sigma2)
aic = append(aic, fit3$aic)
test(M, fit3$residuals)
## False, do not reject null, the residual is white noise, good fit
tsdiag(fit3)
## Support the conclusion. ..... ..... ....

aic
## white noice has the smallest aic, so the overal arima model should be ARIMA(0,1,0)


m = 30
train<- 1:(length(dv)-m)
trainv<- dv[train]
testv <- dv[-train]
e = c()

#white noise
model <- arima(trainv, order = c(0, 0, 0))
foremodel = predict(model, m)
error <- sum((testv - foremodel$pred)^2); error
e = append(e, error)

# AR (12)
model2 <- arima(trainv, order = c(12, 0, 0))
foremodel2 = predict(model2, m)
error <- sum((testv - foremodel2$pred)^2); error
e = append(e, error)

# AR(22)
model3 <- arima(trainv, order = c(22, 0, 0))
foremodel3 = predict(model3, m)
error <- sum((testv - foremodel3$pred)^2); error
e = append(e, error)

e
## confirmed our decision before, white noise is the better model

wf = arima(dv, c(0,0,0))
predict(wf, 10, level = 0.95, prediction.interval = T)


