################# LVMH Analysis #################  

# setup

setwd("C:/Users/liuco/Downloads/ubc 2019-2020 wt2/STAT 443 202/project")
lvmh = read.csv("LVMH.csv")
l = as.ts(lvmh$Adj.Close)
plot(l)

# time series plot has an increasing trend with drop at the end. (mean depends on time)

acf1 = acf(l)
plot(acf1)

# afc decays to 0 slowly, implies long-term dependency between data pts (not stationary)
# this may be caused by the increasing trend found in time series plot

diff.l = diff(l)
plot(diff.l)

# time series plot once differenced, appears to remove trend (mean no longer depends on time)
# plot looks like white noise

acf2 = acf(diff.l)
plot(acf2)
pacf2 = pacf(diff.l)
plot(pacf2)


# acf plot of time series plot differenced once resembles acf plot of white noise
# ^^^ only significant at lag 0
# pacf plot has no significant lags (no pattern)

m = 25

sigma = c()
aic = c()

# white noise model ARIMA(0,0,0)
model.1 = arima(diff.l, c(0, 0, 0)); model.1
sigma = append(sigma, model.1$sigma2)
aic = append(aic, model.1$aic)

n1 = length(model.1$residuals)
dacf1 = acf(model.1$residuals, plot = F, lag.max = m)$acf
dacf1 = dacf1[-1]
q1 = n1 * sum(dacf1[1:m]^2)
pval.1 = pchisq(q1, m, lower.tail = F)
cat("pval.1: ", pval.1)
tsdiag(model.1)

# portmanteau lack-of-fit test has pval.1 that fails to reject H0
# H0: realization from white noise
# Ha: not realization from white noise
# pval.1 suggests this is realization from white noise
# good fit (white noise)


# AR(12) model ARIMA(12,0,0)
model.2 = arima(diff.l, c(12,0,0)); model.2
sigma = append(sigma, model.2$sigma2)
aic = append(aic, model.2$aic)

n2 = length(model.2$residuals)
dacf2 = acf(model.2$residuals, plot = F, lag.max = m)$acf
dacf2 = dacf2[-1]
q2 = n2 * sum(dacf2[1:m]^2)
pval.2 = pchisq(q2, m, lower.tail = F)
cat("pval.2: ", pval.2)
tsdiag(model.2)

# do not reject null, white noise is good fit

# AR(22) model ARIMA(22, 0, 0)
model.3 = arima(diff.l, c(22, 0, 0)); model.3
sigma = append(sigma, model.3$sigma2)
aic = append(aic, model.3$aic)

n3 = length(model.3$residuals)
dacf3 = acf(model.3$residuals, plot = F, lag.max = m)$acf
dacf3 = dacf3[-1]
q3 = n3 * sum(dacf3[1:m]^2)
pval.3 = pchisq(q3, m, lower.tail = F)
cat("pval.3: ", pval.3)
tsdiag(model.3)

# do not reject null, white noise is good fit

# white noise has best AIC, should pick ARIMA(0,1,0) as best fit

# setup
x = diff.l
train = 1:(length(x) - m)
trainx = x[train]
testx = x[-train]

# fit models
model.1 = arima(trainx, order = c(0,0,0))
model.2 = arima(trainx, order = c(12,0,0))
model.3 = arima(trainx, order = c(22,0,0))

# fit models to train set, use models to predict test set
foremodel.1 = predict(model.1, m)
foremodel.2 = predict(model.2, m)
foremodel.3 = predict(model.3, m)

#computes squared errors over test set
error.1 = sum((testx - foremodel.1$pred)^2)
error.2 = sum((testx - foremodel.2$pred)^2)
error.3 = sum((testx - foremodel.3$pred)^2)

cat("error 1: ", error.1, "error 2: ", error.2, "error: 3", error.3)

# white noise is best model

predict(arima(diff.l, c(0,0,0)), 10, level = 0.95, prediction.interval = T)





