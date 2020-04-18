################# COST Analysis #################  

# setup

setwd("C:/Users/liuco/Downloads/ubc 2019-2020 wt2/STAT 443 202/project")
cost = read.csv("COST.csv")
c = as.ts(cost$Adj.Close)
plot(c)
# Here I have plotted the time series of the Costco (coST) adjusted closing prices from
# 03/12/2018 to 29/11/2019. We can see that the plot has a visible
# increasing trend from the start, and that the plot is most likely not stationary.


acf1 = acf(c)
plot(acf1)
# From the ACF plot of the above time series object, we can see that there is a 
# slow, but gradual decay. This indicates the the time series process is most likely
# not stationary due to a dependence or correlation between observations from the original
# data. We will need to transform the data to achieve stationarity for model fitting.

diff.c = diff(c)
plot(diff.c)
# Differencing the original time series object once appears to remove the increasing
# trend that we first observed. This time series plot, now, is more likely stationary.

acf2 = acf(diff.c)
plot(acf2)
# The ACF plot of the time series object, differenced once, appears to be that of a
# stationary process. The only significant ACF value, other than at lag 0, can be found at lag 16.
# However, this significant ACF may be purely due to random chance. In our case, we will 
# take this to be a significant lag. In doing so, we can classify this ACF plot to resemble 
# an ACF plot from a MA(16) or AR(16) process.

pacf2 = pacf(diff.c)
plot(pacf2)
# The PACF plot of the time series object, differenced once, appears to be that of a white noise process.
# There are no significant PACF values to be found.

m = 25
# I have chosen an arbitrary value of M = 25 to conduct a portmanteau lack-of-fit test for the residuals
# of each of the following models: white noise, AR(16), and MA(16). If the residuals are found to be that of a 
# white noise process, then this suggests that the models are adequate fits to the data. In the end, the tsdiag() function
# in R will be applied to the fitted models to confirm if the models are indeed suitable fits.

sigma = c()
aic = c()
# Here, I have constructed empty arrays for the comparison of sigmas and AIC values from 
# modelling the original data to white noise, AR(16), and MA(16) processes.
# Models with lower values of sigma and AIC will be considered to be better fits to the original data.

# white noise model
model.1 = arima(diff.c, c(0,0,0))
sigma = append(sigma, model.1$sigma2)
aic = append(aic, model.1$aic)

n1 = length(model.1$residuals)
dacf1 = acf(model.1$residuals, plot = F, lag.max = m)$acf
dacf1 = dacf1[-1]
q1 = n1 * sum(dacf1[1:m]^2)
pval.1 = pchisq(q1, m, lower.tail = F)

# AR(16) model
model.4 = arima(diff.c, c(16,0,0), method = "ML")
sigma = append(sigma, model.4$sigma2)
aic = append(aic, model.4$aic)

n4 = length(model.4$residuals)
dacf4 = acf(model.4$residuals, plot = F, lag.max = m)$acf
dacf4 = dacf4[-1]
q4 = n4 * sum(dacf4[1:m]^2)
pval.4 = pchisq(q4, m, lower.tail = F)

# MA(16) model
model.5 = arima(diff.c, c(0,0,16), method = "ML")
sigma = append(sigma, model.5$sigma2)
aic = append(aic, model.5$aic)

n5 = length(model.5$residuals)
dacf5 = acf(model.5$residuals, plot = F, lag.max = m)$acf
dacf5 = dacf5[-1]
q5 = n5 * sum(dacf5[1:m]^2)
pval.5 = pchisq(q5, m, lower.tail = F)

cat("pval.1: ", pval.1, "pval.4: ", pval.4, "pval.5: ", pval.5)
# From the above portmanteau lack-of-fit tests, all residuals are observed to be residuals from a white noise process due
# to the fact that none of the p-values are found to be significant. Therefore, the null hypothesis that the residuals
# are from a white noise process is not rejected for any of the three models. We can assume that all three models are
# suitable fits for the time series data.


tsdiag(model.1)
tsdiag(model.4)
tsdiag(model.5)
# By applying the tsdiag() functions to the above models, we observe that the diagnostics of all three models support
# our initial result obtained from the portmanteau lack-of-fit testing due to the following: all p-values for the Ljung-Box
# statistics are above the significance line, all ACF plots are found to have only one significant ACF value at lag 0, and 
# all standardized residual plots consist of evenly spread residuals about the zero line. The diagnostics from the tsdiag()
# function suggest that all models are a suitable fit for the time series.


# training and testing set
x = diff.c
train = 1:(length(x) - m)
trainx = x[train]
testx = x[-train]
# fit models
model.1 = arima(trainx, order = c(0,0,0))
model.4 = arima(trainx, order = c(16,0,0))
model.5 = arima(trainx, order = c(0,0,16))
# fit models to train set, use models to predict test set
foremodel.1 = predict(model.1, m)
foremodel.4 = predict(model.4, m)
foremodel.5 = predict(model.5, m)
#computes squared errors over test set
error.1 = sum((testx - foremodel.1$pred)^2)
error.4 = sum((testx - foremodel.4$pred)^2)
error.5 = sum((testx - foremodel.5$pred)^2)
# The model selection method we have chosen to use is the train/test method in which we split the data into a training set and a test set.
# Here, we are using a training set to assess the fit and compare the fit between models. Then, we will
# determine the performance of the models by using the training sets of the corresponding models
# to fit values in the testing set. The ranking criterion we have chosen to use in assessing a model's performance
# on the test set is the mean squared error (MSE). The model with the lowest MSE will be considered to be the best fit.

cat("error 1: ", error.1, "error: 4", error.4, "error: 5", error.5)
cat("aic: ", aic, "\n")
cat("sigma: ", sigma, "\n")
# After compiling the MSE's as well as the AIC and sigma values for the white noise, AR(16), and MA(16) models, we observe that
# the answer is not exactly clear. For the MSE, the AR(16) model is found to have the lowest value of approximately 92.836. In terms of AIC, the white noise
# has the lowest value of approximately 1265.894. In terms of sigma value, the MA(16) is found to have the lowest score of 8.498. As our
# time series project is moreso concerned with prediction via a train/test method style, we will choose to go forward with modelling
# the original time series data to a AR(16) process (lowest MSE value).


c19COST = read.csv("c19COST.csv")
c19 = ts(c19COST$Adj.Close)
n.c19 = length(c19)
wf = arima(c, c(16,1,0))
p = predict(wf, n.c19)
tsp.c19 = p$pred
tsp.c19 = ts(tsp.c19); tsp.c19
# As stated above, we have chosen to move forward with modelling the original time series data to a AR(16) process. Now,
# we are putting the model to use to predict Costco (COST) adjusted closing prices from 02/12/2019 to 27/03/2020 (COVID-19 prices).
# These are the predicted values from our fitted model.

library(tseries)
seqplot.ts(c19, tsp.c19, "red", "black")
# As expected the predictions obtained from our fitted model are quite similar to the actual Costco adjusted closing prices from 02/12/2019 onwards in the 
# very beginning of the plot, as COVID-19 had not fully emerged until the end of December 2019. At approximately 30 days into the
# prediction interval, we can see the actual Costco adjusted closing prices start to fluctuate. At first, the adjusted closing 
# prices rise to an eventual peak, take several hard drops and rises afterwards, and the last adjusted closing price is found to be
# below our the average of our predicted values (299.6842) by a significant amount (approximately -15).


