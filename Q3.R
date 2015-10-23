q3 = read.csv(file.choose(), header=TRUE)
q3d=q3[,2]
plot.ts(q3d,type = "l", xlab = "Time", ylab = "Time Series", main = "Q3train Dataset" )
acf(q3d, lag.max = 100, type = "correlation", plot = T,  main = "ACF of Nonstationary Data")

#There is obvious linear Trend and Seasonality
q3d1=diff(q3d)
plot.ts(q3d1,type = "l", xlab = "Time", ylab = "Time Series", main = "Q3train Differenced Data")
plot(q3d1[1: (length(q3d1) - 52)], q3d1[53: length(q3d1)])
acf(q3d1,lag.max=200,type = "correlation", plot = T, main = "ACF of Differenced Data")
pacf(q3d1, lag.max=200,main = "PACF of Differenced Data")

#ACF shows a slow decay -> difference needed with lag=52
q3d2=diff(q3d1,52)

#ACF of differenced and seasonal differenced data
par(mfrow=c(1,1))
plot.ts(q3d2,type = "l", xlab = "Time", ylab = "Time Series", main = "Q3train Differenced and Seasonal Differenced Dataset" )
par(mfrow=c(2,1))
acf(q3d2,lag.max=200,type = "correlation", plot = T, main = "ACF of Differenced and Seasaon Differenced Data")
pacf(q3d2, lag.max=200,main = "PACF of Differenced and Seasaon Differenced Data")

#Fitting Different Model to ACF and PACF
q3fit1 = arima(q3d, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 52))
q3fit2 = arima(q3d, order = c(0, 1, 1), seasonal = list(order = c(1, 1, 1), period = 52))
q3fit3 = arima(q3d, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 52))
q3fit3a = arima(q3d2, order = c(1, 0, 1), seasonal = list(order = c(0, 0, 1), period = 52))
q3fit4 = arima(q3d, order = c(2, 1, 1), seasonal = list(order = c(0, 1, 1), period = 52))
q3fit5 = arima(q3d, order = c(1, 1, 2), seasonal = list(order = c(0, 1, 1), period = 52))

#Cross Validation of different models
ny = floor(length(q3d)/52)
py = 5
pred.err.mat = matrix(NA,(ny - py) ,52)
err.year.m1 = rep(-99, (ny-py)) 
for(k in py:(ny-1))
{
  train.dt = q3d[1:(k*52)]
  test.dt = q3d[((k*52)+1):((k*52) + 52)]
  fm1 = arima(train.dt, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 52))
  fcast.m1 = predict(fm1, n.ahead = 52)
  pred.err.mat[(k-py+1),] = (test.dt - fcast.m1$pred)
  err.year.m1[(k-py+1)] = sum(pred.err.mat[(k-py+1),]^2)
}

cv1=mean(err.year.m1)

  ny = floor(length(q3d)/52)
py = 5
pred.err.mat = matrix(NA,(ny - py) ,52)
err.year.m1 = rep(-99, (ny-py))
for(k in py:(ny-1))
{
  train.dt = q3d[1:(k*52)]
  test.dt = q3d[((k*52)+1):((k*52) + 52)]
  fm1 = arima(train.dt, order = c(0, 1, 1), seasonal = list(order = c(1, 1, 1), period = 52))
  fcast.m1 = predict(fm1, n.ahead = 52)
  pred.err.mat[(k-py+1),] = (test.dt - fcast.m1$pred)
  err.year.m1[(k-py+1)] = sum(pred.err.mat[(k-py+1),]^2)
}

cv2=mean(err.year.m1)

  ny = floor(length(q3d)/52)
py = 5
pred.err.mat = matrix(NA,(ny - py) ,52)
err.year.m1 = rep(-99, (ny-py)) 
for(k in py:(ny-1))
{
  train.dt = q3d[1:(k*52)]
  test.dt = q3d[((k*52)+1):((k*52) + 52)]
  fm1 = arima(train.dt, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 52))
  fcast.m1 = predict(fm1, n.ahead = 52)
  pred.err.mat[(k-py+1),] = (test.dt - fcast.m1$pred)
  err.year.m1[(k-py+1)] = sum(pred.err.mat[(k-py+1),]^2)
}

cv3=mean(err.year.m1)

  ny = floor(length(q3d)/52)
py = 5
pred.err.mat = matrix(NA,(ny - py) ,52)
err.year.m1 = rep(-99, (ny-py)) 
for(k in py:(ny-1))
{
  train.dt = q3d[1:(k*52)]
  test.dt = q3d[((k*52)+1):((k*52) + 52)]
  fm1 = arima(train.dt, order = c(2, 1, 1), seasonal = list(order = c(0, 1, 1), period = 52))
  fcast.m1 = predict(fm1, n.ahead = 52)
  pred.err.mat[(k-py+1),] = (test.dt - fcast.m1$pred)
  err.year.m1[(k-py+1)] = sum(pred.err.mat[(k-py+1),]^2)
}

cv4=mean(err.year.m1)


  ny = floor(length(q3d)/52)
py = 5
pred.err.mat = matrix(NA,(ny - py) ,52)
err.year.m1 = rep(-99, (ny-py)) 
for(k in py:(ny-1))
{
  train.dt = q3d[1:(k*52)]
  test.dt = q3d[((k*52)+1):((k*52) + 52)]
  fm1 = arima(train.dt, order = c(1, 1, 2), seasonal = list(order = c(0, 1, 1), period = 52))
  fcast.m1 = predict(fm1, n.ahead = 52)
  pred.err.mat[(k-py+1),] = (test.dt - fcast.m1$pred)
  err.year.m1[(k-py+1)] = sum(pred.err.mat[(k-py+1),]^2)
}

cv5=mean(err.year.m1)

#AIC, Max.Log Likelihood, Cross Validation
AIC=c(q3fit1$aic,q3fit2$aic,q3fit3$aic,q3fit4$aic,q3fit5$aic  )
LogLikelihood=c(q3fit1$log,q3fit2$log,q3fit3$log,q3fit4$log,q3fit5$log)
CV=c(cv1,cv2,cv3,cv4,cv5 )
Model=c("ARIMA(0,1,1)x(0,1,1)_52", "ARIMA(0,1,1)x(1,1,1)_52", "ARIMA(1,1,1)x(0,1,1)_52", "ARIMA(2,1,1)x(0,1,1)_52","ARIMA(1,1,2)x(0,1,1)_52"  )

result=cbind(Model,AIC,LogLikelihood,CV)

#Model Selection
#theoretical ACF and PACF of model 3
ph = c( 0.1196)
th = c( -0.8168 , rep(0, 50),-0.5383 , -0.8168 *-0.5383)
L = 200
corrs = ARMAacf(ar = ph, ma = th, lag.max = L)
par.corrs = ARMAacf(ar = ph, ma = th, lag.max = L, pacf = T)
par(mfrow = c(2, 1))
plot(x = 0:L, y = corrs, type = "h", xlab = "Lag k", ylab = "Autocorrelation", main="Theoretical ACF of Model 3")
abline(h = 0)
plot(x = 1:L, y = par.corrs, type = "h", xlab = "Lag k", ylab = "Partial Autocorrelation", main="Theoretical PACF of Model 3")
abline(h = 0)

$#theoretical ACF and PACF of model 4
ph = c(0.1056, -0.0411)
th = c( -0.7995, rep(0, 50),-0.5419,-0.7995*-0.5419)
L = 200
corrs = ARMAacf(ar = ph, ma = th, lag.max = L)
par.corrs = ARMAacf(ar = ph, ma = th, lag.max = L, pacf = T)
par(mfrow = c(2, 1))
plot(x = 0:L, y = corrs, type = "h", xlab = "Lag k", ylab = "Autocorrelation", main="Theoretical ACF of Model 4")
abline(h = 0)
plot(x = 1:L, y = par.corrs, type = "h", xlab = "Lag k", ylab = "Partial Autocorrelation", main="Theoretical PACF of Model 4")
abline(h = 0)

#Selected Model is ARIMA(1,1,1)x(0,1,1)_52
m = 104
fcast = predict(q3fit3, n.ahead = m)

newx = 1:(length(q3d) + m)
newy = c(q3d, fcast$pred)

plot(newx, newy, type = "l", main="Predictions")
points(newx[((length(q3d)+1):(length(q3d) + m))], newy[((length(q3d)+1):(length(q3d) + m))], type = "l", col="blue" )

points(newx[(length(q3d) + 1):(length(q3d)+m)], fcast$pred + 2*fcast$se, col = "blue", type = "l", lty=2)
points(newx[(length(q3d) + 1):(length(q3d)+m)], fcast$pred - 2*fcast$se, col = "blue", type = "l", lty=2)

#Diagnositc
tsdiag(q3fit3)


