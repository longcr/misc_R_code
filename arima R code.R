# R code for arima

# This code is offered only as a general guide to get you started.
# MODIFY THIS TO SUIT YOUR OWN NEEDS!!!


library(forecast)
library(tseries)  # for adf.test
library(fUnitRoots)  # for unitroots test
help(package = "fUnitRoots")



# function to create acf and pacf plots together

acf.plot = function(x){
        par(mfrow=c(2,1))
	mlag = length(x)/3   # fixes number of lags at one-third of length of ts, can be adjusted
        Acf(x, lag.max = mlag)
        Pacf(x, lag.max = mlag)
        par(mfrow=c(1,1))
        }


# read and check the data

dat = read.table("clipboard", header = TRUE)
head(dat); tail(dat)


# create the ts object, print and plot to check

d.ts = ts(dat$<tbd>, start = c(<tbd>), frequency = <tbd>)

print(d.ts)   # look at the ts object in the R console
plot(d.ts)    # plot the ts object


acf.plot(d.ts)


# based on stationarity of ts plot, explore differencing 
#   (and, if needed, predifferencing transform)
# maybe also try seasonal differencing

d.ts.diff = diff(d.ts, lag = 1, differences = 1)	# difference the ts
d.ts.diff = diff(log(d.ts), lag = 1, differences = 1)	# difference the log of the ts (only if needed)

d.ts.sdiff = diff(d.ts, lag = 12, differences = 1)      # seasonal differencing (assumes period = 12)

d.ts.bothdiff = diff(diff(d.ts), lag = 12)              # applies BOTH nonseasonal and seasonal differencing


# BE SURE TO PLOT THE OUTPUT - THE POINT HERE IS TO SEE IF THE TS IS STATIONARY PRIOR TO ARIMA

plot(d.ts.diff)  

# and also check the acf/pacf of the output
acf.plot(d.ts.diff)


# BUILD ARIMA MODEL
# once you have a ts that is stationary (after differencing, etc)
# then start to explore arima model
# refer to the tables of options based on behavior of acf/pacf in the slides and text
#
# USE THE ORIGINAL TIME SERIES IN THE ARIMA MODEL - put any differencing into the model order


fit = arima(d.ts, order = c(0,1,2), seasonal = list(order = c(1,1,0), period = 12)); print(fit)



# some arima model diagnostics - use others as you see fit

coefficients(fit)   # look to see if any coefficients are NOT statistically significant (compare point est to 2x stderr)

t(confint(fit))  # creates confidence intervals for each coefficient

acf.plot(residuals(fit))

adf.test(residuals(fit))

Box.test(residuals(fit))


# forecast based on arima model (you choose h)

plot(forecast(fit, h = ?????))

	

#-----------------------------------
# get.best.arima function from Cowpertwait p. 144
#-----------------------------------

get.best.arima = function(x.ts, maxord = c(1,1,1,1,1,1))   # the maxord can be modified to suit
{
	best.aic = 1e8
	n = length(x.ts)

	for (p in 0:maxord[1]) 
	  for (d in 0:maxord[2])
	    for (q in 0:maxord[3])

	      for (P in 0:maxord[4])
       	        for (P in D:maxord[5])
       	          for (Q in D:maxord[6])
		  {
			fit = aimra(x.ts, order = c(p,d,q), 
					  seas = list(order = c(P,D,Q)), 
					  frequency(x.ts), method = "CSS")
			fit.aic = -2*fit$loglik + (log(n) + 1)*length(fit$coef)
			if (fit.aic best.aic)
	  	  	{
				best.aic = fit.aic
				best.fit = fit
				best.model = c(p,d,q,P,D,Q)
			}
		  }

		  list(best.aic, best.fit, best.model)

}  # end function



Give it a try.

Cliff


##########

