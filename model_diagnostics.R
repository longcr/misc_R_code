

# load packages ---------------------------------------------------------------

library(car)
library(lmtest)
library(vrtest)



fit1 = aov(log.tbs ~ Zone, data = dat)

plot(dat$Zone, residuals(fit1))



###############################################################################
# MODEL DIAGNOSTICS
###############################################################################


fit = fit1


# model results ---------------------------------------------------------------

print(summary(fit))




# plot fitted vs residuals ----------------------------------------------------

fitted.y = fit$fitted


# plot residuals vs fitted
plot(fitted.y, residuals(fit))
title("Residuals vs Fitted Values")
grid()




# normality diagnostics -------------------------------------------------------

# test normality using Shapiro-Wilks test 
res.shapiro = shapiro.test(residuals(fit))

print(res.shapiro)

if (res.shapiro$p.value < 0.05){
  print("Nonnormally distributed residuals")} else {
    print("Normally distributed residuals")
  }



# normality plot of residuals -------------------------------------------------

qqPlot(residuals(fit), main = "Normal Plot of Residuals")




# family of influence measures ------------------------------------------------

infl.fit = influence.measures(fit)

print(summary(infl.fit))

id.infl = which(apply(infl.fit$is.inf, 1, any))

print("Most influential observations:")
print(id.infl)




# Cook's Distance -------------------------------------------------------------

# find unusual Cooks Distance

fit.cook = cooks.distance(fit)

print("Influential Cooks D")
print(which(fit.cook > 3*mean(fit.cook)))




# plot Cook's diagnostics -----------------------------------------------------

fit.cook = cooks.distance(fit)

id.c = which(fit.cook > 3*mean(fit.cook))

# plot Cooks Distance
cymax <- 3*mean(fit.cook)
plot(fit.cook, ylim = c(0, cymax))
abline(h = c(1,3)*mean(fit.cook), col = c('blue', 'red'))
title("Cook's Distance")
grid()

if (length(id.c) > 0){ text(id.c, fit.cook[id.c], rownames(dat)[id.c], pos = 2, xpd = TRUE) }



# print studentized residuals diagnostics -------------------------------------

# STUDENTIZED RESIDUALS
fit.studres = rstudent(fit)

print("Noteworthy studentized residuals")
print(which(abs(fit.studres) > 3))




# plot studentized residuals diagnostics -------------------------------------

fit.studres = rstudent(fit)

id.sr = which(abs(fit.studres) > 3)

# plot studentized residuals
plot(rstudent(fit), ylim = c(-4,4))
abline(h = c(-3,+3), col = 'red', lty = 3)
title('Studentized Residuals')
grid()


if (length(id.sr) > 0){ text(id.sr, fit.studres[id.sr], rownames(dat)[id.sr], pos = 2, xpd = TRUE) }



# print leverage diagnostics --------------------------------------------------

# LEVERAGE based on HAT MATRIX
fit.hat = hatvalues(fit)

print("Noteworthy leverage values")
print(which(fit.hat > 3*mean(fit.hat)))



# plot leverage diagnostics ---------------------------------------------------

fit.hat = hatvalues(fit)

id.h = which(fit.hat > 3*mean(fit.hat))


# plot leverage
plot(fit.hat)
abline(h = c(1,3)*mean(fit.hat), col = 2)
title('Leverage')
grid()


if (length(id.h) > 0){ text(id.h, fit.hat[id.h], rownames(dat)[id.h], pos = 2, xpd = TRUE) }



# print functional form diagnostics -------------------------------------------

# test for functional form
# conditional mean of residuals equal to zero
# using the RESET test

res.fform = resettest(fit)

if (res.fform$p.value < 0.05){
  print("Functional Form Misspecified [E(e|X) <> 0]")} else {
    print("Functional Form Adequate [E(e|X) = 0]")
  }



# print constant var diagnostics ----------------------------------------------

# test for heteroskedasticity
# using the Breusch-Pagan test

res.bp = bptest(fit)
res.bp$p.value

if (res.bp$p.value < 0.05){
  print("Residuals have NON-constant variance")} else {
    print("Residuals have constant variance")
  }


# print autocorrelation diagnostics -------------------------------------------

# test for autocorrelation
# the Box-Ljung test only tests for specific individual lags

res.box = Box.test(residuals(fit), lag = 1, type = "Ljung-Box")

print("Box-Ljung Test for autocorrelation")

if (res.box$p.value < 0.05){
  print("Residuals NOT independent (autocorrelation)")} else {
    print("Residuals independent (no autocorrelation)")
  }



# the Auto.Q function uses a portmanteau test with multi-lags (10 by default)

portm.multilag = Auto.Q(y = residuals(fit))


print("Portmanteau Test for autocorrelation")

if (portm.multilag$Pvalue < 0.05){
  print("Residuals NOT independent (autocorrelation)")} else {
    print("Residuals independent (no autocorrelation)")
  }




# END CODE ####################################################################