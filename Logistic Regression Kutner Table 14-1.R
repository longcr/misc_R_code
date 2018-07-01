# Logistic Regression Example

# code uses parts from
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/


# example from
# https://sites.google.com/a/crlstatistics.net/statrefs/statwiki-home/statwiki-main-2/methods-2/regression/logistic-regression/logistic-regression-example-1-statistica


# other references
# http://stats.idre.ucla.edu/r/dae/logit-regression/
# https://cran.r-project.org/web/packages/HSAUR/vignettes/Ch_logistic_regression_glm.pdf




# FUNCTIONS ###################################################################

# reverse logit function manually coded
rev_logit_fn = function(linpred){
  prob = 1 / (1 + exp(-linpred))
  return(prob)
}


# LOAD PACKAGES ###############################################################

library(ggplot2)
library(ROCR)
library(plyr)
library(aod)



# LOAD DATA ###################################################################

dat = read.csv('CH14TA01.csv', header = TRUE)



# GRAPHICAL EXPLORATION #######################################################

# use 'jitter' to separate co-located points on x-axis
with(dat, plot(x = jitter(months_exp, 3), y = task_success))



# ANALYSIS ####################################################################

# fit null model --------------------------------------------------------------

fit0 = glm(task_success ~ 1, family = binomial(link = 'logit'), data = dat)

summary(fit0)


# fit model -------------------------------------------------------------------

fit1 = glm(task_success ~ months_exp, family=binomial(link='logit'), data = dat)

summary(fit1)


# compare fit to null model ---------------------------------------------------

fit_compare = anova(fit0, fit1, test = 'Chisq')

str(fit_compare)  # look at structure


# get p-value from comparison

pchisq(q = fit_compare$Deviance[2], df = fit_compare$Df[2], lower.tail=FALSE) 


# same result

with(fit1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))



# compare fit to null model - second method -----------------------------------
# review model based on "deviance"

anova(fit1, test="Chisq")


# for fit get odds ratio and conf int -----------------------------------------
# to get the odds ratio, exponential the coefficients

odds_ratio = cbind(exp(coef(fit1)), exp(confint(fit1)))

odds_ratio


# make predictions from model -------------------------------------------------

# predicted response

# generate 'new' x-values
newdat = data.frame(seq(0, 50, by = 1))
names(newdat) = 'months_exp'

pred_out = predict(fit1, newdata = newdat, type = 'response', se = TRUE)

newdat$pred = pred_out$fit
newdat$se_fit = pred_out$se.fit


# add linear predictors

linpred_out = predict(fit1, newdata = newdat, type = 'link', se = TRUE)

newdat$linpred = linpred_out$fit
newdat$se_linpred = linpred_out$se.fit


# add lower confidence level based on linear predictor
newdat = transform(newdat, linpred_lcl = linpred - 2*se_linpred, linpred_ucl = linpred + 2*se_linpred)

# add upper confidence level based on linear predictor
newdat = transform(newdat, pred_lcl = rev_logit_fn(linpred_lcl), pred_ucl = rev_logit_fn(linpred_ucl))


# plot sigmoid curve ----------------------------------------------------------

# plot using base R

plot(jitter(dat$months_exp, 3), dat$task_success, xlim = c(min(newdat[,1]), max(newdat[,1])))
lines(newdat$months_exp, newdat$pred, type = 'l')

# add confint
lines(newdat$months_exp, newdat$pred_lcl, col = 'red', lty = 2)
lines(newdat$months_exp, newdat$pred_ucl, col = 'red', lty = 2)



# plot using ggplot2

# plots sigmoid curve
sigmoid_plot = ggplot(data = newdat, aes(x = months_exp, y = pred)) + 
  geom_line() 

# plots original and new values as points
sigmoid_plot = sigmoid_plot + 
  geom_point(data = dat, aes(x = months_exp, y = task_success)) 
  # + geom_jitter()

# adds confidence intervals (point-wise, not confidence bands)
sigmoid_plot = sigmoid_plot + 
  geom_line(aes(x = months_exp, y = pred_lcl), color = 'red', linetype = 2) +
  geom_line(aes(x = months_exp, y = pred_ucl), color = 'red', linetype = 2)

sigmoid_plot



# another ggplot method

sigmoid_plot2 = ggplot(data = newdat, aes(x = months_exp, y = pred)) + 
  geom_point() + 
  stat_smooth(method = "glm", method.args=list(family="binomial"), se=TRUE) +
  geom_point(data = dat, aes(x = months_exp, y = task_success))

sigmoid_plot2



# to show the linear (regular) regression line
sigmoid_plot3 = sigmoid_plot2 + geom_smooth(method = "lm", se = FALSE)

sigmoid_plot3
# this is why we don't use regular regression to model binary responses




# ROC curve and AUC -----------------------------------------------------------
# library(ROCR)
# intro:  https://ccrma.stanford.edu/workshops/mir2009/references/ROCintro.pdf

p <- predict(fit1, type="response")

pr <- prediction(p, dat$task_success)

prf <- performance(pr, measure = "tpr", x.measure = "fpr")

plot(prf, col = 'red')
abline(0,1, lty = 2, col = 'blue')


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


# demonstrate reverse logit ---------------------------------------------------

# prediction based on the "linear predictor" aka link
linpred = predict(fit1, type = 'link')


# these are the same

plogis(linpred)
(predict(fit1, type = 'response'))
(revlog = rev_logit_fn(linpred))



# Wald Test -------------------------------------------------------------------
# library(aod)
# http://stats.idre.ucla.edu/r/dae/logit-regression/

wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 1:2)



# deviance residuals ----------------------------------------------------------
# Kutner p. 594

# plot studentized pearson residuals vs linear predictor

res_student <- rstudent(fit1, type = "pearson")

plot(predict(fit1), res_student,
    xlab = "Linear Predictor", ylab = "Studentized Residuals",
    ylim = max(abs(res_student)) * c(-1,1))

lines(lowess(predict(fit1), res_student, f = 0.7, iter = 0), col = "red", lty = 2)
# for the lowess plot use the settings in Kutner p. 595

abline(h = 0, lty = 2)



# plot deviance residuals vs predicted probability

res_dev <- residuals(fit1, type = "deviance")

plot(predict(fit1, type = 'response'), res_dev,
     xlab="Predicted Probability", ylab = "Deviance Residuals",
     ylim = max(abs(res_dev)) * c(-1,1))

lines(lowess(predict(fit1, type = 'response'), res_dev, f = 0.7, iter = 0), col = "red", lty = 2)
# for the lowess plot use the settings in Kutner p. 595

abline(h = 0, lty = 2)




# # NOT NECESSARY #############################################################
# # SEE GEOM_SMOOTH IN ORIGINAL PLOT
# # now do linear model based on proportions ----------------------------------
# 
# with(dat, table(months_exp, task_success))
# 
# dat2 = dat
# dat2 = transform(dat2, month_grp = trunc(months_exp / 4))
# 
# with(dat2, table(month_grp, task_success))
# 
# 
# dat2a = ddply(dat2, .(month_grp), summarize,
#               sum_success = sum(task_success),
#               len_success = length(task_success),
#               prop_success = sum_success / len_success)
# 
# 
# fit_lm = lm(prop_success ~ month_grp, data = dat2a)
# summary(fit_lm)
# 
# 
# # plots sigmoid curve
# sigmoid_plot3 = ggplot(data = newdat, aes(x = months_exp, y = pred)) + 
#   geom_line() 
# 
# # plots original and new values as points
# sigmoid_plot3 = sigmoid_plot3 + 
#   geom_point(data = dat, aes(x = months_exp, y = task_success)) 
# 
# sigmoid_plot3 = sigmoid_plot3 +
#   geom_smooth(method = "lm", se = FALSE)
  
  
  

# END #########################################################################
