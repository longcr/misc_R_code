# experimenting with SPC violations
#   capturing excursions from the last point

# original data
set.seed = 1
d.vec = rnorm(20, 50, 2)
hist(d.vec)


# MODIFY the original data

# add a special cause point at end of data
d.sc = append(d.vec, 60)


# add a special cause point at end of data
d.rr = append(d.vec, rnorm(8, 53, 0.5))



#--------------------

library(qcc)

# Individuals (X) chart - ORIGINAL DATA
X.out = qcc(d.vec, type = 'xbar.one', title = "X Chart")

X.out$violations


# Moving Range (MR) chart - ORIGINAL DATA
d.MR = cbind(d.vec[-length(d.vec)], d.vec[-1])  ## Form subgroups of two consecutive values
MR.out = qcc(d.MR, type = "R", title = "Moving Range Chart")

MR.out$violations

#--------------------

last.obs.fn = function(x){
                last.x = length(x)
                last.mr = length(x) - 1
                return(c(last.x, last.mr))
                }

#--------------------
# test SPECIAL CAUSE VARIATION at last observation

# Individuals (X) chart - DATA WITH LAST SPECIAL CAUSE VARIATION
X.out.sc = qcc(d.sc, type = 'xbar.one', title = "X Chart")


# Moving Range (MR) chart - DATA WITH LAST SPECIAL CAUSE VARIATION
d.MR.sc = cbind(d.sc[-length(d.sc)], d.sc[-1])  ## Form subgroups of two consecutive values
MR.out.sc = qcc(d.MR.sc, type = "R", title = "Moving Range Chart")


# test X chart violations to see if last point is an excursion
last.obs.fn(d.sc)[1] == X.out.sc$violations$beyond.limits


# test MR chart violations to see if last point is an excursion
last.obs.fn(d.sc)[2] == MR.out.sc$violations$beyond.limits


# test X chart violations to see if last point is in a RUNS-RULE VIOLATION
last.obs.fn(d.sc)[1] %in% X.out.sc$violations$violating.runs


# test MR chart violations to see if last point is in a RUNS-RULE VIOLATION
last.obs.fn(d.sc)[2] %in% MR.out.sc$violations$violating.runs


#--------------------
# test X chart RUNS-RULES VIOLATIONS to see if includes last point 

# Individuals (X) chart - DATA WITH LAST SPECIAL CAUSE VARIATION
X.out.rr = qcc(d.rr, type = 'xbar.one', title = "X Chart")


# Moving Range (MR) chart - DATA WITH LAST SPECIAL CAUSE VARIATION
d.MR.rr = cbind(d.rr[-length(d.rr)], d.rr[-1])  ## Form subgroups of two consecutive values
MR.out.rr = qcc(d.MR.rr, type = "R", title = "Moving Range Chart")


# test X chart violations to see if last point is an excursion
last.obs.fn(d.rr)[1] == X.out.rr$violations$beyond.limits


# test MR chart violations to see if last point is an excursion
last.obs.fn(d.sc)[2] == MR.out.rr$violations$beyond.limits


# test X chart violations to see if last point is in a RUNS-RULE VIOLATION
last.obs.fn(d.rr)[1] %in% X.out.rr$violations$violating.runs


# test MR chart violations to see if last point is in a RUNS-RULE VIOLATION
last.obs.fn(d.rr)[2] %in% MR.out.rr$violations$violating.runs




##### END CODE #####
