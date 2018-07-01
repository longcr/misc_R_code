#---|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8
# 2014-06-30
# simulate budget increase


# constant delta --------------------------------------

level1 = vector(mode = "numeric", length = 0)
level1[1] = 0.8

delta1 = 0.1


for (i in 2:100){ 
  level1[i] = level1[i-1] + (1 - level1[i-1])*delta1
}


plot(ts(level1))
points(level1, pch = '-')



# variable delta --------------------------------------

level2 = vector(mode = "numeric", length = 0)
level2[1] = 0.8

delta.base = 0.1
delta2 = vector(mode = "numeric", length = 0)
delta2[1] = (1 - level2[1])


for (i in 2:100){ 
  level2[i] = level2[i-1] + (1 - level2[i-1])*delta2[i-1]
  delta2[i] = delta.base/level2[i]
}


plot(ts(level2))
points(level2, pch = '-')


# compare delta plans ---------------------------------

plot(ts(delta2), col = 'red', lty = 2)
  points(delta2, pch = '-')
abline(h = delta1 - 0.0005, col = 'blue', lty = 1)
leg.txt = c("accelerated delta pct", "constant delta pct")
legend("topright", leg.txt, col = c('red','blue'), lty = c(2,1))

?legend


##### END CODE #####

