# red bead


library(qcc)



dat1 = rnorm(5,20,3)
dat2 = rnorm(5,20,3)
dat3 = rnorm(5,20,3)


d1 = c(3,6,10)
d2 = c(3,7,7)
d3 = c(0,14,14)


datT = append(append(d1, d2), d3)

xc = qcc(datT, type = "xbar.one")

xp = qcc(datT, size = 25, type = "p")




#####
