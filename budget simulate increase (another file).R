


# logit transform example
# experimenting

pi = seq(0.0001, 0.9999, by = 0.0001)
length(pi)

odds = pi/(1-pi)

xlogit = log(odds)

plot(odds, pi, type = 'l', col = 'red')

plot(odds, log(pi), type = 'l', col = 'red')

plot(xlogit, pi, type = 'l', col = 'red')


plot(ts(pi))

plot(ts(1-exp(-pi)))


# experimenting

z = seq(0.01, 10, by = 0.01)
plot(z, exp(z))
plot(z, (1-exp(z)))
plot(z, exp(-z))


# use this one

plot(z, 1-exp(-z), type = 'l', col = 'blue')
grid()
