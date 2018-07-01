# cost of quality

library(plotrix)  # for the axis.break function

x = 0:100


# add factor to compress or stretch COQ curves
# regular k = 1, compressed k = 0.5, very compressed k = 0.3, stretched k = 2
k = 0.3



pa = exp(k*x/10)
PA = pa/max(pa)  # mock data for Prevention and Appraisal costs

IEF = 1/exp(k*x/25)  # mock data for Internal and External Failure costs

# combine PA and IEF for Total COQ

TotalCOQ = PA + IEF + 0.02  # the offset is only for visibility on the graph
min.TCOQ = min(TotalCOQ)



# Exploratory only
# plot(x,k)
# plot(x,PA); lines(x,IEF); lines(x,PA1, col = 'red'); lines(x,IEF1, col = 'blue')

# diagnostic only
# max(PA)
# max(IEF)
# max(TotalCOQ)


#-----------------------------------------------------------
# create plot
#-----------------------------------------------------------

plot(x, PA, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     xlim = c(0,100), ylim = c(0, max(TotalCOQ)),
     xlab = "(low) <--- QUALITY ---> (high)\n", ylab = "COST"); box()

# add breaks in x and y axes

axis.break(axis=1, breakpos = median(x), style = 'slash')
axis.break(axis=2, breakpos = max(PA)/2, style = 'slash')

# add lines for IEF and TotalCOQ

lines(x, IEF, type = 'l', lwd = 2, col = 'red')
lines(x, TotalCOQ, type = 'l', lty = 2, lwd = 3, col = 'darkgreen')

# add line and point showing minimum Total Cost of Quality

xmin.TCOQ = x[TotalCOQ == min.TCOQ]
abline(v = xmin.TCOQ, lty = 4)
points(xmin.TCOQ, min.TCOQ, col = 'red', pch = 19)

# add horizontal line showing margin

cost.margin = 0.67*max(PA)
abline(h = cost.margin, lty = 9)

# add legend to plot

leg.txt <- c("Costs of Internal/External Failure", "Costs of Prevention/Appraisal", "Total COQ")
legend("top", leg.txt, col = c('red','blue','darkgreen'), cex = 0.7, 
       lty = c(1,1,2), lwd = c(1,1,2))
title("Total Cost of Quality")


# OPTIONAL
# add vertical arrow showing net margin after Total COQ

arrows(x0 = xmin.TCOQ, y0 = min.TCOQ, 
       x1 = xmin.TCOQ, y1 = cost.margin, 
       code = 3, lwd = 2) 


#-----------------------------------------------------------


?legend
?mtext
?axis
?lines
?arrows
?plot

##### END CODE #####
