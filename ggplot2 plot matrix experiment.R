# try combining ggplot2 graphs into 2x2 matrix

library(ggplot2)
library(gridExtra)


# create the mock data and plots ----------------------------------------------

dat = data.frame(cbind(x = seq(1:30), 
                       y1 = rnorm(30), 
                       y2 = rnorm(30), 
                       y3 = rnorm(30), 
                       y4 = rnorm(30)))


plot1 = ggplot(dat, aes(x = x, y = y1)) + geom_line()

plot2 = ggplot(dat, aes(x = x, y = y2)) + geom_line()

plot3 = ggplot(dat, aes(x = x, y = y3)) + geom_line()

plot4 = ggplot(dat, aes(x = x, y = y4)) + geom_line()


#------------------------------------------------------------------------------


# build the plots 
p1 <- ggplot_gtable(ggplot_build(plot1))
p2 <- ggplot_gtable(ggplot_build(plot2))
p3 <- ggplot_gtable(ggplot_build(plot3))
p4 <- ggplot_gtable(ggplot_build(plot4))



# another method - uses gridExtra
# library(gridExtra)
# using print seems to overcome a difficulty when using Shiny with grid.arrange


# OLD - replaced 31AUG2015 due to changes in gridExtra package
# print( grid.arrange(arrangeGrob(p2.common.y, p1.common.y, 
#                                 p3.blankPlot, p4.lineplots,
#                                 ncol=2, nrow=2,
#                                 widths=c(1,4)), heights=c(1,2)) )


# NEW - taken from
# http://stackoverflow.com/questions/29062766/store-output-from-gridextragrid-arrange-into-an-object
zz <- gridExtra::arrangeGrob(p1, p2, p3, p4, ncol=2, nrow=2)

grid::grid.draw(zz)


plot(zz)


# END CODE ####################################################################