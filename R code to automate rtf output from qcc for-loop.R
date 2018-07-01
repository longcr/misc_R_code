# Jon Easto, Frank
# MM
# Supply
# over time

# x.wd = getwd()
# setwd(x.wd)
# C:\Users\longcli\Dropbox\Work USSCO\Data for Analysis\0001 Complete\2014-06-19 Manual Markouts (Jon E)
# setwd("C:/Users/longcli/Dropbox/Work USSCO/Data for Analysis/0001 Complete/2014-06-19 Manual Markouts (Jon E)")


dat2.in = read.csv("CubewithTotalLines2 2012-May 2014.csv", header = TRUE)
head(dat2.in); tail(dat2.in); names(dat2.in)

levels(dat2.in$FacNum)

dat2.in$FacNum = as.factor(dat2.in$FacNum)

dat2.in$MMperFL = dat2.in$ManualMarkouts / dat2.in$FillableLines



fn.is.date <- function(x){ !all(is.na(as.Date(as.character(x),format="%d/%m/%Y"))) }

var.info.fn <- function(df.in){
  var.info = data.frame()
  for (i in 1:dim(df.in)[2]){ var.info[i,1] <- names(df.in)[i]
                              var.info[i,2] <- typeof(df.in[,i])
                              var.info[i,3] <- is.numeric(df.in[,i])
                              var.info[i,4] <- is.factor(df.in[,i])
                              var.info[i,5] <- fn.is.date(df.in[,i])
  } # can add columns for other data types
  colnames(var.info) = c("COLNAME","TYPEOF","IS.NUM","IS.FAC","IS.DATE")
  return(var.info)
  print(var.info)
}

var.info.fn(dat2.in)


#---------------------------------------------
# This function can be used to remove outliers based on IQR, reassigning the value to "NA"
# CAUTION:  Be careful about omitting values that are "outliers", as the outlier might be a legitimate part of the population
#---------------------------------------------

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

dat2.in$MMperFL.FILTER = remove_outliers(dat2.in$MMperFL)



#---------------------------------------------
# summary
#---------------------------------------------

summary(dat2.in)

hist(dat2.in$MMperFL.FILTER)
hist(log(dat2.in$MMperFL))

names(dat2.in)

dat2.tmp = dat2.in  # use because are experimenting with reordering factors

library(lattice)

histogram(~ MMperFL.FILTER | FacNum, data = dat2.tmp)

bwplot(MMperFL ~ FacNum | FacilityCAT, layout = c(3,1), data = dat2.tmp, scales=list(x=list(rot=90)))

bwplot(MMperFL.FILTER ~ FacNum | FacilityCAT, layout = c(3,1), data = dat2.tmp, scales=list(x=list(rot=90)))


# Lattice book p. 190
# rearrange the plot within the panels

dat2.tmp$FacilityCAT = with(dat2.tmp, reorder(FacilityCAT, MMperFL.FILTER, median))

dat2.tmp$FacNum = with(dat2.tmp, reorder(reorder(FacNum, MMperFL.FILTER, median), as.numeric(FacilityCAT)))

bwplot(MMperFL.FILTER ~ FacNum | FacilityCAT, data = dat2.tmp, 
       layout = c(3,1), 
       between = list(x = c(0.5, 0.5)),
       drop.unused.levels = TRUE)





#-----

# Manual Markouts

bwplot(ManualMarkouts ~ FacNum | FacilityCAT, layout = c(3,1), data = dat2.in, scales=list(x=list(rot=90)))

# find the facility with the unusual MM value
dat2.in[dat2.in$ManualMarkouts > 4000,]
dat2.in[dat2.in$ManualMarkouts < 0,]

# limit y-axis to 1200 (due to unusual value)
bwplot(ManualMarkouts ~ FacNum | FacilityCAT, layout = c(3,1), ylim = c(-50,1200), data = dat2.in, scales=list(x=list(rot=90)))



# Fillable Lines

bwplot(FillableLines ~ FacNum, data = dat2.in, scales=list(x=list(rot=90)))

bwplot(FillableLines ~ FacNum | FacilityCAT, layout = c(3,1), data = dat2.in, scales=list(x=list(rot=90)))



#---------------------------------------------
# quick anova model file
#---------------------------------------------

mod.1 = aov(MMperFL.FILTER ~ FacNum, data = dat2.in)
anova(mod.1)
summary(mod.1)

THSD = TukeyHSD(mod.1)
str(THSD)
dim(THSD$FacNum)
dim(THSD$FacNum[THSD$FacNum[,4] <= 0.05,])
THSD$FacNum[THSD$FacNum[,4] > 0.05,]

library(gplots)
plotmeans(MMperFL.FILTER ~ FacNum, data = dat2.in)


# anova by FacilityCAT

mod.2 = aov(MMperFL.FILTER ~ FacilityCAT, data = dat2.in)
anova(mod.2)
summary(mod.2)

THSD = TukeyHSD(mod.2)
str(THSD)
dim(THSD$FacNum)
dim(THSD$FacilityCAT[THSD$FacilityCAT[,4] <= 0.05,])
THSD$FacilityCAT[THSD$FacilityCAT[,4] > 0.05,]

library(gplots)
plotmeans(MMperFL.FILTER ~ FacNum, data = dat2.in)




# nested ANOVA Fixed Effects

res1 <- aov(MMperFL.FILTER ~ FacilityCAT + FacilityCAT/FacNum, data = dat2.in)
anova(res1)




#---------------------------------------------
# aggregate results between facilities
#---------------------------------------------

# summarize by facility


library(plyr)

head(dat2.in)

sub.agg1 = ddply(dat2.in, .(FacNum), function(x) c(avg.MMperFL = mean(x$MMperFL.FILTER, na.rm = TRUE), 
                                                   sd.MMperFL = sd(x$MMperFL.FILTER, na.rm = TRUE), 
                                                   nsize = length(x$MMperFL.FILTER), 
                                                   median.MMperFL = median(x$MMperFL.FILTER, na.rm = TRUE), 
                                                   IQR.MMperFL = IQR(x$MMperFL.FILTER, na.rm = TRUE))  )
sub.agg1$se.MMperFL = sub.agg1$sd.MMperFL / sqrt(sub.agg1$nsize)
head(sub.agg1)


library(ggplot2)

#------------------------------
# organized by facility only
# in decreasing order of average MMperFL

tmp1.sort = sub.agg1
tmp1.sort$FacNum = factor(tmp1.sort$FacNum, levels = tmp1.sort[order(tmp1.sort$avg.MMperFL),1])

p1 = ggplot(tmp1.sort, aes(x = avg.MMperFL, y = FacNum)) + 
  geom_point() + theme(legend.position = "none")  + 
  labs(title="Average Manual Markout per Fillable Lines"); p1


# p1 = ggplot(tmp1.sort, aes(x = avg.MMperFL, y = FacNum, color = factor(FacilityCAT))) + 
#   geom_point() + theme(legend.position = "none")  + 
#   labs(title="add later"); p1


# NOTE - add Facets or Color by FacilityCAT
# maybe something like this (but this does not yet work right)
#     + facet_wrap(~ FacilityCAT, ncol = 1)
# color
# aes(col = nsize)


# for printing the table of value to the console, decreasing average MMperFL
tmp1.sort[order(-tmp1.sort$avg.MMperFL),]

# for printing the table of value to the console, decreasing IQR MMperFL
tmp1.sort[order(-tmp1.sort$IQR.MMperFL),]



#------------------------------
# organized by facility AND facility category
# in decreasing order of average MMperFL

sub.agg2 = ddply(dat2.in, .(FacilityCAT, FacNum), function(x) c(avg.MMperFL = mean(x$MMperFL.FILTER, na.rm = TRUE), 
                                                                sd.MMperFL = sd(x$MMperFL.FILTER, na.rm = TRUE), 
                                                                nsize = length(x$MMperFL.FILTER), 
                                                                median.MMperFL = median(x$MMperFL.FILTER, na.rm = TRUE), 
                                                                IQR.MMperFL = IQR(x$MMperFL.FILTER, na.rm = TRUE)))
sub.agg2$se.MMperFL = sub.agg2$sd.MMperFL / sqrt(sub.agg2$nsize)
head(sub.agg2)


tmp2.sort = sub.agg2
tmp2.sort$FacNum = factor(as.factor(tmp2.sort$FacNum), levels = tmp2.sort[order(tmp2.sort$avg.MMperFL),2])

head(sub.agg2); tail(sub.agg2)
head(tmp2.sort); tail(tmp2.sort)

#------------
# original

p2 = ggplot(tmp2.sort, aes(x = avg.MMperFL, y = FacNum, color = factor(FacilityCAT))) + 
  geom_point(size = 3)  + labs(title="Average Manual Markout per Fillable Lines") + 
  labs(colour ="Facility Type") ; p2


# for printing the table of value to the console
tmp2.sort[order(tmp2.sort$FacilityCAT, -tmp2.sort$avg.MMperFL),]


#------------
# experiment with facet_grid
0.47*60
8.6*4

head(tmp2.sort)

p2.2 = ggplot(tmp2.sort, aes(x = avg.MMperFL, y = FacNum, color = factor(FacilityCAT))) + 
  geom_point()  + labs(title="Average Manual Markout per Fillable Lines") + 
  labs(colour ="Facility Type") ; p2.2

p2.2 + facet_grid(FacilityCAT ~ ., scales = "free", space = "free") + opts(strip.text.y = theme_text())

#------------
# add error bars (based on mean +/- 2*se)
# STILL NOT QUITE RIGHT - can added error bars, but not with faceting

p2.3 = ggplot(tmp2.sort, aes(x = avg.MMperFL, y = FacNum, color = factor(FacilityCAT))) + 
  geom_point() + labs(title="Manual Markout per Fillable Lines") + 
  labs(colour ="Facility Type") ; p2.3

#p2.3 + geom_segment(aes(xmax = avg.MMperFL + 2*se.MMperFL, xmin = avg.MMperFL - 2*se.MMperFL))

#geom_errorbarh(data=d, mapping=aes(y=drink, x=upper, xmin=upper, xmax=lower), height=0.2, size=1, color="blue") +
  
p2.3 + geom_errorbarh(data = tmp2.sort, mapping=aes(y=FacNum, x=avg.MMperFL, 
                                                    xmin=avg.MMperFL - 2*se.MMperFL, 
                                                    xmax=avg.MMperFL + 2*se.MMperFL),  height=0.01, size=1)

p2.3 + facet_grid(FacilityCAT ~ . , scales = "free", space = "free") 
  # somehow lose the error bars when add the facet

#------------


#------------------------------
# scatter plot by median and IQR
# by FacNum
#------------------------------

head(sub.agg1)

plot(sub.agg1$median.MMperFL, sub.agg1$IQR.MMperFL, xlab = "Median by FacNum", 
     pch = 21, col = 'blue', 
     ylab = "Interquartile Range by FacNum", 
     main = "Manual Markout per Fillable Lines\nby Facility")

abline(v = quantile(sub.agg1$median.MMperFL, 0.50), col = 'blue', lty = 3)
abline(v = quantile(sub.agg1$median.MMperFL, 0.25), col = 'darkgreen', lty = 3)
abline(v = quantile(sub.agg1$median.MMperFL, 0.75), col = 'darkgreen', lty = 3)

abline(h = quantile(sub.agg1$IQR.MMperFL, 0.50), col = 'blue', lty = 3)
abline(h = quantile(sub.agg1$IQR.MMperFL, 0.25), col = 'darkgreen', lty = 3)
abline(h = quantile(sub.agg1$IQR.MMperFL, 0.75), col = 'darkgreen', lty = 3)


# same scatterplot, but using sub.agg2 which has FacilityCAT
# so can add point colors

head(sub.agg2)

# save default parameters
# .pardefault <- par(no.readonly = T)
# par(.pardefault)


par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

plot(sub.agg2$median.MMperFL, sub.agg2$IQR.MMperFL, xlab = "Median by FacNum", 
     pch = 21, bg = c('red','green','black')[sub.agg2$FacilityCAT], 
     ylab = "Interquartile Range by FacNum", 
     main = "Manual Markout per Fillable Lines\nby Facility")

abline(v = quantile(sub.agg2$median.MMperFL, 0.50), col = 'blue', lty = 3)
abline(v = quantile(sub.agg2$median.MMperFL, 0.25), col = 'darkgreen', lty = 3)
abline(v = quantile(sub.agg2$median.MMperFL, 0.75), col = 'darkgreen', lty = 3)

abline(h = quantile(sub.agg2$IQR.MMperFL, 0.50), col = 'blue', lty = 3)
abline(h = quantile(sub.agg2$IQR.MMperFL, 0.25), col = 'darkgreen', lty = 3)
abline(h = quantile(sub.agg2$IQR.MMperFL, 0.75), col = 'darkgreen', lty = 3)

leg.txt = c("Hub","Primary","Spoke")
legend('topleft', leg.txt, pch = 19, col = c('red','green','black'))


# cex = 0.7, 

#---------------------------------------------
# time series and SPC plotting
# MANUAL subset by facility
#---------------------------------------------

levels(dat2.in$FacNum)  # lists codes for facilities

x.fac = "1" 
x.fac = "Boston" 
x.fac = "Chicago" 
x.fac = "Houston"

x.fac = "Memphis" 
x.fac = "Columbus" 
x.fac = "Minneapolis"


sub.fac = subset(dat2.in, dat2.in$FacNum == x.fac)
head(sub.fac); tail(sub.fac); names(sub.fac)

plot(1:length(sub.fac$MMperFL.FILTER), sub.fac$MMperFL.FILTER, type = 'l')

# make sure data is sorted in ascending date order for SPC chart


# An Individuals SPC chart has a strong assumption of normality of the data

library(car)
qqPlot(na.omit(sub.fac$MMperFL.FILTER), main = paste("Normality QQ Plot\n Facility -",x.fac))
shapiro.test(sub.fac$MMperFL.FILTER)



# SPC charting and statistics

library(qcc)

# Individuals (X) chart
qcc.X = qcc(na.omit(sub.fac$MMperFL.FILTER), type = 'xbar.one', 
            title = paste("X Chart for Facility ",x.fac,"\n Jan 2013 to May 2014"))



# Moving Range (MR) chart
dat.MR = na.omit( cbind(sub.fac$MMperFL.FILTER[-length(sub.fac$MMperFL.FILTER)], sub.fac$MMperFL.FILTER[-1]) )  ## Form subgroups of two consecutive values
qcc.MR = qcc(dat.MR, type = "R", 
             title = paste("Moving Range Chart for ",x.fac,"\n Jan 2013 to May 2014"))



# USE ONLY IF NEED TO CREATE LIMITS FROM SUBSET OF DATES AND APPLY TO NEWER DATA
#
# qcc X-chart with old and new limits
# x.old = sub.fac$bulkcartonperline[1:9]
#x.new = sub.fac$bulkcartonperline[10:length(sub.fac$bulkcartonperline)]

#qcc.X2 = qcc(x.old, type = 'xbar.one', newdata = x.new, 
#             title = paste("X Chart for ",x.fac,"\n Jan 2013 to Jun 2014"))


# separate plots with diff limits for old and new groups
#qcc.X.old = qcc(x.old, type = 'xbar.one', ylim = c(1.5, 1.9),
#                title = paste("X Chart for 1st group ",x.fac,"\n Jan 2013 to Jun 2014"))
#qcc.X.new = qcc(x.new, type = 'xbar.one',  ylim = c(1.5, 1.9),
#                title = paste("X Chart for 2nd group ",x.fac,"\n Jan 2013 to Jun 2014"))



# par(mfrow=c(2,1))
# plot(qcc.X, title = "Individuals SPC Chart\n Bulk Cartons per Line")
# plot(qcc.MR, title = "Moving Range SPC Chart\n Bulk Cartons per Line")
# par(mfrow=c(1,1))


# plot the data for the numerator and denominator
# dat2.in$MMperFL = dat2.in$ManualMarkouts / dat2.in$FillableLines


MM.ts = ts(sub.fac$ManualMarkouts, start = c(2012,1), frequency = 12)
FL.ts = ts(sub.fac$FillableLines, start = c(2012,1), frequency = 12)


ts.plot(1000*MM.ts, FL.ts, col = c('blue','green'), 
        main = paste("Manual Markouts and\n Fillable Lines for",x.fac))
leg.txt = c("MM","FL")
legend('bottomleft', leg.txt, lty = 1, col=c('blue','green'))


##########

# FRANK - I stopped here on THU night

#------------------------------
# automated SPC charting across all zones
# write results to table for further focus
#------------------------------

# here, limit strata to Facility (FacNum)
strata = dat2.in$FacNum

levels(strata)
length(levels(strata))

head(dat2.in)


library(qcc)
library(rtf)


# store in results
# zone, $Center, $StdDev, $violations[1], $violations[2], avg.ssize = mean($sizes),
# min.ssize = min($sizes), max.ssize = max($sizes)

#--------------------
# initialize dataframe used to capture results
rslt.df <- data.frame(x.strata = factor(),
                      x.center = numeric(), 
                      x.stdev = numeric(), 
                      MR.center = numeric(), 
                      MR.upperlim = numeric() )
                      #p.viol1 = list() 
                      #p.viol2 = list()


#--------------------
# used to set up an rtf file to collect the results

path = getwd() 
# set the path in which to store the files

rtf = RTF("resultsfile.doc", width=8.5, height=11, omi=c(1, 1, 1, 1), font.size=10)
# creates the document in which to store the results
# the name of the file may be changed within the parentheses
# rm('rtf')

appendage <- ".png" 

#--------------------


for ( i in 1:length(levels(strata)) ){
#i=20
  strata.level = levels(strata)[i]
  sub.strata = subset(dat2.in, strata == strata.level)
    
  # SPC charting and statistics
  
  # Individuals (X) chart
  qcc.X = qcc(na.omit( sub.strata$MMperFL.FILTER ), type = 'xbar.one', 
              title = paste("X Chart for ",strata.level,"\n Jan 2013 to Jun 2014"), 
              plot = FALSE)
  
  # Moving Range (MR) chart
  dat.MR = na.omit( cbind(sub.strata$MMperFL.FILTER[-length(sub.strata$MMperFL.FILTER)], sub.strata$MMperFL.FILTER[-1]) )  ## Form subgroups of two consecutive values
  qcc.MR = qcc(dat.MR, type = "R", 
               title = paste("Moving Range Chart for ",strata.level,"\n Jan 2013 to Jun 2014"), 
               plot = FALSE)

  
  newRow <- data.frame(x.strata = as.factor(strata.level),
                       x.center = round(qcc.X$center, 5), 
                       x.stdev  = round(qcc.X$std.dev, 5), 
                       #p.viol1 = qcc.X$violations[1], 
                       #p.viol2 = qcc.X$violations[2], 
                       MR.center = round(qcc.MR$center, 5), 
                       MR.UCL = round(qcc.MR$limits[2], 5) )
  
  rslt.df <- rbind(rslt.df, newRow)

#----------
# write results to a MS Word file

  
  # create png files from output

  X.outputFile <- file.path(path, paste0('Facility ', strata.level,' X-chart', appendage)) 
  png(file = X.outputFile)
  plot(qcc.X, title = paste("X Chart for Facility ",strata.level,"\n Jan 2013 to Jun 2014"))
  dev.off()

  MR.outputFile <- file.path(path, paste0('Facility ', strata.level,' MR-chart', appendage)) 
  png(file = MR.outputFile)
  plot(qcc.MR, title = paste("Moving Range Chart for Facility ",strata.level,"\n Jan 2013 to Jun 2014"))
  dev.off()
  
  # add png files to rtf document

  addNewLine(rtf)
  addPng(rtf, file = X.outputFile, width=5, height=4)

  addNewLine(rtf)  
  addPng(rtf, file = MR.outputFile, width=5, height=4)

  addNewLine(rtf)
  addPageBreak(rtf, width=8.5, height=11, omi=c(1, 1, 1, 1))



}  # end loop

# after loop, add table of overall results

addTable(rtf, rslt.df)
addPageBreak(rtf, width=8.5, height=11, omi=c(1, 1, 1, 1))

addNewLine(rtf)
addSessionInfo(rtf)
# writes information about this R session to the file 
# to support reproducibility

done(rtf)
  




# for printing to console
rslt.df[order(rslt.df$x.center, decreasing = TRUE),]



# plot results
# cex = rslt.df$p.med.ssize,
# col = rainbow(length(z))[rank(z)]
# bg = rainbow(length(rslt.df$p.med.ssize))[rank(rslt.df$p.med.ssize)], 


# this section needed to color by median sample sizes

range01 <- function(x)(x-min(x))/diff(range(x))

colRamp <- function(x){
  cols <- colorRamp(rev(rainbow(5)))(range01(x))
  apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
}  


# this section creates the plot

head(rslt.df)

with(rslt.df, 
     plot(x.center, x.stdev,   
          pch = 21, col = 'blue', 
          bg = colRamp(MR.UCL), 
          xlab = "X-chart stdev", 
          ylab = "X-chart center", 
          main = "X-chart statistics\n (dot color ~ Moving Range UCL)",
          panel.first = grid()  )
)



# jitter(x.stdev, amount = 0.02)
# bg = colRamp(MR.UCL), 
# legend("top", col = cRamp(rslt.df$p.med.ssize), pch=16)

abline(v = quantile(rslt.df$x.center, 0.50), col = 'blue', lty = 3)
abline(v = quantile(rslt.df$x.center, 0.25), col = 'darkgreen', lty = 3)
abline(v = quantile(rslt.df$x.center, 0.75), col = 'darkgreen', lty = 3)

abline(h = quantile(rslt.df$x.stdev, 0.50), col = 'blue', lty = 3)
abline(h = quantile(rslt.df$x.stdev, 0.25), col = 'darkgreen', lty = 3)
abline(h = quantile(rslt.df$x.stdev, 0.75), col = 'darkgreen', lty = 3)


# 3D plots

library(scatterplot3d)

with(rslt.df, 
     scatterplot3d(x = p.center, y = p.stdev, z = p.max.ssize, 
                   highlight.3d = TRUE, col.axis = "blue",
                   col.grid = "lightblue", main = "misc", pch = 20)
)


# spinning 3d Scatterplot
library(rgl)

with(rslt.df, 
     plot3d(p.center, p.stdev, p.max.ssize, col="red", size=3)
)


# another spinning 3d Scatterplot

library(Rcmdr)

with(rslt.df, 
     scatter3d(p.center, p.stdev, p.max.ssize)
)






##### END CODE #####
