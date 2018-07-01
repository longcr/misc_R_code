# 19AUG2014
# ITBQ analysis
# using data directly from OpsAnalytics.accdb database
# combined with facility info


# libraries ###################################################################

library(RODBC)
library(plyr)
library(lattice)
library(ggplot2)
library(qcc)



# get the data ################################################################

# Connect to Access db --------------------------------------------------------

# for 64-bit windows (requires 32-bit R)
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                             DBQ=S:/Admin-Share/key measures/Analytics/OpsAnalytics.accdb")


# This code filters database records to remove quarterly results (monthly only)
# (someone with better SQL skills could probably write this more succinctly!)


# read itbq data --------------------------------------------------------------

d.itbq <- sqlQuery( channel , paste ("SELECT * FROM Quality",
  "WHERE  ( (([Quality].[Time Period])='January') Or 
          (([Quality].[Time Period])='February') Or 
          (([Quality].[Time Period])='March') Or 
          (([Quality].[Time Period])='April') Or 
          (([Quality].[Time Period])='May') Or 
          (([Quality].[Time Period])='June') Or 
          (([Quality].[Time Period])='July') Or 
          (([Quality].[Time Period])='August') Or 
          (([Quality].[Time Period])='September') Or 
          (([Quality].[Time Period])='October') Or 
          (([Quality].[Time Period])='November') Or 
          (([Quality].[Time Period])='December')  )
          And (([Quality].[Business])='USSCO') "  )    )

levels(d.itbq$Business)
with(d.itbq, table(Year*month))


# Look at the beginning and end of the data object

head(d.itbq); tail(d.itbq); names(d.itbq)


# close the ODBC connection
odbcCloseAll()


# check data types for each field

var.info.fn(d.itbq)

# change FacNum from integer (numeric) to factor
d.itbq$FacNum = as.factor(d.itbq$FacNum)

# change FacNum from factor to integer (numeric, non-factor) - FOR SQL LEFT JOIN
d.itbq$FacNum = as.factor(d.itbq$FacNum)
table(d.itbq$FacNum)

d.itbq$facnum = d.itbq$FacNum  # field with common name for use in LEFT JOIN



# adjust col names ------------------------------------------------------------


# original names from itbq database - used in R script that follows
[1] "Business"                  "Region"                    "FacNum"                   
[4] "Facility"                  "Year"                      "Time Period"              
[7] "Fill Lines"                "In the Box Quality Goal %" "Warehouse Errors"         
[10] "Warehouse Errors Goal %"   "Damage"                    "Damage Goal %"            
[13] "Manual Markouts"           "Manual Markouts Goal %"    "Goal Fill Lines" 

table(d.itbq$FacNum)
levels(d.itbq$FacNum)
levels(d.itbq$Facility)


# names from prior code
[1] "facnum"     "facility"   "year"       "month"      "monthnum"   "fill_lines" "whse"      
[8] "damage"     "mm"         "region"     "director"   "factype"


# adjust names 
names(d.itbq)

colnames(d.itbq)[,"Time Period"] =  c("Business", 
                                      "Region", 
                                      "facnum",     # FacNum
                                      "facname",    # Facility
                                      "year",       # Year
                                      "month",      # Time Period
                                      "fill_lines", # Fill Lines
                                      "itbq_pct",   # In the Box Quality Goal %
                                      "whse",       # Warehouse Errors
                                      "whse_pct_goal",    # Warehouse Errors Goal %
                                      "damage",     # Damage
                                      "damage_pct_goal",  # Damage Goal %
                                      "mm",         # Manual Markouts
                                      "mm_pct_goal",      # Manual Markouts Goal %
                                      "fill_lines_goal",  # Goal Fill Lines"
                                      )

whse
damage
mm


# create and set monthnum

table(dat.in[,"Time Period"])

dat.in[dat.in[,"Time Period"] == "January", "monthnum"] = 1
dat.in[dat.in[,"Time Period"] == "February", "monthnum"] = 2
dat.in[dat.in[,"Time Period"] == "March", "monthnum"] = 3
dat.in[dat.in[,"Time Period"] == "April", "monthnum"] = 4
dat.in[dat.in[,"Time Period"] == "May", "monthnum"] = 5
dat.in[dat.in[,"Time Period"] == "June", "monthnum"] = 6
dat.in[dat.in[,"Time Period"] == "July", "monthnum"] = 7
dat.in[dat.in[,"Time Period"] == "August", "monthnum"] = 8
dat.in[dat.in[,"Time Period"] == "September", "monthnum"] = 9
dat.in[dat.in[,"Time Period"] == "October", "monthnum"] = 10
dat.in[dat.in[,"Time Period"] == "November", "monthnum"] = 11
dat.in[dat.in[,"Time Period"] == "December", "monthnum"] = 12

with(dat.in, table("Time Period"*monthnum))


# read archived itbq data -----------------------------------------------------
#
# FROM PRIOR ANALYSIS - using to get col names aligned with R script
# dat.in = read.csv("all dc cyc counts.csv", header = TRUE)

# dat.in = read.table("clipboard", header = TRUE)

dat.in = read.csv("2013-2014 FY Data ITBQ.csv", header = TRUE)
head(dat.in, 10); tail(dat.in); names(dat.in); dim(dat.in)


# read table with facility info -----------------------------------------------
#
# from file "Facility Information.xlsx"

d.facinfo = read.table("clipboard", header = TRUE)

var.info.fn(d.facinfo)

# change facnum from integer (numeric) to factor

d.facinfo$facnum = as.factor(d.facinfo$facnum)
levels(d.facinfo$facnum)

# change facnum back to integer (numeric, non-factor) - USE FOR SQL LEFT JOIN

d.facinfo$facnum = as.integer(d.facinfo$facnum)
table(d.facinfo$facnum)


# eventually want to remove data from defunct facilities from the d.itbq data !!


# join the d.itbq data with d.facinfo -----------------------------------------
# based on facility to add demographics
# http://stackoverflow.com/questions/1299871/how-to-join-data-frames-in-r-inner-outer-left-right


dat.in = merge(x = d.itbq, y = d.facinfo, by = "facnum", all.x=TRUE)
dim(d.itbq)
dim(d.facinfo)
dim(dat.in)

var.info.fn(dat.in)


# more data prep --------------------------------------------------------------

# remove any incomplete month
dat.old = dat.in
dat.in = dat.old[!( (dat.in$year == 2014) & (dat.in$monthnum == 7) ),]


var.info.fn(dat.in)

summary(dat.in)

head(dat.in)


# relationship between each ITB variable and fill lines ? -------------------

par(mfrow = c(3,1))
with(dat.in, plot(fill_lines, whse, main = 'whse'))
with(dat.in, plot(fill_lines, damage, main = 'damage'))
with(dat.in, plot(fill_lines, mm, main = 'mm'))
par(mfrow = c(1,1))

head(dat.in)
pairs(dat.in[,6:9])


# library(lattice)
# xyplot(whse + damage + mm ~ fill_lines, data = dat.in)


# add proportions related to fill_lines -------------------------------------

dat.in$whseprop = dat.in$whse / dat.in$fill_lines
dat.in$damageprop = dat.in$damage / dat.in$fill_lines
dat.in$mmprop = dat.in$mm / dat.in$fill_lines


# misc scatterplots ---------------------------------------------------------

pairs(dat.in[,c(6,13,14,15)])
pairs(dat.in[,c(6,13,14,15)], col=dat.in$factype)



names(dat.in)

library(reshape2)
dat.wide = dat.in[,c(2,6,10,12,13,14,15)]
names(dat.wide)
dat.long = melt(dat.wide, id.vars=c("facility","region","factype"))
names(dat.long)

#library(ggplot2)
#ggplot(dat.in, aes())


library(lattice)
splom(~dat.in[,c(6,13,14,15)], groups = factype, data = dat.in)



# ANOVA model (not used yet) ------------------------------------------------
# ANOVA (lm) model to check to see if there are noteworthy
# differences between regions

# fit1 = lm( ~ region, data = )
# summary(fit1)


# plot standardized model residuals
# plot(cc.sub$Region, rstandard(fit5)); abline(h = c(-2.5,0,2.5), lty = 2)
# plot(cc.sub$Facility, rstandard(fit5)); abline(h = c(-2.5,0,2.5), lty = 2)

# using lattice
# xyplot(rstandard(fit5) ~ cc.sub$Region)
# xyplot(rstandard(fit5) ~ cc.sub$Facility, scales=list(x=list(rot=90)))

# qqPlot(residuals(fit5))


# graphics by strata (facility, facility type, region) -----------------------

library(lattice)

bwplot(whseprop ~ facility | region + factype, data = dat.in, 
       scales=list(x=list(rot=90)))



# ggplot2 on orig data -------------------------------------------------------

library(ggplot2)

datplot = dat.in
head(dat.in)


# run one
setfactype = "HUB"
setfactype = "PRIMARY"
setfactype = "SPOKE"

datplot = subset(dat.in, factype == setfactype)
head(datplot)


# run one
setvar = datplot$whseprop; varname = "whseprop"
setvar = datplot$damageprop; varname = "damageprop"
setvar = datplot$mmprop; varname = "mmprop"

ggplot(  datplot, aes( reorder(facility, setvar, median), setvar, col = region)  ) + 
  geom_boxplot() + 
  facet_grid(. ~ region, scale = "free_x") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = setfactype) + 
  xlab("facility") + 
  ylab(varname) 



# more ggplot2 on orig data - boxplots
# this one not as interesting

ggplot(datplot, aes(x = region, y = setvar)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = varname) + 
  xlab("region") + 
  ylab(varname) 


# slightly more interesting

ggplot(datplot, aes( reorder(facility, setvar, median), setvar, col = region)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = varname) + 
  xlab(setfactype) + 
  ylab(varname) 


# less interesting

ggplot(datplot, aes( reorder(facility, setvar, median), setvar)) + 
  geom_boxplot() + 
  facet_grid(region ~ factype, scale = "free_x") + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = varname) + 
  xlab(setfactype) + 
  ylab(varname) 






# create sub.agg1 (summarize by facility) ------------------------------------
# includes strata for factype and region
#
# LIMIT RESULTS TO MOST RECENT (2014)

library(plyr)

head(dat.in)

sub.pre = subset(dat.in, year == 2014)

sub.agg1 = ddply(sub.pre, .(facility, factype, region), 
                 function(x) c(median_fill_lines = median(x$fill_lines),
                               median_whse = median(x$whse),
                               IQR_whse = IQR(x$whse),
                               median_damage = median(x$damage),
                               IQR_damage = IQR(x$damage),
                               median_mm = median(x$mm),
                               IQR_mm = IQR(x$mm),
                               # ALSO add proportion relative to fill_lines 
                               median_prop_whse = median(x$whseprop),
                               IQR_prop_whse = IQR(x$whseprop),
                               median_prop_damage = median(x$damageprop),
                               IQR_prop_damage = IQR(x$damageprop),
                               median_prop_mm = median(x$mmprop),
                               IQR_prop_mm = IQR(x$mmprop) ) )

sub.agg1[,1:2]


# created sub.agg9 to merge with credit.in ------------------------------
# did not want to change sub.agg1 above due to code below
# used in the scatterplot code later on

sub.pre = subset(dat.in, year == 2014)

sub.agg9 = ddply(sub.pre, .(facility, facnum, factype, region), 
                 function(x) c(median_fill_lines = median(x$fill_lines),
                               median_whse = median(x$whse),
                               IQR_whse = IQR(x$whse),
                               median_damage = median(x$damage),
                               IQR_damage = IQR(x$damage),
                               median_mm = median(x$mm),
                               IQR_mm = IQR(x$mm),
                               # ALSO add proportion relative to fill_lines 
                               median_prop_whse = median(x$whseprop),
                               IQR_prop_whse = IQR(x$whseprop),
                               median_prop_damage = median(x$damageprop),
                               IQR_prop_damage = IQR(x$damageprop),
                               median_prop_mm = median(x$mmprop),
                               IQR_prop_mm = IQR(x$mmprop) ) )

sub.agg9[,1:2]  # list of facilities and facility numbers

#----------------------------------------------------------------------------

xvar = sub.agg1$median_prop_whse
xvar = sub.agg1$median_prop_damage
xvar = sub.agg1$median_prop_mm

hist(xvar)
plot(density(xvar))

library(car)
qqPlot(xvar)
shapiro.test(xvar)


# scatter plot by median and IQR ---------------------------------------------
# by facility and ITBQ metric (whse, damage, mm)

head(sub.agg1)

# use ITBQvar as a generic placeholder for the variable of interest
# choose only one, then run the code
ITBQvar = "whse";   vcol = c(1,2,5,6)
ITBQvar = "damage"; vcol = c(1,2,7,8)
ITBQvar = "mm";     vcol = c(1,2,9,10)


xvar = sub.agg1[,vcol[3]]; yvar = sub.agg1[,vcol[4]]


# for printing ordered results table to console
sub.agg1[order( sub.agg1[,vcol[4]], decreasing = TRUE ), vcol]

# brute-force method for checking
# sub.agg1[order( sub.agg1$median_whse, decreasing = TRUE ), vcol]


plot(xvar, yvar, xlab = paste("Median", ITBQvar), 
     xlim = c(0,1.2*max(xvar)), ylim = c(0,1.2*max(yvar)),
     pch = 21, col = 'blue', 
     cex = sub.agg1$median_fill_lines/100000,  # sizes prop to fill lines
     ylab = paste("Interquartile Range by", ITBQvar), 
     main = paste(ITBQvar,"by facility (2014)") )

abline(v = quantile(xvar, 0.50), col = 'blue', lty = 3)
abline(v = quantile(xvar, 0.25), col = 'darkgreen', lty = 3)
abline(v = quantile(xvar, 0.75), col = 'darkgreen', lty = 3)

abline(h = quantile(yvar, 0.50), col = 'blue', lty = 3)
abline(h = quantile(yvar, 0.25), col = 'darkgreen', lty = 3)
abline(h = quantile(yvar, 0.75), col = 'darkgreen', lty = 3)




# rotatable 3d plot ----------

x = xvar
y = yvar
z = sub.agg1$median_fill_lines

library(rgl)

plot3d(x,y,z, 
       type = "s",
       size=1,
       xlab = "x", 
       ylab = "y", 
       zlab = "z",
       box=T)


# scatter plot by median and IQR of PROPORTION -------------------------------
# by facility and ITBQ metric (prop whse, damage, mm)
#
# median_prop_whse
# median_prop_damage
# median_prop_mm


head(sub.agg1)


# use ITBQvar as a generic placeholder for the variable of interest
# choose only one, then run the code
p.ITBQvar = "prop_whse";   p.vcol = c(1,2,3,11,12)
p.ITBQvar = "prop_damage"; p.vcol = c(1,2,3,13,14)
p.ITBQvar = "prop_mm";     p.vcol = c(1,2,3,15,16)


p.xvar = sub.agg1[,p.vcol[4]]; p.yvar = sub.agg1[,p.vcol[5]]


# for printing ordered results table to console
hh = sub.agg1[order( sub.agg1[,p.vcol[5]], decreasing = TRUE ), p.vcol]
head(hh); tail(hh)

### more interesting

plot(p.xvar, p.yvar, xlab = paste("Median", p.ITBQvar), 
     # xlim = c(0,1.2*max(p.xvar)), ylim = c(0,1.2*max(p.yvar)),
     pch = 21, col = 'blue', 
     ylab = paste("Interquartile Range by", p.ITBQvar), 
     main = paste(p.ITBQvar,"by facility (2014)") )

abline(v = quantile(p.xvar, 0.50), col = 'blue', lty = 3)
abline(v = quantile(p.xvar, 0.25), col = 'darkgreen', lty = 3)
abline(v = quantile(p.xvar, 0.75), col = 'darkgreen', lty = 3)

abline(h = quantile(p.yvar, 0.50), col = 'blue', lty = 3)
abline(h = quantile(p.yvar, 0.25), col = 'darkgreen', lty = 3)
abline(h = quantile(p.yvar, 0.75), col = 'darkgreen', lty = 3)



# GGPLOT2 scatter plot by median and IQR of proportion -----------------------
# uses data sub.agg1
#
# by facility and ITBQ metric (prop whse, damage, mm)

dim(sub.agg1)

# THIS CODE DUPLICATES SECTION ABOVE using GGPLOT2 ---------------------------
# use ITBQvar as a generic placeholder for the variable of interest
# choose only one, then run the code
p.ITBQvar = "prop_whse";   p.vcol = c(1,2,3,11,12)
p.ITBQvar = "prop_damage"; p.vcol = c(1,2,3,13,14)
p.ITBQvar = "prop_mm";     p.vcol = c(1,2,3,15,16)


p.xvar = sub.agg1[,p.vcol[4]]
p.yvar = sub.agg1[,p.vcol[5]]


# for printing ordered results table to console SORT BY MEDIAN
hh = sub.agg1[order( sub.agg1[,p.vcol[5]], decreasing = TRUE ), p.vcol]
head(hh); tail(hh)

# for printing ordered results table to console SORT BY IQR
hh = sub.agg1[order( sub.agg1[,p.vcol[5]], decreasing = TRUE ), p.vcol]
head(hh); tail(hh)


datplot2 = sub.agg1
# head(datplot2)

ggplot(datplot2, aes(p.xvar, p.yvar)) + 
  geom_point(aes(col = factype), size = 4) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = paste(p.ITBQvar,"by facility (2014)")) + 
  xlab("median") + 
  ylab("IQR") 



# run anova on median proportions by factype ---------------------------------
# specific metric chosen above

fit2 = aov(datplot2[,p.vcol[4]] ~ factype, data = datplot2)
summary(fit2)

plot(fitted(fit2), residuals(fit2))
abline(h=0, col='blue', lty=2)

TukeyHSD(fit2)


# subset data for another median-IQR plot using ggplot2 ----------------------

setfactype = "HUB"
setfactype = "PRIMARY"
setfactype = "SPOKE"

datplot3 = subset(sub.agg1, factype == setfactype)
head(datplot3)

p3.xvar = datplot3[,p.vcol[4]]
p3.yvar = datplot3[,p.vcol[5]]


# for printing ordered results table to console - sort by MEDIAN
hh3 = datplot3[order( datplot3[,p.vcol[4]], decreasing = TRUE ), p.vcol]
head(hh3); tail(hh3)

# for printing ordered results table to console - sort by IQR
hh3 = datplot3[order( datplot3[,p.vcol[5]], decreasing = TRUE ), p.vcol]
head(hh3); tail(hh3)


ggplot(datplot3, aes(p3.xvar, p3.yvar)) + 
  geom_point(aes(color = region), size = 4) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = paste(setfactype,"\n metric =",p.ITBQvar)) + 
  xlab("median") + 
  ylab("IQR") 




# Time Series and SPC --------------------------------------------------------



# time series and SPC plotting by raw metric or proportions -----------
# subset by facility
# this section done manually for each zone


names(dat.in)

#levels(dat.in$facility)  # lists codes for facilities


# facilities of interest
x.facility = "Cranbury"
x.facility = "LosAngeles"
x.facility = "Dallas"
x.facility = "Sacramento"
x.facility = "Atlanta"
x.facility = "Chicago"
x.facility = "Baltimore"
x.facility = "Tulsa"

x.facility = "StLouis"

# better facilities
x.facility = "Portland"
x.facility = "Tulsa"
x.facility = "Miami"
x.facility = "SaltLakeCity"


# noteworthy proportion whse per fill line
x.facility = "Denver"       # very high proportion median and IQR
x.facility = "Houston"      # very high proportion median
x.facility = "Boston"       # very low proportion median and IQR
x.facility = "Albany"       # very low proportion median and IQR



strata = dat.in$facility; strata.name = "facility"


cc.strata = subset(dat.in, dat.in$facility == x.facility)  # select subset
cc.strata = cc.strata[order(cc.strata$year, cc.strata$monthnum, 
                            decreasing = FALSE),]  # order by month
head(cc.strata); tail(cc.strata)

dim(cc.strata)

# using raw data
qccvar = "whse"; qcol = 7
qccvar = "damage"; qcol = 8
qccvar = "mm"; qcol = 9

# using proportions
qccvar = "whseprop"; qcol = 13
qccvar = "damageprop"; qcol = 14
qccvar = "mmprop"; qcol = 15


# SPC charting and statistics

library(qcc)

# Individuals (X) chart

x.chart = qcc( cc.strata[,qccvar], type = "xbar.one", 
               title = paste("Individuals chart\n", x.facility, qccvar) )


# Moving Range (MR) chart

dat.MR = cbind(cc.strata[,qccvar][-length(cc.strata[,qccvar])], 
               cc.strata[,qccvar][-1])  
## Form subgroups of two consecutive values

mr.chart = qcc(dat.MR, type = "R", 
               title = paste("Moving Range chart\n", x.facility, qccvar) )



# !!! when SPC charting proportions, also plot numerator and denominator
# head(cc.strata)

par(mfrow=c(2,1))
plot(ts(cc.strata[,"fill_lines"]), ylab = "fill_lines")
plot(ts(cc.strata[,(qcol-6)]), ylab = colnames(cc.strata)[(qcol-6)])
par(mfrow=c(1,1))



# automated SPC charting across all facilities -------------------------------

#------------------------------
# write results to table for further focus
#------------------------------


# strata = cc.dat.in$Whsezone; strata.name = "Whsezone"
strata = dat.in$facility; strata.name = "facility"

levels(strata)
length(levels(strata))


library(qcc)

# store results in "rslt.df"
# strata, $Center, $StdDev, $violations[1], $violations[2], avg.ssize = mean($sizes),
# min.ssize = min($sizes), max.ssize = max($sizes)

# initialize dataframe used to capture results
rslt.df <- data.frame(p.strata = factor(),
                      p.center = numeric(), 
                      p.stdev = numeric(), 
                      #p.viol1 = list(), 
                      #p.viol2 = list(), 
                      p.med.ssize = numeric(), 
                      p.min.ssize = numeric(), 
                      p.max.ssize = numeric() )


for ( i in 1:length(levels(strata)) ){
  
  strata.level = levels(strata)[i]
  
  cc.strata = subset(dat.in, strata == strata.level)
  
  # clean this up
  
  # SPC charting and statistics
  
  p.chart = qcc(cc.strata.prop$ct.notmatch, sizes = cc.strata.prop$nsize, 
                type = "p", 
                title = as.character(strata), plot = FALSE)
  
  newRow <- data.frame(p.strata = strata.level,
                       p.center = round(p.chart$center, 5), 
                       p.stdev  = round(p.chart$std.dev, 5), 
                       #p.viol1 = p.chart$violations[1], 
                       #p.viol2 = p.chart$violations[2], 
                       p.med.ssize = median(p.chart$sizes), 
                       p.min.ssize = min(p.chart$sizes), 
                       p.max.ssize = max(p.chart$sizes) )
  
  rslt.df <- rbind(rslt.df, newRow)
  
}  # end loop



# for printing to console
rslt.df[order(rslt.df$p.center, decreasing = TRUE),]



# plot results
# cex = rslt.df$p.med.ssize,
# col = rainbow(length(z))[rank(z)]
# bg = rainbow(length(rslt.df$p.med.ssize))[rank(rslt.df$p.med.ssize)], 


# this section needed to color by median sample sizes

range01 <- function(x)(x-min(x))/diff(range(x))

colRamp <- function(x){
  cols <- colorRamp(rainbow(5))(range01(x))
  apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
}  

?rnorm
# this section creates the plot

head(rslt.df)

with(rslt.df, 
     plot(p.center, jitter(p.stdev, amount = 0.02), 
          xlab = paste("p-chart center by ", strata.name),  
          pch = 21, col = 'blue', 
          bg = colRamp(p.med.ssize), 
          xlim = c(0,1), ylim = c(0,1), 
          ylab = paste("p-chart stdev by ", strata.name), 
          main = paste("p-chart statistics\nby ", strata.name) 
     )
)

# legend("top", col = cRamp(rslt.df$p.med.ssize), pch=16)

abline(v = quantile(rslt.df$p.center, 0.50), col = 'blue', lty = 3)
abline(v = quantile(rslt.df$p.center, 0.25), col = 'darkgreen', lty = 3)
abline(v = quantile(rslt.df$p.center, 0.75), col = 'darkgreen', lty = 3)

abline(h = quantile(rslt.df$p.stdev, 0.50), col = 'blue', lty = 3)
abline(h = quantile(rslt.df$p.stdev, 0.25), col = 'darkgreen', lty = 3)
abline(h = quantile(rslt.df$p.stdev, 0.75), col = 'darkgreen', lty = 3)


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


# plot using ggplot2

col = rslt.df$p.max.ssize
geom_point(aes(size = log(rslt.df$p.max.ssize)))


gg.plot <- ggplot( data = rslt.df, aes(x = rslt.df$p.center, y = rslt.df$p.stdev, 
                                       col = rslt.df$p.max.ssize) ) 
+ geom_point(aes(size = rslt.df$p.max.ssize))

+ scale_colour_gradientn(colours=rainbow(5))




# try GoogleVis motion chart ##################################################

# need median and IQR data from both 2013 and 2014, so will use code from above



# create sub.agg1 (summarize by facility) ------------------------------------
# includes strata for factype and region
#
# use both 2013 and 2014 for motion chart



library(zoo)
dat.in$yearmon <- as.yearmon(paste0(sub.gvis$year,sub.gvis$monthnum),"%Y%m") 
dat.in$yearmon2 <- as.Date(sub.gvis$yearmon, format = "%Y/%m/%d")

sub.gvis = dat.in[,c(2,12,13,14,15)]
head(sub.gvis)

#as.Date(as.yearmon(format(x), "%Y%m"))

#fn.is.date <- function(x){ !all(is.na(as.Date(as.character(x),format="%d/%m/%Y"))) }
#fn.is.date(sub.gvis$yearmon2)


head(sub.gvis)
tail(sub.gvis)


library(googleVis)
# demo(WorldBank) ## At least version googleVis_0.2.10 required


# gvisMotionChart(data, idvar = "id", timevar = "time", xvar = "",
#                 yvar = "", colorvar = "", sizevar = "", date.format = "%Y/%m/%d",
#                 options = list(), chartid)

vx2 = gvisMotionChart(data = Fruits, idvar = "Fruit", timevar = "Year", 
                      xvar = "Sales", yvar = "Expenses", 
                      colorvar = "Location", sizevar = "Profit", date.format = "%Y/%m/%d",
                      options = list())


M = gvisMotionChart(data = sub.gvis, idvar = "facility", timevar = "yearmon2", 
                    xvar = "mmprop", yvar = "whseprop", 
                    colorvar = "factype", sizevar = "damageprop",
                    date.format = "%Y/%m/%d",
                    options = list())

plot(vx2)

?cumsum



#============================================================================

# added for BL on 24JUL -----------------------------------------------------
# adds info for 'shortage credits'
# see other data file "2014-07 denied credits.xlsx"

credit.in = read.table("clipboard", header = TRUE)
var.info.fn(credit.in)


sub.agg9.wcredit <- merge(sub.agg9, credit.in, by="facnum")
names(sub.agg1)
names(credit.in)
head(sub.agg1)
head(sub.agg9.wcredit); tail(sub.agg9.wcredit)
names(sub.agg9.wcredit)


col.to.pair = c("median_fill_lines",
                "median_whse", "IQR_whse",
                "median_damage", "IQR_damage",
                "median_mm", "IQR_mm",
                "median_prop_whse", "IQR_prop_whse",
                "median_prop_damage", "IQR_prop_damage",
                "median_prop_mm", "IQR_prop_mm",
                "DollarsApproved", "DollarsDenied")

col.to.pair = c("median_fill_lines",
                "median_whse", 
                "median_damage", 
                "median_mm", 
                "median_prop_whse", 
                "median_prop_damage", 
                "median_prop_mm", 
                "DollarsApproved", "DollarsDenied")

pairs(sub.agg9.wcredit[,col.to.pair], col = total$factype)

cor.out = cor(sub.agg9.wcredit[,col.to.pair])
str(cor.out)


corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

corstarsl(sub.agg9.wcredit[,col.to.pair])


# explore median_prop_whse by median_fill_lines by factype and facility

setvar = sub.agg9.wcredit$median_prop_whse; varname = "median_prop_whse"
setvar = sub.agg9.wcredit$median_prop_damage; varname = "median_prop_damage"
setvar = sub.agg9.wcredit$median_prop_mm; varname = "median_prop_mm"


ggplot(sub.agg9.wcredit, aes(median_fill_lines, median_prop_whse)) + 
  geom_point() + 
  facet_grid(factype ~ .) + 
  labs(title = varname)


fit9 = lm(DollarsDenied ~ median_whse + median_damage + median_mm, data = sub.agg9.wcredit)
summary(fit9)


sub.agg9.wcredit$prop_DollarsDenied = sub.agg9.wcredit$DollarsDenied/sub.agg9.wcredit$median_fill_lines

names(sub.agg9.wcredit)

sub.agg9.wcredit$prop_whse_dmg = sub.agg9.wcredit$median_prop_whse + sub.agg9.wcredit$median_prop_damage

fit10 = lm(median_whse ~ DollarsDenied + median_fill_lines, data = sub.agg9.wcredit)
summary(fit10)

par(mfrow=c(1,1))
plot(sub.agg9.wcredit$median_whse, residuals(fit10))


fit11 = lm(prop_whse_dmg ~ prop_DollarsDenied, data = sub.agg9.wcredit)
summary(fit11)

library(xtable)
xtbl.out = xtable(summary(fit11))
print(xtbl.out, type = 'html')


par(mfrow=c(1,1))
plot(sub.agg9.wcredit$prop_DollarsDenied, residuals(fit11))
abline(h=0, lty = 2, col = 'blue')





fit12 = lm(prop_whse_dmg ~ prop_DollarsDenied*factype, data = sub.agg9.wcredit)
summary(fit12)

library(xtable)
xtbl.out = xtable(fit12)
print(xtbl.out, type = 'html')


par(mfrow=c(1,1))
plot(sub.agg9.wcredit$prop_DollarsDenied, residuals(fit12))
abline(h=0, lty = 2, col = 'blue')


fit.compare = anova(fit11, fit12)

fit.c.xtbl = xtable(fit.compare)
print(fit.c.xtbl, type = 'html')



#=============================================================================


# FUNCTIONS ###################################################################


# function to print a summary of dataframe columns and their type ------------

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



##### END CODE ################################################################

