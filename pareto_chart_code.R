

# LOAD PACKAGES ###############################################################

library(readxl)



# FUNCTIONS ###################################################################



# Pareto Function
fn.pareto = function(x, s.sub){
  
  tbl = sort(table('VIOLATION'), decreasing = TRUE)  # counts
  p.tbl = prop.table(tbl)  # proportions
  
  axis.max = trunc(10*max(p.tbl) + 1)/10  # adds some height to the y-axis
  
  # default par(mar = c(5, 4, 4, 2) + 0.1) based on c(bottom, left, top, right)
  par(mar = c(7, 5, 4, 4) + 0.1)  # adds space at the bottom, left, right margins
  
  barplot(p.tbl, las = 3, ylim = c(0, axis.max), 
          ylab = 'Proportion', 
          main = paste("Pareto of Supplier Issues\n")  )
  
  lines(cumsum(p.tbl)*axis.max, col = 'blue', lwd = 2)  
  # adds line for cum prop scaled from (0,1) to (0,axis.max)
  abline(h = 0.8*axis.max, col = 'blue', lty = 2)  
  # adds horizontal line at 80%
  axis(side = 4,at = seq(0, axis.max, 0.2*axis.max),labels = c(0,0.2,0.4,0.6,0.8,1))
  mtext(text = "Cumulative Proportion", side = 4)
  
}  # END FUNCTION

fn.pareto(supplier2, 'pay_to_affl')



# PARETO CHARTS - VARIOUS METHODS #############################################

# prepare data ----------------------------------------------------------------

#This code is necessary to run some of the code below
# load 2016 SPT data 

# fname1 = paste0("C:/Users/fross22/Documents/Supplier Performance/Analysis/Explore SPT/", 
#                 "2016 SPT Data.xlsx")

fname1 = "2016 SPT Data.xlsx"

fsheet1 = '2016 SPT Data'

d_spt = read_excel(path = fname1, sheet = fsheet1)

d_spt = clean_names(d_spt)


supplier2 = d_spt
s.sub = supplier2[supplier2$pay_to_affl == 2490,]
x = sort(table(s.sub$viol_desc), decreasing = TRUE)
x3 = x
p.tbl = prop.table(x3)



# pareto of supplier issues ---------------------------------------------------

barplot(p.tbl, las = 3, ylim = c(0, axis.max), 
        ylab = 'Proportion', 
        main = paste("Pareto of Supplier Issues") 
)

lines(cumsum(p.tbl)*axis.max, col = 'blue', lwd = 2)  
# adds line for cum prop scaled from (0,1) to (0,axis.max)
abline(h = 0.8*axis.max, col = 'blue', lty = 2)  
# adds horizontal line at 80%
axis(side = 4,at = seq(0, axis.max, 0.2*axis.max),labels = c(0,0.2,0.4,0.6,0.8,1))
mtext(text = "Cumulative Proportion", side = 4)



# pareto of supplier issues fit to the window better --------------------------

axis.max = trunc(10*max(p.tbl2) + 1)/10
par(mar = c(9, 5, 2, 4) + 0.1)
barplot(p.tbl2, las = 3, ylim = c(0, axis.max), 
        ylab = 'Proportion', 
        main = paste("Pareto of Supplier Issues")
)
lines(cumsum(p.tbl2)*axis.max, col = 'blue', lwd = 2)
abline(h = 0.8*axis.max, col = 'blue', lty = 2)
axis(side = 4,at = seq(0, axis.max, 0.2*axis.max),labels = c(0,0.2,0.4,0.6,0.8,1))
mtext(text = "Cumulative Proportion", side = 4)



# pareto of supplier issues weighted by operation impact ----------------------

axis.max = trunc(10*max(p.tbl3) + 1)/10
par(mar = c(7, 5, 4, 4) + 0.1)
barplot(p.tbl3, las = 3, ylim = c(0, axis.max), 
        ylab = 'Proportion', 
        main = paste("Pareto of Supplier Issues\n Freq Weighted by Operational Impact"),
        names.arg = bind[,1]
)
lines(cumsum(p.tbl3)*axis.max, col = 'blue', lwd = 2)
abline(h = 0.8*axis.max, col = 'blue', lty = 2)
axis(side = 4,at = seq(0, axis.max, 0.2*axis.max),labels = c(0,0.2,0.4,0.6,0.8,1))
mtext(text = "Cumulative Proportion", side = 4)



# END CODE ####################################################################