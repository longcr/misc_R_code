
library(dplyr)


mu_x0 <- 100
sd_x0 <- 10
ssize <- 77


rslt <- data.frame(i = as.numeric(), dtot = as.numeric())


for (loop_i in 1:1000){
  
  set.seed(loop_i); x <- rnorm(ssize, mean = mu_x0, sd = sd_x0); 
  mx <- mean(x) 
  sx <- sd(x)
  
  diff_m <- abs(mx - mu_x0)
  diff_s <- abs(sx - sd_x0)
  
  diff_tot <- diff_m + diff_s
  
  rslt[loop_i, 'i'] <- loop_i
  rslt[loop_i, 'dtot'] <- diff_tot
  
}



rslt <- rslt %>% mutate(absdif = abs(dtot - 0))

hist(rslt$absdif)

rslt <- arrange(rslt, absdif)


plot(rslt$absdif)

head(rslt)
use_index <- rslt[1,'i']

print(use_index)
set.seed(use_index); x = rnorm(ssize, mean = mu_x0, sd = sd_x0); mean(x); sd(x)


# END CODE ####################################################################