# random walk with random impulse/disturbances
# 2018-06-22
# Cliff Long


# LOAD PACKAGES ###############################################################

library(tidyverse)



# FUNCTIONS ###################################################################

shift_fn <- function(x, lag) {
  n <- length(x)
  xnew <- rep(NA, n)
  if (lag < 0) {
    xnew[1:(n-abs(lag))] <- x[(abs(lag)+1):n]
  } else if (lag > 0) {
    xnew[(lag+1):n] <- x[1:(n-lag)]
  } else {
    xnew <- x
  }
  return(xnew)
}



# MAIN ########################################################################


# create time index
timex <- 1:400
chooseseed <- 10

# select index to apply impulse function
set.seed(chooseseed); impulse <- sample(timex, 1)

# impulse function
y_imp <- ifelse(timex %in% impulse, -1, 0)

# random walk
set.seed(chooseseed); y_rw <- cumsum(rnorm(timex, mean = 0))
rw_range <- max(y_rw) - min(y_rw)

# lagged random walk
y_imprw <- shift_fn(y_rw, impulse)

# gaussian white noise
set.seed(chooseseed); y_gn <- rnorm(timex)

# exponential decay
y_expdecay <- exp(1/(timex*0.5)) * rw_range * 0.1
# max(y_expdecay)

# lagged exponential decay starting at impulse index
y_impdecay <- shift_fn(y_expdecay, impulse)

# lagged exponential decay times random walk starting at impulse index
y_impdecayrw <- y_impdecay * y_imprw

# sinusoid steady state
y_ss <- sin(timex/(pi)) * rw_range/2


dat <- data.frame(cbind(timex, y_imp, y_expdecay, y_imprw, y_impdecay, y_impdecayrw, y_ss)) %>% 
  mutate(y_all = rowSums(select(., c('y_ss', 'y_impdecayrw')), na.rm = TRUE)) %>% 
  gather(signal_type, value, y_imp:y_all)


plot_cats <- c('y_ss', 'y_imp', 'y_expdecay', 'y_impdecay', 'y_imprw', 'y_impdecayrw', 'y_all')
plot_cats <- c('y_ss', 'y_imp', 'y_all')

datplot <- dat %>% filter(signal_type %in% plot_cats)


plot1 <- ggplot(datplot, aes(x = timex, y = value, color = signal_type)) + 
  geom_line() + 
  facet_grid(signal_type ~ ., scales = 'free_y') + 
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  ylab('Process Outcome') + 
  xlab('Time')
plot1



# EXAMPLE
# x <- seq(0,8*pi,length.out=100)
# y <- sin(x)
# plot(x,y,type="l")
# 
# x <- seq(0,8*pi,length.out=100)
# x/pi <- seq(0,8,length.out=100)