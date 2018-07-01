# link to shinyapps.io account
# user:  long.admin@crlstatistics.net
# 22JAN2015

# https://www.shinyapps.io/admin/#/dashboard


# STEP 1 - INSTALL DEVTOOLS ---------------------------------------------------

install.packages('devtools')



# STEP 2 - INSTALL SHINYAPPS --------------------------------------------------

devtools::install_github('rstudio/shinyapps')



# STEP 3 - AUTHORIZE ACCOUNT --------------------------------------------------

shinyapps::setAccountInfo(name='crlstatistics', 
                          token='5B3EAABE6D803873969980D06190F9D2', 
                          secret='1usON4jWpnjFVv/BfIik3Zw4eyNwDHPnrKtClSB3')


# STEP 4 - DEPLOY -------------------------------------------------------------

library(shinyapps)
shinyapps::deployApp('path/to/your/app')



# END CODE ####################################################################