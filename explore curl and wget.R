# R read MS Excel xlsm file from SharePoint
# 2014-12-12



# method using gdata - seems to work with SharePoint --------------------------
# NOTE:  requires 'perl' installed
#
# example from
# http://r.789695.n4.nabble.com/trying-to-import-xls-or-xlsx-files-td3620580.html

library(gdata) 

fileurl = "http://sharepoint2/ops/quality/metricspc/Metric OptIn List/SPC Metric Opt-In List.xlsm"


d.optin.init <- read.xls(fileurl, 
                         sheet = "OPT-IN LIST", 
                         perl = "/usr/bin/perl")

getwd()

head(d.optin.init)
tail(d.optin.init)
dim(d.optin.init)

#-----------------------------------

# for bridging the gap between Linux server and Windows SharePoint
# try this, from http://goo.gl/5kH0jJ

# begin code segment ----------
tmp <- tempfile()
download.file(fileurl, 
              destfile=tmp, 
              method="wget")

d.optin.tryagain = read.xls(tmp)
unlink(tmp)

# end code segment ----------
?download.file



#-----------------------------------

library(RCurl)



#-----------------------------------

# works from the Linux shell command line




#-----------------------------------

library(httr)

d.optin.init2 <- read.xls(fileurl, 
                         sheet = "OPT-IN LIST", 
                         perl = "/usr/bin/perl")

head(d.optin.init2, 60)
dim(d.optin.init2)


d.optin2 = d.optin.init[!(d.optin.init$BUSINESS.UNIT == "(blank)"),]  
# omit any row where business unit has '(blank)' as an entry
# due to the way that the Excel file was structured
# head(d.optin, 60)
# dim(d.optin)


# reassign column names

colnames(d.optin) <- c("FIRST.NAME", 
                       "LAST.NAME", 
                       "FACILITY", 
                       "BUSINESS.UNIT", 
                       "METRIC", 
                       "EMAIL", 
                       "MOBILE.PHONE",
                       "MOBILE.CARRIER")


head(d.optin); tail(d.optin); names(d.optin); dim(d.optin)

sessionInfo()




# another try -----------------------------------------------------------------

grepl("linux", R.Version()$os)


# THIS WORKS (from Linux) !! ##################################################
library(XLConnect) 

# url = "http://www.nseindia.com/content/fo/fii_stats_12-Oct-2012.xls" 
# local.xls.file = tempfile() 
# 
# download.file(url, local.xls.file, method='wget', extra="-U 'Mozilla/5.0 (X11; Linux) Gecko Firefox/5.0'") 
# wb = loadWorkbook(local.xls.file, create=F) 
# data = readWorksheet(wb, sheet=1)


url = "http://sharepoint2/ops/quality/metricspc/Metric OptIn List/SPC Metric Opt-In List.xlsm" 
local.xls.file = tempfile() 

download.file(url, local.xls.file, method='wget', extra = "--user longcli --password United23") 
wb = loadWorkbook(local.xls.file, create=F) 
data = readWorksheet(wb, sheet = "OPT-IN LIST")


##### END CODE ################################################################
