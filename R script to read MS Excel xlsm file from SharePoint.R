# R read MS Excel xlsm file from SharePoint
# 2014-12-12



# method using gdata - seems to work with SharePoint --------------------------
# NOTE:  requires 'perl' installed
#
# example from
# http://r.789695.n4.nabble.com/trying-to-import-xls-or-xlsx-files-td3620580.html

library(gdata) 

fileurl = "http://sharepoint2/ops/quality/metricspc/Metric OptIn List/temporary SPC Metric Opt-In List.xlsm"


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


library(httr)
GET(fileurl, authenticate("key", "secret"))


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

##### END CODE ################################################################
