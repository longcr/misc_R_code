# R read MS Excel xlsm file from SharePoint
# 2014-12-12



# method using gdata - seems to work with SharePoint --------------------------
# NOTE:  requires 'perl' installed
#
# example from
# http://r.789695.n4.nabble.com/trying-to-import-xls-or-xlsx-files-td3620580.html


library(gdata) 


fileurl = "http://sharepoint2/ops/quality/metricspc/Metric OptIn List/temporary SPC Metric Opt-In List.xlsm"

d.optin.init <- read.xls(fileurl, 
                         sheet = "OPT-IN LIST", 
                         perl = "C:\\Perl64\\bin\\perl.exe")

head(d.optin.init, 60)
dim(d.optin.init)


d.optin = d.optin.init[!(d.optin.init$BUSINESS.UNIT == "(blank)"),]  
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


##### END CODE ################################################################


##### ALTERNATIVE METHODS #####################################################

# most of these do not quite work with SharePoint (yet)


# test file information (location) --------------------------------------------

#fileinfo = "http://sharepoint2/ops/quality/metricspc/Metric%20OptIn%20List/temporary%20SPC%20Metric%20Opt-In%20List.xlsm"
fileinfo = "http://sharepoint2/ops/quality/metricspc/Metric OptIn List/temporary SPC Metric Opt-In List.xlsm"
filename = "temporary SPC Metric Opt-In List.xlsm"
filepath = "http://sharepoint2/ops/quality/metricspc/Metric%20OptIn%20List/"
sheetname = "OPT-IN LIST"



# method using package 'xlsx' -------------------------------------------------

library(RCurl)
x <- getURL(fileinfo)
y <- read.xlsx(text = x, sheetName = sheetname)
res <- read.xlsx(x, 1) # read the first sheet


fileinfo = "C:\\Users\\longcli\\Dropbox\\Work USSCO\\Data for Analysis\\_06 DEPLOY Automated SPC monitor and alert\\Facility Opt-In\\temporary SPC Metric Opt-In List.xlsm"


file.exists(fileinfo)

file <- system.file(fileinfo, package = "xlsx")
res <- read.xlsx(fileinfo, sheetName = "OPT-IN LIST")




# another try using 'xlsx' ----------------------------------------------------
# http://grokbase.com/t/r/r-help/096qgtz575/r-opening-url-for-writing


conn <- url(description="https://path_to_sharepointsite/data.csv", open ="r")
dat <- read.table(file=conn, sep=",")


conn <- url(description = fileinfo, open ="r")

conn <- url(description = "http://sharepoint2/ops/quality/metricspc/Metric OptIn List/temporary SPC Metric Opt-In List.xlsm", 
            open = "r")

dat <- read.xlsx(file = conn, sheetName = "OPT-IN LIST")



# another try using package 'XLConnect' ---------------------------------------
# WORKS FROM REGULAR FOLDER PATH 

require(XLConnect)

f.path = "http://sharepoint2/ops/quality/metricspc/Metric OptIn List/temporary SPC Metric Opt-In List.xlsm"
  # the main location

f.path = "C:\\Users\\longcli\\Downloads\\temporary_SPC_Metric_Opt-In_List.xlsm"
  # testing a standard Windows folder location

file.exists(f.path)

wb = loadWorkbook(f.path)


# used to get number of rows to read by reading only first column

d.optin.init = readWorksheet(wb, sheet = "OPT-IN LIST", header = TRUE, endCol = 1)

d.optin.all = readWorksheet(wb, sheet = "OPT-IN LIST", header = TRUE, 
                          useCachedValues = TRUE, endRow = dim(optin.init)[1])


# reassign column names

colnames(d.optin.all) <- c("FIRST.NAME", 
                           "LAST.NAME", 
                           "FACILITY", 
                           "BUSINESS.UNIT", 
                           "METRIC", 
                           "EMAIL", 
                           "MOBILE.PHONE",
                           "MOBILE.CARRIER")


##### END ALTERNATIVES ########################################################
