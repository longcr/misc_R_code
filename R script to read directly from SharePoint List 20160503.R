# read directly from SharePoint list
# updated 20160503


### example using OCRP Tracking List ##########################################
### THIS WORKS 

library(XML)


### this works !! -------------------------------------------------------------
# URL structure taken from
# http://www.dotnetmafia.com/blogs/dotnettipoftheday/archive/2010/01/21/introduction-to-querying-lists-with-rest-and-listdata-svc-in-sharepoint-2010.aspx
# make note of ListData.svc


the.URL <- "http://sharepoint2/ops/quality/audit_program_management_site/_vti_bin/ListData.svc/EndOfLineCartonInspectionShelf"



data = xmlParse(readLines(the.URL))

items = getNodeSet(data, "//m:properties")

df = xmlToDataFrame(items, stringsAsFactors = FALSE)




# test with authentication of URL ---------------------------------------------
# this WORKS from Windows and if no authentication is needed

setInternet2(NA)


library(RCurl)

getURL(the.URL, userpwd="longcli:United02") 





# another method with authentication of URL -----------------------------------
# this WORKS from Windows and if no authentication is needed


#  http://user:pass@site

the.URL2 <- "http://longcli:Un@sharepoint2/ops/quality/metricspc/_vti_bin/ListData.svc/OutOfControlResponsePlanTrackingList"

# pick one - the second one works from SharePoint
data2 = xmlParse(readLines(the.URL2))
data2 = getURL(the.URL2, userpwd = 'SP_OpReporting:Essendant216!') 

items2 = getNodeSet(data2, "//m:properties")

df2 = xmlToDataFrame(items2, stringsAsFactors = FALSE)



# another attempt -------------------------------------------------------------


library(httr)
GET("http://google.com/")





### END CODE ##################################################################
