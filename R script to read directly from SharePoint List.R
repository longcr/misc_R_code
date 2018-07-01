# read directly from SharePoint list
# 30MAR2015

### online example ############################################################
# from
# http://stackoverflow.com/questions/28792265/using-r-to-connect-to-a-sharepoint-list
# http://www.dotnetmafia.com/blogs/dotnettipoftheday/archive/2010/01/21/introduction-to-querying-lists-with-rest-and-listdata-svc-in-sharepoint-2010.aspx

# for installation of 'XML' package on R in Linux environment
# http://stackoverflow.com/questions/13846311/non-zero-exit-status-error-downloading-xml-and-rcurl-r-pacakges


# URL <- "http://yoursharepointserver/_vti_bin/ListData.svc/yourlist"    
# data = xmlParse(readLines(URL))
# 
# ## get the individual list items    
# items = getNodeSet(data, "//m:properties")
# 
# ## convert to a data frame
# df = xmlToDataFrame(items, stringsAsFactors = FALSE)


### example using OCRP Tracking List ##########################################
### THIS WORKS 

library(XML)

# http://sharepoint2/ops/quality/metricspc/Lists/OCRP%20Tracking%20List/AllItems.aspx

the.URL <- "http://sharepoint2/ops/quality/metricspc/_vti_bin/ListData.svc/OutOfControlResponsePlanTrackingList"

data = xmlParse(readLines(the.URL))

items = getNodeSet(data, "//m:properties")

df = xmlToDataFrame(items, stringsAsFactors = FALSE)




# test with authentication of URL ---------------------------------------------
# this WORKS from Windows and if no authentication is needed

setInternet2(NA)


library(RCurl)

getURL(the.URL, userpwd="longcli:United24") 





# another method with authentication of URL -----------------------------------
# this WORKS from Windows and if no authentication is needed


#  http://user:pass@site

the.URL2 <- "http://longcli:United24@sharepoint2/ops/quality/metricspc/_vti_bin/ListData.svc/OutOfControlResponsePlanTrackingList"

data2 = xmlParse(readLines(the.URL2))

items2 = getNodeSet(data2, "//m:properties")

df2 = xmlToDataFrame(items2, stringsAsFactors = FALSE)



# another attempt -------------------------------------------------------------


library(httr)
GET("http://google.com/")





### END CODE ##################################################################
