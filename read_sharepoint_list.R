# 2018-05-18
# Cliff Long
# R script to read from Essendant SharePoint list


# LOAD PACKAGES ###############################################################

require(XML)
require(RCurl)



# CREATE FUNCTION #############################################################

read_sp_list_fn <- function(fn_url){
  xml_data_fn <- xmlParse(getURL(fn_url, userpwd = 'SP_OpReporting:Essendant216!'))
  
  xml_items_fn <- getNodeSet(xml_data_fn, "//m:properties")
  
  df <- xmlToDataFrame(xml_items_fn, stringsAsFactors = FALSE)
  
  return(df)
}


# END CODE ####################################################################