#### LOS 24 and 48 Report ####################################################
### Retrieves all LOS for item information at manifest level #################
### from all sites > 24 to 48 hours old ######################################


### Load libraries -----------------------------------------------------------
library(sendmailR)
library(xlsx)
library(tidyverse)
library(janitor)
library(bizdays)
library(scales)


### Set constants ----------------------------------------------------------------
source("/opt/qrying/run_qry_file.R")

## ASSUMPTION: cronjob runs daily report Tuesday through Saturday (so Friday's data is emailed on Saturday)
# Input date for sql queries

#dat_date <- Sys.Date() - 23 # used for Verification of 12/04 week
dat_date <- Sys.Date() - 1 #for actual code

# Exclude holidays and weekends for bizdays time calculation -- warning will appear
holidays_essendant <- as.Date(c("2014-01-01", 
                                "2015-01-01", 
                                "2016-01-01",
                                "2017-01-01", "2017-05-29", "2017-07-04", "2017-09-04", "2017-11-23", "2017-11-24", "2017-12-22", "2017-12-25", 
                                "2018-01-01", "2018-05-28", "2018-07-04", "2018-09-03", "2018-11-22", "2018-12-25",
                                "2019-01-01"
))

cal <- Calendar(weekdays = c('sunday','saturday'),
                holidays = holidays_essendant)
#(tester_holiday <- bizdays('2017-12-25', '2017-12-26', cal))


# Set start_of_week, month, and year
start_of_week <- as.character(cut(dat_date, "week")) #always gives Monday of the week
start_of_month <- as.character(cut(dat_date, "month")) 
start_of_year <- as.character(cut(dat_date, "year"))

# File and path information
out_path <- "/opt/cron_jobs/los_report/reports"
out_filename <- paste0("LOS Manifest_Aging ", dat_date, ".xlsx")
out_file <- paste0(out_path, "/", out_filename)

# Email information
mailControl = list(smtpServer="ch2cas.ussco.com")
email_from = "noreply@essendant.com"

email_to = c("FacilityLeaders-South@essendant.com", "FacilityLeaders-WestCentral@essendant.com", 
             "FacilityLeaders-Northeast@essendant.com", 
             "InboundReceivingContacts@essendant.com", 
             "GQuaiver@essendant.com", 
             "jmikes@essendant.com", "jeasto@essendant.com", 
             "bfairchild@essendant.com", "mramsey@essendant.com", 
             "JKukoleva@essendant.com", "MSyed@essendant.com")

email_subject = "LOS Manifest Aging Report"


### Functions -----------------------------------------------------------------


log_message <- function(msg) {
  if (!file.exists(paste0(out_path,"/los_manifest_aging.log"))) {file.create(paste0(out_path,"/los_manifest_aging.log"))}
  
  write(paste0("On Date/Time (CDT): ", Sys.time()," Message: ", msg), file=paste0(out_path,"/los_manifest_aging.log"), append=TRUE)
}


percent_format <- function(nplaces = 2) {
  function(y) {
    if (length(y) == 0) return(character())
    
    y <- format(round((y*100), digits = nplaces), nsmall = nplaces)
    paste0(y, "%")
    
    #example (tester_var <- percent_format()(0)) #gives 0.00%
  }
}


metric_maker <- function(item, mfst, start_date){ 
  
  #wtd_final <- metric_maker(item = big_df_item, mfst = big_df_mfst, start_date = start_of_week)
  # mtd_final <- metric_maker(item = big_df_item, mfst = big_df_mfst, start_date = start_of_month)
  # ytd_final <- metric_maker(item = big_df_item, mfst = big_df_mfst, start_date = start_of_year)
  # item = big_df_item
  # mfst = big_df_mfst
  # start_date = dat_date
  
  df_item <- filter(item, VNDR_CLS_DT >= start_date)
  df_mfst <- filter(mfst, VNDR_CLS_DT >= start_date)
  
  if( (nrow(df_item)==0) | (nrow(df_mfst)==0) ){
    log_message(paste0("  Metric_maker: 'If' ran -- no data found for dates ", start_date, " and ", dat_date))
    
    email_to_error = c("bfairchild@essendant.com", "mramsey@essendant.com")
    #email_to_error = c("mramsey@essendant.com")
    
    email_subject = "ERROR with LOS Manifest Aging Report"
    email_body_text = paste0("Data was not subsetted back in metric_maker for the LOS Report for dates ", start_date, " and ", dat_date,
                             "! No data will appear for this date on the report.")
    email_body = list(email_body_text)
    
    tryCatch({
      sendmail(from = email_from, to = email_to_error, subject = email_subject,
               msg = email_body, control = mailControl)},
      
      error = function(cond){log_message(paste0("ERROR with sendmail in metric_maker, cond = ", cond))}
    ) #end of tryCatch
    
    return(data.frame(NULL)) #return blank dataframe
    
  } else {
    
    dat_range = range(df_item$VNDR_CLS_DT)
    log_message(paste0("  Metric_maker: filtered data by ", dat_range[1], " to ", dat_range[2]))
    
    ### Data Cleaning & Sorting -------------------------------------------------------------
    # Clean df_item
    df_item <- clean_names(df_item)
    df_item$fac_num <- as.factor(df_item$fac_num)
    
    # Clean df_manifest
    df_mfst <- clean_names(df_mfst)
    df_mfst$fac_num <- as.factor(df_mfst$fac_num)
    
    
    ### Calculate metrics  -------------------------------------------------------------
    ## DAILY
    # For df_item
    # Calculate the DIFFERENCE from manifest open date to interface date
    #    We are assuming interface date (for closing/end date) is rec_last_upd_dt
    df_item$item_aging_time <- abs(as.numeric(bizdays(df_item$mfst_opn_dt, df_item$rec_last_upd_dt, cal)))
    
    # Put aging times (differences) into buckets by 24 hrs, 48 hrs, or more than 48 hrs
    df_item$item_aging_ctgry <- ifelse(df_item$item_aging_time <= 1, "24_hrs", 
                                       ifelse(df_item$item_aging_time <= 2, "48_hrs", "48+_hrs"))
    df_item$item_aging_ctgry <- as.factor(df_item$item_aging_ctgry)
    
    # Roll-up item numbers by item aging category, facility, manifest, and vendor
    # Match formatting and numbers from John Mikes' initial data pull (12/04/17)
    df_item_rolled <-   df_item %>%
      mutate(mfst_vndr = paste0(df_item$mfst_num, "-", df_item$dcs_vndr_nm)) %>%
      group_by(fac_num, item_aging_ctgry, glb_src_cde) %>%
      count(itm_num) %>%
      summarise(cnt_itms = sum(n))
    
    # Create two tables of items -- global source code or not
    df_item_g_1 <- df_item_rolled %>% filter(glb_src_cde == "Y") %>% select(fac_num, item_aging_ctgry, cnt_itms)
    df_item_ng_1 <- df_item_rolled %>% filter(glb_src_cde == "N") %>% select(fac_num, item_aging_ctgry, cnt_itms)
    
    # Spread the aging categories -- rearrange away Brad
    df_item_g_4 <- spread(df_item_g_1, item_aging_ctgry, cnt_itms, fill = 0)
    df_item_ng_4 <- spread(df_item_ng_1, item_aging_ctgry, cnt_itms, fill = 0)
    
    # Check to make sure each category exists and create column with 0's if not.
    # for global
    if (!('24_hrs' %in% colnames(df_item_g_4))){
      #print("24 not there yo")
      
      df_item_g_4$`24_hrs` <- NA
    } 
    if (!('48_hrs' %in% colnames(df_item_g_4))){
      #print("48 not there yo")
      
      df_item_g_4$`48_hrs` <- NA
    } 
    if (!('48+_hrs' %in% colnames(df_item_g_4))){
      #print("48+ not there yo")
      
      df_item_g_4$`48+_hrs` <- NA
    } 
    
    
    # for not_global
    if (!('24_hrs' %in% colnames(df_item_ng_4))){
      #print("24 not there yo")
      
      df_item_ng_4$`24_hrs` <- NA
    } 
    if (!('48_hrs' %in% colnames(df_item_ng_4))){
      #print("48 not there yo")
      
      df_item_ng_4$`48_hrs` <- NA
    } 
    if (!('48+_hrs' %in% colnames(df_item_ng_4))){
      #print("48+ not there yo")
      
      df_item_ng_4$`48+_hrs` <- NA
    } 
    
    # For df_mfst
    # Calculate the DIFFERENCE from manifest open date to vendor close date
    # note Jon look at this weird negative value that we correct with abs...
    # e.g. 12/01 to 12/01 = 0, 12/01 to 12/02 = 1
    # mfst_opn_dt will always be populated; 
    # vndr_opn_dt may not always be populated, but equals mfst_opn_dt
    df_mfst$mfst_aging_time <- abs(as.numeric(bizdays(df_mfst$mfst_opn_dt, df_mfst$vndr_cls_dt, cal)))
    
    # Put aging times (differences) into buckets by 24 hrs, 48 hrs, or more than 48 hrs
    # NOT including weekends
    # e.g. Within 24 hrs is a 0 or 1 (12/01 to 12/01 OR 12/01 to 12/02)
    # e.g. Within 48 hrs is a 2 (12/01 to 12/03) 
    # e.g. Over 48 hrs is a >=3 (12/01 to 12/04 or later) 
    df_mfst$mfst_aging_ctgry <- ifelse(df_mfst$mfst_aging_time <= 1, "24_hrs", 
                                       ifelse(df_mfst$mfst_aging_time <= 2, "48_hrs", "48+_hrs"))
    df_mfst$mfst_aging_ctgry <- as.factor(df_mfst$mfst_aging_ctgry)
    
    # Create a unique value per row, which manifest number and vendor name
    df_mfst$mfst_vndr <- paste0(df_mfst$mfst_num, "-", df_mfst$dcs_vndr_nm)
    
    # Create two tables of manifests -- global source code or not
    df_mfst_g_1 <- filter(df_mfst, glb_src_cde == "Y")
    df_mfst_ng_1 <- filter(df_mfst, glb_src_cde == "N")
    
    # Count & sum the count of instances of mfst# and vendor occurring in each manifest table
    df_mfst_g_2 <- df_mfst_g_1 %>% group_by(fac_num, mfst_aging_ctgry) %>% count(mfst_vndr) %>% summarise(cnt_vndr_mfsts = sum(n))
    df_mfst_ng_2 <- df_mfst_ng_1 %>% group_by(fac_num, mfst_aging_ctgry) %>% count(mfst_vndr) %>% summarise(cnt_vndr_mfsts = sum(n))
    
    # Spread the aging categories -- rearrange away Brad
    df_mfst_g_4 <- spread(df_mfst_g_2, mfst_aging_ctgry, cnt_vndr_mfsts, fill = 0)
    df_mfst_ng_4 <- spread(df_mfst_ng_2, mfst_aging_ctgry, cnt_vndr_mfsts, fill = 0)
    
    # Check to make sure each category exists and create column with 0's if not.
    # for global
    if (!('24_hrs' %in% colnames(df_mfst_g_4))){
      #print("24 not there yo")
      
      df_mfst_g_4$`24_hrs` <- NA
    } 
    if (!('48_hrs' %in% colnames(df_mfst_g_4))){
      #print("48 not there yo")
      
      df_mfst_g_4$`48_hrs` <- NA
    } 
    if (!('48+_hrs' %in% colnames(df_mfst_g_4))){
      #print("48+ not there yo")
      
      df_mfst_g_4$`48+_hrs` <- NA
    } 
    
    
    # for not_global
    if (!('24_hrs' %in% colnames(df_mfst_ng_4))){
      #print("24 not there yo")
      
      df_mfst_ng_4$`24_hrs` <- NA
    } 
    if (!('48_hrs' %in% colnames(df_mfst_ng_4))){
      #print("48 not there yo")
      
      df_mfst_ng_4$`48_hrs` <- NA
    } 
    if (!('48+_hrs' %in% colnames(df_mfst_ng_4))){
      #print("48+ not there yo")
      
      df_mfst_ng_4$`48+_hrs` <- NA
    } 
    
    
    # Outer join global and non-global for item and manifest: merge(x = df1, y = df2, by = "CustomerId", all = TRUE)
    df_item_final <- merge(x = df_item_ng_4, y = df_item_g_4, by ="fac_num", all = TRUE, suffixes = c("_not_global", "_global"))
    df_mfst_final <- merge(x = df_mfst_ng_4, y = df_mfst_g_4, by ="fac_num", all = TRUE, suffixes = c("_not_global", "_global"))
    
    # Change na's to 0's in each table
    df_item_final[is.na(df_item_final)] <- 0
    df_mfst_final[is.na(df_mfst_final)] <- 0
    
    # Change these columns from characters to doubles to percentages so we can sum
    df_item_final[2:ncol(df_item_final)] <- lapply(df_item_final[2:ncol(df_item_final)], as.numeric) 
    df_mfst_final[2:ncol(df_item_final)] <- lapply(df_mfst_final[2:ncol(df_mfst_final)], as.numeric) 
    
    # Adding Column Totals to the bottom Row (May need to change data types)  
    # for item  
    item_totals <- df_item_final %>% 
      summarize(`24_hrs_not_global` = sum(df_item_final$`24_hrs_not_global`), 
                `48_hrs_not_global` = sum(df_item_final$`48_hrs_not_global`), 
                `48+_hrs_not_global` = sum(df_item_final$`48+_hrs_not_global`), 
                `24_hrs_global` = sum(df_item_final$`24_hrs_global`), 
                `48_hrs_global` = sum(df_item_final$`48_hrs_global`), 
                `48+_hrs_global` = sum(df_item_final$`48+_hrs_global`) 
      ) %>%
      mutate(fac_num = "Totals")
    
    df_item_final <- union_all(df_item_final, item_totals)
    
    # for manifest 
    mfst_totals <- df_mfst_final %>% 
      summarize(`24_hrs_not_global` = sum(df_mfst_final$`24_hrs_not_global`), 
                `48_hrs_not_global` = sum(df_mfst_final$`48_hrs_not_global`), 
                `48+_hrs_not_global` = sum(df_mfst_final$`48+_hrs_not_global`), 
                `24_hrs_global` = sum(df_mfst_final$`24_hrs_global`), 
                `48_hrs_global` = sum(df_mfst_final$`48_hrs_global`), 
                `48+_hrs_global` = sum(df_mfst_final$`48+_hrs_global`) 
      ) %>%
      mutate(fac_num = "Totals")
    
    df_mfst_final <- union_all(df_mfst_final, mfst_totals)
    
    # Calculate percentages
    ## To sum each ROW and not the column, use a + and don't use sum function
    # for item_final
    df_item_final$`24_pct_not_global` <- percent_format()(df_item_final$`24_hrs_not_global`/(df_item_final$`24_hrs_not_global` + df_item_final$`48_hrs_not_global` + df_item_final$`48+_hrs_not_global`))
    df_item_final$`48_pct_not_global` <- percent_format()(df_item_final$`48_hrs_not_global`/(df_item_final$`24_hrs_not_global` + df_item_final$`48_hrs_not_global` + df_item_final$`48+_hrs_not_global`))
    df_item_final$`48+_pct_not_global` <- percent_format()(df_item_final$`48+_hrs_not_global`/(df_item_final$`24_hrs_not_global` + df_item_final$`48_hrs_not_global` + df_item_final$`48+_hrs_not_global`))
    
    df_item_final$`24_pct_global` <- percent_format()(df_item_final$`24_hrs_global`/(df_item_final$`24_hrs_global` + df_item_final$`48_hrs_global` + df_item_final$`48+_hrs_global`))
    df_item_final$`48_pct_global` <- percent_format()(df_item_final$`48_hrs_global`/(df_item_final$`24_hrs_global` + df_item_final$`48_hrs_global` + df_item_final$`48+_hrs_global`))
    df_item_final$`48+_pct_global` <- percent_format()(df_item_final$`48+_hrs_global`/(df_item_final$`24_hrs_global` + df_item_final$`48_hrs_global` + df_item_final$`48+_hrs_global`))
    
    # for manifest_final
    df_mfst_final$`24_pct_not_global` <- percent_format()(df_mfst_final$`24_hrs_not_global`/(df_mfst_final$`24_hrs_not_global` + df_mfst_final$`48_hrs_not_global` + df_mfst_final$`48+_hrs_not_global`))
    df_mfst_final$`48_pct_not_global` <- percent_format()(df_mfst_final$`48_hrs_not_global`/(df_mfst_final$`24_hrs_not_global` + df_mfst_final$`48_hrs_not_global` + df_mfst_final$`48+_hrs_not_global`))
    df_mfst_final$`48+_pct_not_global` <- percent_format()(df_mfst_final$`48+_hrs_not_global`/(df_mfst_final$`24_hrs_not_global` + df_mfst_final$`48_hrs_not_global` + df_mfst_final$`48+_hrs_not_global`))
    
    df_mfst_final$`24_pct_global` <- percent_format()(df_mfst_final$`24_hrs_global`/(df_mfst_final$`24_hrs_global` + df_mfst_final$`48_hrs_global` + df_mfst_final$`48+_hrs_global`))
    df_mfst_final$`48_pct_global` <- percent_format()(df_mfst_final$`48_hrs_global`/(df_mfst_final$`24_hrs_global` + df_mfst_final$`48_hrs_global` + df_mfst_final$`48+_hrs_global`))
    df_mfst_final$`48+_pct_global` <- percent_format()(df_mfst_final$`48+_hrs_global`/(df_mfst_final$`24_hrs_global` + df_mfst_final$`48_hrs_global` + df_mfst_final$`48+_hrs_global`))
    
    
    # If there are NA%'s (because 0/0 equals NaN%) then replace as 0%
    # for item
    df_item_final$`24_pct_global` <- ifelse(grepl("NaN%", df_item_final$`24_pct_global`), "0.00%", df_item_final$`24_pct_global`)
    df_item_final$`48_pct_global` <- ifelse(grepl("NaN%", df_item_final$`48_pct_global`), "0.00%", df_item_final$`48_pct_global`)
    df_item_final$`48+_pct_global` <- ifelse(grepl("NaN%", df_item_final$`48+_pct_global`), "0.00%", df_item_final$`48+_pct_global`)
    
    df_item_final$`24_pct_not_global` <- ifelse(grepl("NaN%", df_item_final$`24_pct_not_global`), "0.00%", df_item_final$`24_pct_not_global`)
    df_item_final$`48_pct_not_global` <- ifelse(grepl("NaN%", df_item_final$`48_pct_not_global`), "0.00%", df_item_final$`48_pct_not_global`)
    df_item_final$`48+_pct_not_global` <- ifelse(grepl("NaN%", df_item_final$`48+_pct_not_global`), "0.00%", df_item_final$`48+_pct_not_global`)
    
    # for manifest
    df_mfst_final$`24_pct_global` <- ifelse(grepl("NaN%", df_mfst_final$`24_pct_global`), "0.00%", df_mfst_final$`24_pct_global`)
    df_mfst_final$`48_pct_global` <- ifelse(grepl("NaN%", df_mfst_final$`48_pct_global`), "0.00%", df_mfst_final$`48_pct_global`)
    df_mfst_final$`48+_pct_global` <- ifelse(grepl("NaN%", df_mfst_final$`48+_pct_global`), "0.00%", df_mfst_final$`48+_pct_global`)
    
    df_mfst_final$`24_pct_not_global` <- ifelse(grepl("NaN%", df_mfst_final$`24_pct_not_global`), "0.00%", df_mfst_final$`24_pct_not_global`)
    df_mfst_final$`48_pct_not_global` <- ifelse(grepl("NaN%", df_mfst_final$`48_pct_not_global`), "0.00%", df_mfst_final$`48_pct_not_global`)
    df_mfst_final$`48+_pct_not_global` <- ifelse(grepl("NaN%", df_mfst_final$`48+_pct_not_global`), "0.00%", df_mfst_final$`48+_pct_not_global`)
    
    # Rearrange columns so percentages are next to appropriate column 
    # e.g. 24 then 24%, 48 then 48%
    df_item_final <- select(df_item_final, fac_num, `24_hrs_not_global`, `24_pct_not_global`, `48_hrs_not_global`, `48_pct_not_global`, `48+_hrs_not_global`, `48+_pct_not_global`,
                            `24_hrs_global`, `24_pct_global`, `48_hrs_global`, `48_pct_global`, `48+_hrs_global`, `48+_pct_global`)
    
    df_mfst_final <- select(df_mfst_final, fac_num, `24_hrs_not_global`, `24_pct_not_global`, `48_hrs_not_global`, `48_pct_not_global`, `48+_hrs_not_global`, `48+_pct_not_global`,
                            `24_hrs_global`, `24_pct_global`, `48_hrs_global`, `48_pct_global`, `48+_hrs_global`, `48+_pct_global`)
    
    # Combine item and manifest df's into final df (prepare for export)
    df_final <- merge(x = df_item_final, y = df_mfst_final, by ="fac_num", all = TRUE, fill = 0, suffixes = c("_item", "_manifest"))
    
    return(df_final)
  } #end of if/else
  
} #RETURN df_final for metric_maker


worksheet_maker <- function(df, sheet_name){
  # Define some cell styles within that workbook
  
  log_message(paste0("  START Worksheet_maker "))
  
  x = as.data.frame(df)
  sheet <- createSheet(wb, sheetName = sheet_name)
  
  cs_title <- CellStyle(wb) + 
    Font(wb, heightInPoints=14, isBold=TRUE) +
    Fill(backgroundColor="#41B6E6", foregroundColor="#41B6E6", pattern="SOLID_FOREGROUND") + 
    Alignment(h="ALIGN_CENTER")
  
  cs_subtitle <- CellStyle(wb) + 
    Font(wb, heightInPoints=12, isItalic=TRUE, isBold=FALSE) +
    Alignment(h="ALIGN_CENTER")
  
  cs_cell3_col_names <-  CellStyle(wb) + 
    Font(wb, isBold=TRUE) + 
    Fill(backgroundColor="yellow", foregroundColor="yellow", pattern="SOLID_FOREGROUND") + 
    Alignment(h="ALIGN_CENTER") +
    Border(color="black", position=c("LEFT", "RIGHT"), 
           pen=c("BORDER_THIN", "BORDER_THIN"))
  
  cs_cell4_col_names <-  CellStyle(wb) + Font(wb, isBold=TRUE) + 
    Alignment(h="ALIGN_CENTER") +
    Border(color="black", position=c("LEFT", "RIGHT"), 
           pen=c("BORDER_THIN", "BORDER_THIN"))
  
  cs_cell4_col_names_g <-  CellStyle(wb) + Font(wb, isBold=TRUE) + 
    Fill(backgroundColor="lightgray", foregroundColor="lightgray", pattern="SOLID_FOREGROUND") + 
    Alignment(h="ALIGN_CENTER") +
    Border(color="black", position=c("LEFT", "RIGHT"), 
           pen=c("BORDER_THIN", "BORDER_THIN"))
  
  cs_table <-  CellStyle(wb) + Font(wb, isBold=FALSE) + 
    DataFormat("#,##0")
  
  # cs_table_row_names <-  CellStyle(wb) + Font(wb, isBold=TRUE) +
  #   Border(color="black", position=c("RIGHT"),  #, "LEFT", "RIGHT"
  #          pen=c("BORDER_THICK")) 
  
  cs_table_col_names <-  CellStyle(wb) + Font(wb, isBold=TRUE) + 
    Alignment(h="ALIGN_CENTER") +
    Border(color="black", position=c("TOP", "BOTTOM"),  #, "LEFT", "RIGHT"
           pen=c("BORDER_THICK", "BORDER_THICK")) #, "BORDER_THICK", "BORDER_THICK"
  
  cs_table_col_bdr <-  CellStyle(wb) +
    Border(color="black", position=c("RIGHT"),
           pen=c("BORDER_THIN"))
  
  
  # Input some text into specific cells in Excel
  rows <- createRow(sheet, rowIndex = 1)
  sheet_title <- createCell(rows, colIndex = 1)
  setCellValue(sheet_title[[1,1]], "Level of Service (LOS) Reporting")
  setCellStyle(sheet_title[[1,1]], cs_title)
  addMergedRegion(sheet, startRow = 1, endRow = 1, startColumn = 1, endColumn = 25)
  
  rows <- createRow(sheet, rowIndex = 2)
  sheet_subtitle <- createCell(rows, colIndex = 1)
  setCellValue(sheet_subtitle[[1,1]], "'24' = Open date Plus 1 Day    '48' = Open date Plus 2 Days")
  setCellStyle(sheet_subtitle[[1,1]], cs_subtitle) 
  addMergedRegion(sheet, startRow = 2, endRow = 2, startColumn = 1, endColumn = 25)
  
  # Add Merged Area Item Level
  row3 <- createRow(sheet, 3)
  cells3 <- createCell(row3, colIndex = 1:13)
  setCellValue(cells3[[1,2]], "Item Manifest Level Aging")
  setCellStyle(cells3[[1,2]], cs_cell3_col_names)
  addMergedRegion(sheet, startRow = 3, endRow = 3, startColumn = 2, endColumn = 13)
  
  # Add Merged Area Manifest Level
  cells3_b <- createCell(row3, colIndex = 14:25)
  setCellValue(cells3_b[[1:1]], "Vendor Manifest Level Aging")
  setCellStyle(cells3_b[[1:1]], cs_cell3_col_names)
  addMergedRegion(sheet, startRow = 3, endRow = 3, startColumn = 14, endColumn = 25)
  
  # Add Current Day Area - row 4
  cell4_b_title_nonglobal <- paste0("Non-Global: Manifest Closed As of Day ", dat_date)
  cell4_b_title_global <- paste0("Global: Manifest Closed As of Day ", dat_date)
  
  row4 <- createRow(sheet, 4)
  cells4 <- createCell(row4, colIndex = 1:13)
  cells4_b <- createCell(row4, colIndex = 14:25)
  
  setCellValue(cells4[[1,2]], cell4_b_title_nonglobal)
  setCellStyle(cells4[[1,2]], cs_cell4_col_names)
  addMergedRegion(sheet, startRow = 4, endRow = 4, startColumn = 2, endColumn = 7)
  
  setCellValue(cells4[[1,8]], cell4_b_title_global)
  setCellStyle(cells4[[1,8]], cs_cell4_col_names_g)
  addMergedRegion(sheet, startRow = 4, endRow = 4, startColumn = 8, endColumn = 13)
  
  setCellValue(cells4_b[[1,1]], cell4_b_title_nonglobal)
  setCellStyle(cells4_b[[1,1]], cs_cell4_col_names)
  addMergedRegion(sheet, startRow = 4, endRow = 4, startColumn = 14, endColumn = 19)
  
  setCellValue(cells4_b[[1,7]], cell4_b_title_global)
  setCellStyle(cells4_b[[1,7]], cs_cell4_col_names_g)
  addMergedRegion(sheet, startRow = 4, endRow = 4, startColumn = 20, endColumn = 25)
  
  if( (nrow(x)==0) | (nrow(x)==0) ){
    row5 <- createRow(sheet, rowIndex = 5)
    cells_fac_num <- createCell(row5, colIndex = 1)
    
    setCellValue(cells_fac_num[[1,1]], "No data.")
    
  }
  else{
    # Input dataframe of final data (x) into excel
    addDataFrame(x = x, sheet = sheet, row.names = FALSE, startRow = 5,
                 # col.names = TRUE, colnamesStyle = cs_table_col_names,
                 startColumn = 1, colStyle = list(`2` = cs_table, 
                                                  `4` = cs_table,
                                                  `6` = cs_table,
                                                  `8` = cs_table,
                                                  `10` = cs_table,
                                                  `12` = cs_table,
                                                  `14` = cs_table,
                                                  `16` = cs_table,
                                                  `18` = cs_table,
                                                  `20` = cs_table,
                                                  `22` = cs_table,
                                                  `24` = cs_table))
    
    # Rename Data Frame Title Cells
    row5 <- createRow(sheet, rowIndex = 5)
    cells_column_titles <- createCell(row5, colIndex = 2:25)
    cells_fac_num <- createCell(row5, colIndex = 1)
    
    setCellValue(cells_fac_num[[1,1]], "Fac Num")
    
    setCellValue(cells_column_titles[[1,1]], "24 hrs")
    setCellValue(cells_column_titles[[1,7]], "24 hrs")
    setCellValue(cells_column_titles[[1,13]], "24 hrs")
    setCellValue(cells_column_titles[[1,19]], "24 hrs")
    
    setCellValue(cells_column_titles[[1,2]], "24 %")
    setCellValue(cells_column_titles[[1,8]], "24 %")
    setCellValue(cells_column_titles[[1,14]], "24 %")
    setCellValue(cells_column_titles[[1,20]], "24 %")
    
    setCellValue(cells_column_titles[[1,3]], "48 hrs")
    setCellValue(cells_column_titles[[1,9]], "48 hrs")
    setCellValue(cells_column_titles[[1,15]], "48 hrs")
    setCellValue(cells_column_titles[[1,21]], "48 hrs")
    
    setCellValue(cells_column_titles[[1,4]], "48 %")
    setCellValue(cells_column_titles[[1,10]], "48 %")
    setCellValue(cells_column_titles[[1,16]], "48 %")
    setCellValue(cells_column_titles[[1,22]], "48 %")
    
    setCellValue(cells_column_titles[[1,5]], "48+ hrs")
    setCellValue(cells_column_titles[[1,11]], "48+ hrs")
    setCellValue(cells_column_titles[[1,17]], "48+ hrs")
    setCellValue(cells_column_titles[[1,23]], "48+ hrs")
    
    setCellValue(cells_column_titles[[1,6]], "48+ %")
    setCellValue(cells_column_titles[[1,12]], "48+ %")
    setCellValue(cells_column_titles[[1,18]], "48+ %")
    setCellValue(cells_column_titles[[1,24]], "48+ %")
    
    
    # Add Harry Stylez
    for (i in 1:24){
      setCellStyle(cells_column_titles[[1,i]], cs_table_col_names)
    }
    
  }
  
  
  # xlsx package https://cran.r-project.org/web/packages/xlsx/xlsx.pdf 
  # https://tradeblotter.wordpress.com/2013/05/02/writing-from-r-to-excel-with-xlsx/
  # examples/questions https://www.r-bloggers.com/using-the-xlsx-package-to-create-an-excel-file/
  # solutions https://www.r-exercises.com/2017/06/17/using-the-xlsx-package-to-create-an-excel-file-solutions/ 
  # https://github.com/dragua/xlsx/blob/master/R/Cell.R 
  
  #createFreezePane(sheet, 1, startColumn = 1)
  
  log_message(paste0("  END Worksheet_maker"))
  
} #END OF WORKSHEET_MAKER FUNCTION -- adds worksheets to workbook, but does not return anything



### Retrieve and export data -----------------------------------------------------
log_message(paste0("Start file run (about to get data -- constants and functions are defined).")) 

# available_conns()  # Test connections/dwhs available 
# qry_item <- "select * from DWHDBA.DM_VNDR_DIM" # Test qry

# how John Mikes pulled ITEM level DAY of 12/04/17 was Monday only here AL3.VNDR_CLS_DT= DATE'2017-12-04' ; manifest level WEEK 12/04/17 through 12/08/17 

# Queries set to YTD
qry_item <- paste0("SELECT AL3.FAC_NUM, AL3.MFST_NUM, AL3.DCS_VNDR_NM, AL3.VNDR_OPN_DT, case when AL3.DCS_VNDR_NM = 'IRTRANS'  then 'IR' else 'NON IR' end AS IR_indicator, AL3.VNDR_STAT_CDE, AL5.REC_LAST_UPD_DT, AL5.ITM_NUM, AL4.MFST_OPN_DT, AL3.VNDR_CLS_DT, AL4.SHP_TY_CDE, AL3.VNDR_MSG_CDE, AL6.GLB_SRC_CDE  FROM OPSDBA.STG_IB_MFST_VNDR AL3, OPSDBA.STG_IB_MFST AL4, OPSDBA.STG_IB_REC_ITM AL5, DWHDBA.DM_VNDR_DIM AL6 WHERE (AL4.FAC_NUM=AL3.FAC_NUM AND AL4.MFST_CR_DT=AL3.MFST_CR_DT AND AL4.MFST_NUM=AL3.MFST_NUM AND AL3.FAC_NUM=AL5.FAC_NUM AND AL3.MFST_CR_DT=AL5.MFST_CR_DT AND AL3.MFST_NUM=AL5.MFST_NUM AND AL3.VNDR_NUM=AL5.VNDR_NUM AND AL3.VNDR_NUM=AL6.VNDR_NUM)  AND ((AL3.DCS_VNDR_NM NOT  IN ('IRTRANS', 'RETURN') AND AL3.VNDR_CLS_DT>= DATE'", start_of_year, "' AND AL3.VNDR_CLS_DT<= DATE'", dat_date, "' AND AL5.REC_TOT_LN_QTY>0))")
qry_mfst <- paste0("SELECT AL3.FAC_NUM, AL3.MFST_NUM, AL3.DCS_VNDR_NM, AL4.MFST_OPN_DT, AL3.VNDR_OPN_DT, AL3.VNDR_CLS_DT,  case when AL3.DCS_VNDR_NM = 'IRTRANS'   then 'TR' else  'REG' end AS IR_indicator, AL4.SHP_TY_CDE, AL5.GLB_SRC_CDE FROM OPSDBA.STG_IB_MFST_VNDR AL3, OPSDBA.STG_IB_MFST AL4, DWHDBA.DM_VNDR_DIM AL5 WHERE (AL4.FAC_NUM=AL3.FAC_NUM AND AL4.MFST_CR_DT=AL3.MFST_CR_DT AND AL4.MFST_NUM=AL3.MFST_NUM AND AL3.VNDR_NUM=AL5.VNDR_NUM)  AND ((AL3.DCS_VNDR_NM NOT  IN ('IRTRANS', 'RETURN') AND AL3.VNDR_CLS_DT>= DATE'", start_of_year, "' AND AL3.VNDR_CLS_DT<= DATE'", dat_date, "'))")

log_message(paste0(" START Running big_df queries FOR HODOR.")) 

# Run Queries from CP DWH
big_df_item <- run_qry("cp_dwh", qry_item)

log_message(paste0(" big_df_item query finished FOR HODOR.")) 

big_df_mfst <- run_qry("cp_dwh", qry_mfst)

log_message(paste0(" END Running big_df queries FOR HODOR.")) 

# Make sure the original (big) dataframes have data
if( (nrow(big_df_item)==0) | (nrow(big_df_mfst)==0) ){
  email_to_error = c("bfairchild@essendant.com", "mramsey@essendant.com")
  
  email_subject = "ERROR with LOS Manifest Aging Report"
  email_body_text = paste0("Data was not pulled back for the Yearly LOS Report for dat_date = ", start_of_year, dat_date,
                           "! Please fix it now and manually run to send to field!")
  email_body = list(email_body_text)
  
  tryCatch({
    sendmail(from = email_from, to = email_to_error, subject = email_subject,
             msg = email_body, control = mailControl)},
    
    error = function(cond){log_message(paste0("ERROR with sendmail in if for big_df, cond = ", cond))}
  ) #end of tryCatch
} else {
  
  log_message(paste0("  Else ran after big data (starting subsets of data with metric_maker)."))
  
  ### Filter data and create metrics in dataframes based on time frame desired ---------------------------------------------------
  daily_final <- metric_maker(item = big_df_item, mfst = big_df_mfst, start_date = dat_date)
  wtd_final <- metric_maker(item = big_df_item, mfst = big_df_mfst, start_date = start_of_week)
  mtd_final <- metric_maker(item = big_df_item, mfst = big_df_mfst, start_date = start_of_month)
  ytd_final <- metric_maker(item = big_df_item, mfst = big_df_mfst, start_date = start_of_year)
  
  
  ### Create output file in xlsx -------------------------------------------------------
  wb <- createWorkbook(type = "xlsx")
  
  # DAILY: convert final dataframe into dataframe (sanity check) to export to excel
  sheet_name = paste0(dat_date, " DAILY LOS Report")
  worksheet_maker(daily_final, sheet_name)
  
  # WEEKLY: convert final dataframe into dataframe (sanity check) to export to excel
  sheet_name = paste0("WTD (Since Monday) LOS Report")
  worksheet_maker(wtd_final, sheet_name)
  
  # MONTHLY: convert final dataframe into dataframe (sanity check) to export to excel
  sheet_name = paste0("MTD LOS Report")
  worksheet_maker(mtd_final, sheet_name)
  
  # YEARLY: convert final dataframe into dataframe (sanity check) to export to excel
  sheet_name = paste0("YTD LOS Report")
  worksheet_maker(ytd_final, sheet_name)
  
  saveWorkbook(wb, out_file)
  
  
  
  ### Send email with xlsx attachment --------------------------------------------------------------
  email_body_text = paste0("Please see the attached Level of Service (LOS) Manifest Aging Report for all manifests closed by end of day yesterday, \n \t ", dat_date, ". \n",
                           "If you have any questions or concerns about the data, please contact Bradley Fairchild and/or Megan Ramsey. ",
                           "\n \n",
                           "Overview & Assumptions of Data: \n",
                           "You will receive this report via email each Monday through Saturday morning with the previous day's data. \n",
                           "\t Since no manifests close on Saturday, you will not receive a report on Sunday. \n",
                           "All of the manifests included in the data had a maximum close date of yesterday. \n",
                           "There are four tabs in the spreadsheet that each have a different time period of manifest closing dates: ",
                           "\n \t - daily (yesterday only) \n \t - isoweek-to-date (Monday through yesterday) \n \t - month-to-date (First of Month through yesterday) \n \t - year-to-date (First of Year through yesterday) \n",
                           "E.g. If you receive an email on Wednesday, then \n \t - daily data tab will include any manifest closed on Tuesday (yesterday)", 
                           " \n \t - WTD data tab will include any manifest closed on Monday through Tuesday (of that week)", 
                           " \n \t - MTD data tab will include any manifest closed on 1st of Month through Tuesday", 
                           " \n \t - YTD data tab will include any manifest closed on 1st of Year through Tuesday. \n",
                           "If you see 'No data.' on a spreadsheet tab, then no manifests were closed yesterday or from the start of time period through yesterday.",
                           " \n \n",
                           "Calculations: \n",
                           "Each manifest and line item on a manifest is categorized into one of three manifest aging categories: \n \t - 24 hours \n \t - 48 hours \n \t - 48+ hours", "\n",
                           "Weekends and Essendant holidays are NOT counted when calculating manifest aging category. \n",
                           "E.g. If the manifest opened on Friday and closed on Monday, then it was closed within 24 hours (because we don't count weekends). \n \n",
                           "Definitions: \n",
                           "Item Manifest Level Aging:   The count of all manifest line items (Item# line level) summarized in either the 24, 48, or 48+ hour category. \n",
                           "Vendor Manifest Level Aging:   The count of all manifests (Manifest# level) summarized in either the 24, 48, or 48+ hour category. \n",
                           "Non-Global:   Manifests closed from vendors within the continental U.S. \n",
                           "Global:   Manifests closed from vendors outside of the continental U.S. \n",
                           "The manifest aging category is calculated based on the number of days from manifest open date to close date (this is not based on hour of day). \n",
                           " "
  )
  
  email_attach = mime_part(x = out_file, name = out_filename)
  email_body = list(email_body_text, email_attach)
  
  tryCatch({
    sendmail(from = email_from, to = email_to, subject = email_subject, 
             msg = email_body, control = mailControl)},
    
    error = function(cond){log_message(paste0("ERROR with sending final email, cond = ", cond))}
  )
  
  log_message(paste0("End of ELSE"))
  
  
} # End of ELSE -- No Data for MTD qry

log_message(paste0("End file run (email has been sent)."))

### Create a graph -------------------------------------------------
## Gather (opposite spread) df
# df_item_4_plot <- gather(data = df_final, 
#                           key = `24_hrs_not_global_item`
#                           #key = c(`24_pct_not_global`, `48_pct_not_global`, `48+_pct_not_global`, `24_pct_global`, `48_pct_global`, `48+_pct_global`)
#                           )
# 
#   plot_item_day <- ggplot(data = df_final, 
#                          aes(x = fac_num, y = tot_amt, fill = disp_type)) + #, fill = disp_type or , fill = alpha_pfx
#   geom_bar(stat = "identity") +
#   geom_text(aes(label = comma(tot_amt)), position = position_stack(0.5), size = 5) + #position = position_dodge(width=25) #vjust = 0.5, 
#   labs(x = 'Time', y = 'Dollars or Quantity') + 
#   facet_grid(facets = alpha_pfx ~ ., scales = "free_y") + #
#   scale_x_date(date_breaks = "1 month", position = "top") + #scale_x_date(date_breaks = "1 month", position = "top") +
#   scale_y_continuous(labels = comma_format()) + #labels = scales::dollar
#   scale_fill_brewer(palette = "Accent", name = "Disposition Types") +
#   theme(legend.position="top", legend.direction="horizontal") +
#   theme(axis.title = element_text(size = 16), legend.title = element_text(size = 16)) +
#   theme(axis.text.x = element_text(size = 14, angle = 90), 
#         axis.text.y = element_text(size = 14), legend.text = element_text(size = 14)) +
#   theme(strip.text = element_text(size = 14)) + #, panel.spacing = unit(1, "lines")
#   theme(plot.background=element_rect(fill = "darkseagreen"))
# 
# return(plot_final_day)

# plot_gl = ggplot(datGL_plot_final, aes(x = dateymd, y = doc_amt, color = gen_led_acct_nm)) + #, label = doc_amt
#   geom_line() + #aes(fill=factor(gen_led_acct_nm))
#   geom_point(aes(x = dateymd, y = doc_amt), size = 2) + #previously nothing in ()
#   geom_hline(yintercept = 0, size = 1) +
#   geom_text_repel(aes(dateymd, doc_amt, label = scales::dollar(doc_amt)),
#                   box.padding = unit(0.45, "lines"), color = "black", size = 6) +
#   #geom_text(aes(label = doc_amt), vjust=-1, size = 4, color = "black") +
#   theme(legend.position="top", legend.direction="horizontal", legend.title = element_blank()) +
#   scale_x_date(date_breaks = "1 months", position = "top") +
#   guides(fill=guide_legend(nrow=2)) +
#   scale_y_continuous(labels = scales::dollar) +
#   labs(x = 'Time', y = 'Dollars') +
#   scale_color_manual(name = "GL Account", 
#                      values = c("Distressed Contra" = "red",
#                                 "Distressed Inventory" = "#56B4E9",
#                                 "Distressed Recovery" = "#F0E442", #yellow is #F0E442 #dark gold is goldenrod3
#                                 "Freight On Distresse" = "cyan",
#                                 "Vendor Debit Recover" = "#009E73", # bluish green
#                                 "TOTAL" = "magenta")) +
#   #scale_fill_brewer(palette = "Set3") + #, name = "EOR Categories/GL Accounts"
#   theme(axis.text.x = element_text(size = 14, angle = 90), #, angle = 90
#         axis.text.y = element_text(size = 14), legend.text = element_text(size = 14)) +
#   theme(strip.text = element_text(size = 14)) + 
#   theme(axis.title = element_text(size = 18), legend.title = element_text(size = 16))
# 




### Eat some pancakes -- THE END