out_path <- "/opt/cron_jobs/pilferage_data_refresh"

log_message <- function(msg) {
  if (!file.exists(paste0(out_path,"/create_pilferage_data.log"))) {file.create(paste0(out_path,"/create_pilferage_data.log"))}
  
  write(paste0("On Date/Time (CDT): ", Sys.time()," Message: ", msg), file=paste0(out_path,"/create_pilferage_data.log"), append=TRUE)
}


log_message(msg = 'start query')
log_message(msg = 'start process original data')
