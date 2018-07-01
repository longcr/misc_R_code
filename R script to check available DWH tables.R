# to check which tables we have access to 
# run the following in R on the linux server

tab <- run_qry("cp_dwh","SELECT OWNER || '.' || TABLE_NAME FROM ALL_TABLES WHERE OWNER NOT LIKE '%SYS%'")