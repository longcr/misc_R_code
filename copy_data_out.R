# Copy data out of R

copy_table_fn <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)  
}


# MORE

# https://www.johndcook.com/blog/r_excel_clipboard/
  
  