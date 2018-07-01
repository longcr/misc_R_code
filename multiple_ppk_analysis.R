#----------------------------------------------
###   BEGINNING OF CODE   ###
#----------------------------------------------
# written by Cliff Long (August 16, 2013)
#----------------------------------------------


# INSTALL THE REQUIRED PACKAGES

# This code may be used to load the required packages.
# R packages do NOT need to be reinstalled each time. 

# The system may prompt the user to select an R mirror.
# Run the following three lines, but without the "#".
	# install.packages("XLConnect")
	# install.packages("SixSigma")
	# install.packages("rtf")


#----------------------------------------------
# LOAD THE REQUIRED PACKAGES
#----------------------------------------------

library(XLConnect)
	# this package is required to read data from Excel

library(rtf)
	# this package is required for the process analysis and graphics

library(SixSigma)
	# this package is required to write the results to a file


#----------------------------------------------
# READ THE DATA FROM MS EXCEL
#----------------------------------------------

# the data should start in the row after the spec and tolerance information
# BE SURE TO SET THE WORKING DIRECTORY FIRST
# the name of the data file should be "psa_data.xls"

wb = loadWorkbook("psa_data.xls")
dat = readWorksheet(wb, sheet = 1, startRow = 9, rownames = 1)
	# code used to check the data after importing ...
	# head(dat)
	# names(dat)


# read in the specs and tolerances

specs = readWorksheet(wb, sheet = 1, startRow = 2, endRow = 7, rownames = 1)
	# reads the data from Excel; requires the XLConnect package
	# code used to check the data after importing ...
		# head(specs)


#----------------------------------------------
# RUN THE ANALYSIS AND WRITE THE OUTPUT TO A MS WORD FILE
#----------------------------------------------

path = getwd() 
	# set the path in which to store the files


rtf = RTF("resultsfile.doc", width=8.5, height=11, omi=c(1, 1, 1, 1), font.size=10)
	# creates the document in which to store the results
	# the name of the file may be changed within the parentheses


# This next section has a loop that iterates across the data for each characteristic
# and writes the results to a file.

for (i in 1:length(dat[1,])) {
	x = dat[,i]
	x.Target = specs[1,i]
	x.LSL = specs[5,i]
	x.USL = specs[4,i]

	appendage <- ".png" 
	fname = names(dat)[i]	# uses the name of the characteristic from the data file
	outputFile <- file.path(path, paste0(fname, appendage)) 

	png(file = outputFile)
	ss.study.ca(x, LSL = x.LSL, USL = x.USL, Target = x.Target, f.su = fname)
	dev.off()

	addNewLine(rtf)
	addPng(rtf, file = outputFile, width=5, height=5)
	addNewLine(rtf)
	addNewLine(rtf)
	addPageBreak(rtf, width=8.5, height=11, omi=c(1, 1, 1, 1))
	}


addTable(rtf, dat)
addPageBreak(rtf, width=8.5, height=11, omi=c(1, 1, 1, 1))

addNewLine(rtf)
addSessionInfo(rtf)
	# writes information about this R session to the file 
	# to support reproducibility

done(rtf)


#----------------------------------------------
###   END OF CODE   ###
#----------------------------------------------
