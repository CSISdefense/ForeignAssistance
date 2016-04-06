###########################################################################
# read_FA_data FUNCTION:
# Takes a filepath to a directory containing CSVs as an argument,
# or by default will look in "FA_CSVs" directory within current working dir.
#
# Reads foreign assistance data from however many CSV files it finds
# in the specified directory and returns it merged into a single data frame.
###########################################################################

read_FA_data <- function(filepath = "./FA_CSVs"){
  origwd <- getwd()
  setwd(filepath)
  require(data.table)
  require(plyr)
  files <- list.files(pattern = "csv$")
  df <- rbind.fill(lapply(files, fixcolumn))
  setwd(origwd)
  df
}

###########################################################################
# FIXCOLUMN FUNCTION
# Used internally by read_FA_data function
# to standardize quirky column title names across various CSV files.
#
# Accomplishes that by removing non-alphanumeric characters and 
# forcing all column names to upper case.
###########################################################################

fixcolumn <- function(file) {
  dt <- fread(file)
  names(dt) <- toupper(gsub("[^[:alnum:]]", "", names(dt)))
  dt
}