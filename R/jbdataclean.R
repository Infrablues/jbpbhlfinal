
#test4
#This first part is a chunk of code to import .xlsx data for which the first row
#is variable column names. It asks the user for the path
#
#' This code helps identify missing data from our table
#'
#' @param x the data being inputted
#'
#' @return the output of print, it tells you what data is missing \code\{\link{print}}
#' @export
#'
#'
jbdataclean <- function(x){


#the code will check for missing elements or NA elements
missing_by_column <- colSums(is.na(x) | x == "" | x == "ERROR" | x == "N/A")
#weirdly I could not get the above code to work if the dataset included a column with a date...

print(missing_by_column)

# Identify columns with missing elements
columns_with_missing <- names(missing_by_column[missing_by_column > 0])

if (length(columns_with_missing) > 0) {
  print("Warning: The dataset contains missing elements in the following columns:\n")
  for (col in columns_with_missing) {
    print(paste("  -", col, ": ", missing_by_column[col], "missing elements\n"))
  }
} else {
  print("No missing elements detected in the dataset.\n")
}
}
#the above code all worked with my sample dataset thus far
