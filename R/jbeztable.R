# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#Midterm scratch code

#

library(flextable)
library(dplyr)
library(readxl)
library(devtools)
library(utils)

#This first part is a chunk of code to import .xlsx data for which the first row
#is variable column names. It asks the user for the path

excel_file <- "C:\\Users\\baril\\OneDrive\\Documents\\2023-2024\\Research\\Masters\\Biostats Computing\\Midterm\\Scrub Variables Test File.xlsx"
#to prompt the user for the file path, will need to use a different code
original_data <- readxl::read_excel(excel_file, sheet = 1, col_names = TRUE)

summarize(original_data)


#next, the code will check for missing elements or NA elements
missing_by_column <- colSums(is.na(original_data) | original_data == "" | original_data == "ERROR" | original_data == "N/A")
#weirdly I could not get the above code to work if the dataset included a column with a date...

print(missing_by_column)

# Identify columns with missing elements
columns_with_missing <- names(missing_by_column[missing_by_column > 0])

if (length(columns_with_missing) > 0) {
  cat("Warning: The dataset contains missing elements in the following columns:\n")
  for (col in columns_with_missing) {
    cat(paste("  -", col, ": ", missing_by_column[col], "missing elements\n"))
  }
} else {
  cat("No missing elements detected in the dataset.\n")
}

#the above code all worked with my sample dataset thus far

#here I would include some code to clean up the missing data if needed, but I
#think for now I will skip that

#next the code will identify what type of variable each one is and its name,
#and check with the user if that is correct(? not sure how to do that yet)

#one way to do this is to look at numbers and detect if they are whole numbers
#by using the floor function, however sometimes datasets will have whole numbers
#only and still be continuous variables. Likewise, some data such as CPT and diagnosis codes
#are actually categorical variables despite looking like numbers with decimals
#like 23.3 or 49000
#so this step is more complicated than just detecting decimals, whole numbers, and text

variable_types <- character(ncol(original_data))
variable_names <- names(original_data)
included_variables <- character(0)
#this is a new type of code for me that I had to look up, using a menu
for (i in seq_along(variable_names)) {
  choices <- c("Numerical Continuous", "Categorical", "Numerical Discrete")
  selection_type <- select.list(choices, title = paste("Select the type of variable for column '", variable_names[i], "':"))
  variable_types[i] <- selection_type

  # Now we need to ask the user if the variable should be included in the table
  include_variable <- select.list(choices, title = paste("Include variable '", variable_names[i], "' in the table?"), choices = c("Yes", "No"))

  if (include_variable == "Yes") {  #I had an issue here where I didn't denote it as "Yes" but intead tried to say "1" as the selection, but it needs to be "Yes"
    included_variables <- c(included_variables, variable_names[i])
  }
}

#i suppose the above code could be optimized by first asking if the variable should be included
#and then asking for the type of variable it is, so the user isn't forced to identify
#types of variables for vars that won't be included in the final table anyway

#now I need a dataset that is only the included variables
dataset_for_table <- original_data[, included_variables]

print(n=6, dataset_for_table) #just to check that the correct things have been included

#and corresponding variable types
variable_types_for_table <- variable_types[names(variable_types) %in% included_variables]

#now lets start making our flextable

# Create an empty flextable
ft <-  flextable(data = dataset_for_table, col_keys = names(dataset_for_table))


#next I'll need to make a flextable which has values that vary based on the type of data
#that is included. the variable_types_for_table and the dataset_for_table

