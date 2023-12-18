#function to categorize data
#' Title
#'
#' @param x
#'
#' @return #the output is you final table ready for publish
#' @export
#'
#'
jbcategorize <- function(x){


variable_types <- character(ncol(x))
variable_names <- names(x)
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


#now I need a dataset that is only the included variables

dataset_for_table <- x[, included_variables]

print(n=6, dataset_for_table) #just to check that the correct things have been included

#and corresponding variable types
variable_types_for_table <- variable_types[names(variable_types) %in% included_variables]


calculate_summary <- function(variable, variable_type) {
  if (variable_type == "Numerical Continuous") {
    mean_val <- mean(variable, na.rm = TRUE)
    sd_val <- sd(variable, na.rm = TRUE)
    return(paste0("", sprintf("%.2f", mean_val), " (SD = ", sprintf("%.2f", sd_val), ")"))
  } else if (variable_type == "Categorical") {
    counts <- table(variable)
    percentages <- prop.table(counts) * 100
    return(paste(counts, " (", sprintf("%.1f%%", percentages), ")", collapse = "\n"))
  } else if (variable_type == "Numerical Discrete") {
    median_val <- median(variable, na.rm = TRUE)
    iqr_val <- IQR(variable, na.rm = TRUE)
    return(paste0("", sprintf("%.1f", median_val), " (IQR = ", sprintf("%.2f", iqr_val), ")"))
  } else {
    return("Unknown variable type")
  }
}


variable_names <- names(dataset_for_table)
variable_types <- variable_types_for_table

# Create an empty data frame to store variable names and summary data
summary_data <- data.frame(VariableName = character(0), Summary = character(0), stringsAsFactors = FALSE)

# Add rows to the summary data data frame based on variable types
for (i in seq_along(variable_names)) {
  var_name <- variable_names[i]
  var_type <- variable_types[i]

  if (var_type == "Categorical") {
    levels <- unique(dataset_for_table[[var_name]])

    # Append the categorical variable name only
    summary_data <- rbind(summary_data, data.frame(VariableName = var_name, Summary = "", stringsAsFactors = FALSE))

    for (level in levels) {
      subset_data <- dataset_for_table[dataset_for_table[[var_name]] == level, , drop = FALSE]
      summary_value <- calculate_summary(subset_data[[var_name]], var_type)
      # Append to the summary data data frame
      summary_data <- rbind(summary_data, data.frame(VariableName = paste0("    ", level), Summary = summary_value, stringsAsFactors = FALSE))
    }
  } else {
    summary_value <- calculate_summary(dataset_for_table[[var_name]], var_type)
    # Append to the summary data data frame
    summary_data <- rbind(summary_data, data.frame(VariableName = var_name, Summary = summary_value, stringsAsFactors = FALSE))
  }
}

# Create a flextable with variable names and summary data
ft <- flextable(data = summary_data)

ft <- flextable::set_table_properties(ft, layout = "autofit") %>%
  align(j = 1, align = "left") %>%
  align(j = 2, align = "center")

ft <- set_header_labels(ft, VariableName = "Variable")

header_label <- paste("Summary (n=", nrow(x), ")", sep = "")
ft <- set_header_labels(ft, Summary = header_label)

# Print the flextable
ft
}
