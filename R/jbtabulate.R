
#' Title
#'
#' @param x
#' @param variable_types
#'
#' @return returns the final flextable for you
#' @export
#'
#'
jbtabulate <- function(x,variable_types){
  #Creates a data frame to store variable names and summary data
  dataset_for_table <- x
  variable_names <- names(x)
  summary_data <- data.frame(VariableName = character(0), Summary = character(0), stringsAsFactors = FALSE)

  #Next, add rows to the summary data data frame based on variable types prespecified by user
  for (i in seq_along(variable_names)) {
    var_name <- variable_names[i]
    var_type <- variable_types[i]

    if (var_type == "Categorical") {
      levels <- unique(dataset_for_table[[var_name]])

      #Append the categorical variable name only
      summary_data <- rbind(summary_data, data.frame(VariableName = var_name, Summary = "", stringsAsFactors = FALSE))

      for (level in levels) {
        subset_data <- dataset_for_table[dataset_for_table[[var_name]] == level, , drop = FALSE]
        summary_value <- calculate_summary(subset_data[[var_name]], var_type)
        #Append to the summary data data frame to include
        summary_data <- rbind(summary_data, data.frame(VariableName = paste0("    ", level), Summary = summary_value, stringsAsFactors = FALSE))
      }
    } else {
      summary_value <- calculate_summary(dataset_for_table[[var_name]], var_type)
      #Append to the summary data data frame
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
