

#' Title
#'
#' @param x
#'
#' @return returns the code you need to create a new string that has necessary data for jbtabulate function
#' @export
#'
#'
jbcategorize2 <- function(){
 print(The following code is interactive and must run in your console.

      Run the following code in your console:
       x <- %OURDATAHERE%
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
       variable_names <- names(dataset_for_table)
       variable_types <- variable_types_for_table
       )

}
