#' Find number of occurrences of a string in surveys
#'
#' @description
#' This function returns a vector containing the number of occurrences of a user-defined string (e.g. "Prefer not to answer") per survey throughout the dataframe.
#' The function takes as arguments a dataframe, the string looked for.
#'
#'
#'
#' Note: the string needs to be harmonized across the dataframe. E.g. "don´t want to asnwer" should always be stored on the dataframe as "don´t want to answer".
#'
#'
#' @param data dataframe containing survey data.
#' @param string character string that will be counted.
#'
#'
#' @return A character vector having the count of occurrence of the string.
#' @export
#'
#' @examples
#' # Get the number of occurrences of "don´t want to answer" (stored as "dwta" on the dataframe).
#' \code{n} data <- data %>% mutate(check_nb_dwta = add_count_of(., "dwta"))
#'
#' # Get the names of occurrences of "dont_know".
#' \code{n} data <- data %>% mutate(check_nb_dk = add_count_of(., "dont_know"))


## TODO: Link it to the xls form, calculate prevalence

add_count_of <- function(data,
                         string) {

  if(is.null(data) | nrow(data)<1 | !is.data.frame(data)){
    stop("Please provide the dataset. Dataset should contain at least 1 survey/row")
  }
  if(!is.character(string) | is.null(string)){
    stop("Please provide the string you are looking for in the dataset")
  }


  string__count <- apply(data, 1, function(x) sum(grepl(paste0("^",string,"$"), x)))


  return(string__count)
}

# devtools::document()
