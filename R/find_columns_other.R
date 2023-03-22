#' Find column names of columns "other"
#'
#' @description
#' This function returns a vector containing the names of columns that end with "other" and are text questions on the XLS form.
#'
#'
#'
#' @param data dataframe containing data.
#' @param survey_df dataframe containing ´survey´ sheet of the XLS form.
#'
#'
#' @return A character vector containing the names of the "other" variables.
#' @export
#'
#' @examples
#' # Get the column names of text "other" .
#' \code{n} columns_other <- find_columns_other(data, kobo_survey)


find_columns_other <- function(data, survey_df) {


  # Preparation: cleaning up column names
  data<- data %>%
    stats::setNames(gsub("^[X_]?|^[_]?|^ï..", "",names(.)))


  potential_others <- names(data)[stringr::str_detect(tolower(names(data)), "_other$|_others$|^specify_other" )]

  text_vars <- survey_df %>%
    dplyr::mutate(type = trimws(type, which = "both")) %>%
    dplyr::filter(type == "text") %>%
    pull(name)

  others <- potential_others[which(potential_others %in% text_vars)]


  return(others)

}


# devtools::document()

