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
#' \code columns_other <- find_columns_other(data, kobo_survey)


find_columns_other <- function(data, survey_df) {

  if(is.null(data) | nrow(data)<=1 | !is.data.frame(data)){
    stop("Please provide the dataset. Dataset should contain at least 2 surveys/rows")
  }
  if(!is.data.frame(survey_df) | is.null(survey_df) |
     sum(stringr::str_detect(names(survey_df), "^type$|^name$") )<2 ){
    stop("Please provide the dataframe holding the survey sheet of your XLS form with column names unchanged")
  }


  # Preparation: cleaning up column names
  data<- data %>%
    stats::setNames(gsub("^[X_]?|^[_]?|^ï..", "",names(.)))


  potential_others <- names(data)[stringr::str_detect(tolower(names(data)), "_other$|_others$|^specify_other" )]

  text_vars <- survey_df %>%
    dplyr::mutate(type = trimws(type, which = "both")) %>%
    dplyr::filter(type == "text") %>%
    pull(name)

  others <- text_vars[which(text_vars %in% potential_others)]


  return(others)

}


# devtools::document()

