#' Find difference between two submitted surveys
#'
#' @description
#' This function returns a string containing all column names where values are different between surveys.
#' The function takes as arguments a dataframe, name of the unique identifier column, and 2 survey ID (1 mandatory, 1 optional).
#'
#'
#'
#' Note: if you are comparing two surveys with the same identifier, only one identifier is needed.
#'
#'
#' @param data dataframe containing survey data and unique identifier column for the surveys.
#' @param identifier column name of the survey ID. Default: ´identifier´.
#' @param value the survey ID in question. Used as a key for filtering out surveys for comparison.
#' @param value2 optional parameter defining a different survey ID than the first. Useful for comparing surveys with different identifiers.
#'
#'
#' @return A character string containing the names of the columns that are different. column names are separated with a comma ´,´ within the string.
#' @export
#'
#' @examples
#' # Get the names of questions that have different responses on surveys. Fetching by 1 identifier i.e. surveys with duplicate identifier.
#' \code{n} names <- find_different_columns(data, "identifier", "3681f44c-8f1f-49b0-1dzc-6f741d087b24")
#'
#' # Get the names of questions that have different responses on surveys. Fetching by 2 different identifiers
#' \code{n} names <- find_different_columns(data, "identifier", "3681f44c-8f1f-49b0-1dzc-6f741d087b24", "a211f44a-9d1f-94b0-10dc-6f741d811b29")



find_col_difference <- function(data,
                                identifier="identifier",
                                value,
                                value2=NULL) {

  if(is.null(data) | nrow(data)<=1 | !is.data.frame(data)){
    stop("Please provide the dataset. Dataset should contain at least 2 surveys/rows")
  }
  if( !is.character(identifier) | (is.null(identifier) & is.na(match("identifier",names(data)))) | (!is.null(identifier) & is.na(match(identifier,names(data)))) ){
    stop("Please provide the column name of ´survey ID´")
  }
  if( !is.character(value) | is.null(value) | is.na(match(value,data[[identifier]])) ){
    stop("Please provide the ID of the survey in question. E.g. ´2554f55-88988ff-qan22rr27-p66abhfb1´")
  }
  if( (is.null(value2) & length(which(data[[identifier]]==value))<2) | (!is.null(value2) & !is.character(value2)) | (!is.null(value2) & length(which(data[[identifier]]==value2))<1) ){
    stop("Please provide the ID of the second survey in question. It could be that the first provided ID is not duplicated or the second provided ID does not exist in the column")
  }

  # Select the two rows and transpose the resulting dataframe
  working_data <- data %>%
    subset(identifier %in% c(value, value2)) %>%
    t()


  # Fix failure in transposing the colnames (question names)
  rownames(working_data)<- colnames(data)
  #NB: Did not change the column name to identifier because R would add ..1 ..2 to the column name (because identifier values can be duplicated)


  # Get the names of the questions that have different values from the first survey
  col_diff <- rownames(working_data)[apply(working_data, 1, function(x){any(x != x[1])})]

  # Return the column names as a string (can use punctuation to find out how many different columns)
  return(paste0(paste(col_diff, collapse = ", "),"."))
}

# devtools::document()
