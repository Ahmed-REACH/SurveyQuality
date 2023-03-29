#' Checks if all values are the same for FCS
#'
#' @description
#' This function checks, per survey, if the values are the same across FCS questions.
#'
#'
#'
#' @param data dataframe containing data.
#' @param fcs_col_start pattern to use for finding the FCS questions in the dataset.
#' @param fcs_columns vector containing column names of FCS questions.
#' @param ignore_condiment If FALSE, the condiment column will be ignored in this check. Defaults to TRUE.
#'
#'
#' @return A numeric binary vector. 1 means the values are the same. 0 otherwise.
#' @export
#'
#' @examples
#' # Create a new column for the check as part of the dataset.
#' \code{n} data <- data %>% mutate(CHECK__FCS_same = check_FCS_all_same(data, fcs_col_start = "fs_fcs_", ignore_condiment = T))

check_FCS_all_same <- function(data,
                               fcs_col_start = NULL,
                               fcs_columns = NULL,
                               ignore_condiment = TRUE) {

  if (ignore_condiment) {nb<- 8} else {nb<- 9}
  if(is.null(data) | nrow(data)<=1 | !is.data.frame(data)){
    stop("Please provide the dataset. Dataset should contain at least 2 surveys/rows")
  }
  if(is.null(fcs_col_start) & is.null(fcs_columns)){
    stop("Please provide either a vector of FCS column names or a pattern to identify them on the dataset")
  }
  if(!is.null(fcs_col_start) & (!is.character(fcs_col_start) | sum(stringr::str_detect(names(data), fcs_col_start) )<nb) ){
    stop("Please provide the pattern to identify FCS columns on the dataset")
  }
  if(!is.null(fcs_columns) & (!is.character(fcs_columns) | length(fcs_columns)<nb)){
    tmp_col <- paste0("^",paste(fcs_columns, collapse = "$|^"),"$")
    if (sum(stringr::str_detect(names(data), tmp_col) )<nb){
      stop("Please provide character vector containing the column names of FCS questions on the dataset. You need at least the 8 food groups.")
    }
  }

  check <- data %>%
    mutate(temp_FCS_min_max_same = pmax(!!!syms(fcs_columns), na.rm = T) == pmin(!!!syms(fcs_columns), na.rm = T) ) %>%
    pull(FCS_min_max_same) %>%
    as.numeric()

  return(check)
}






# devtools::document()
