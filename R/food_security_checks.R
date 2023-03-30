#' Checks by row if all values are the same in provided columns.
#'
#' @description
#' This function checks, per row/survey, if the values are the same across the provided columns in the dataset.
#' Applicable to Food Consumption questions.
#'
#'
#'
#' @param data dataframe. The dataset.
#' @param fcs_columns vector. The column names of FCS questions.
#' @param fcs_col_pattern character. The pattern to use for finding the FCS questions in the dataset.
#' @param ignore_condiment logical. If FALSE, the condiment column will be ignored in this check. Defaults to TRUE.
#'
#' @return A numeric binary vector. 1 means the values are the same. 0 otherwise.
#' @export
#'
#' @examples
#' # Create a new column for the check as part of the dataset.
#' \code data <- data %>% mutate(CHECK__FCS_same = check_FCS_all_same(data, fcs_col_pattern = "fs_fcs_", ignore_condiment = T))

check_FCS_all_same <- function(data,
                               fcs_columns = NULL,
                               fcs_col_pattern = NULL,
                               ignore_condiment = TRUE) {


  if (ignore_condiment) {nb<- 8} else {nb<- 9}

  if(is.null(data) | nrow(data)<=1 | !is.data.frame(data)){
    stop("Please provide the dataset. Dataset should contain at least 2 surveys/rows")
  }

  if(is.null(fcs_col_pattern) & is.null(fcs_columns)){
    stop("Please provide either a vector of FCS column names or a pattern to identify them on the dataset")
  } else if (is.null(fcs_columns)) {
    cols_fetched <- colnames(data)[stringr::str_detect(colnames(data),fcs_col_pattern)]
  } else if (is.null(fcs_col_pattern)) {
    fcs_columns_pattern <- paste0("^",paste(fcs_columns, collapse = "$|^"),"$")
    cols_fetched <- colnames(data)[stringr::str_detect(colnames(data),fcs_columns_pattern)]
  }

  if(!is.null(fcs_col_pattern) & !is.character(fcs_col_pattern)) {
    stop("Please provide the pattern to identify FCS columns on the dataset.")
    if (sum(stringr::str_detect(names(data), fcs_col_pattern) )<nb){
      stop("Please provide the pattern to identify FCS columns on the dataset. Make sure the pattern can be used to identify ALL columns needed, including ´condiment´")
    }
  }


  if(!is.null(fcs_columns) & (!is.character(fcs_columns) | length(fcs_columns)<nb)){
    # tmp_col <- paste0("^",paste(fcs_columns, collapse = "$|^"),"$")
    # if (sum(stringr::str_detect(names(data), tmp_col) )<nb){
    if (sum(stringr::str_detect(names(data), cols_fetched) )<nb){
      stop("Please provide character vector containing the column names of FCS questions on the dataset. You need at least the 8 food groups.")
    }
  }

  if (missing(ignore_condiment) | ignore_condiment == T) { #if missing, ignore_condiment is at default value TRUE
    cols_fetched <- stringr::str_subset(cols_fetched, pattern = "condiment", negate = TRUE)
  } else if (sum(stringr::str_detect(colnames(data), pattern = "condiment"))<=0) {
    stop("Condiment column will not be ignored. Yet, cannot be found on the dataset. Try adding pattern ´condiment´ in the column name")

  }


  data$check_cols_min_max_same <- apply(data[, cols_fetched], 1,
                                        function(x) length(unique(x)) == 1
  )



  return(data %>% dplyr::pull(check_cols_min_max_same) %>% as.numeric())
}





#' Checks if a condition is respected in a column by comparing column values to a threshold.
#'
#' @description
#' This function performs value checks. Right Hand Side (RHS) is the column, Left Hand side is for the threshold. If the condition is not met, it would be flagged as 1 in the result.
#'
#'
#'
#' @param data dataframe. The dataset.
#' @param column_to_check character. The column name of the column to check its values e.g. "fs_fcs_cerealgrainroottuber".
#' @param operator character. The mathematical operator to use. default: "<=".
#' @param threshold numeric. The threshold.
#'
#'
#' @return A numeric binary vector. 1 means the specified condition is not met. 0 otherwise.
#' @export
#'
#' @examples
#' # Create a new column for the check as part of the dataset.
#' \code data <- data %>% mutate(CHECK__FCS_cereals = check_value_to_threshold(data, column_to_check = "fs_fcs_cerealgrainroottuber", operator = "<=", threshold = 2))
#'
#' \code data <- data %>% mutate(CHECK__FCS_cereals = check_value_to_threshold(., "fs_fcs_cerealgrainroottuber", "<=", 2))

check_value_to_threshold <- function(data,
                               column_to_check = NULL,
                               operator = "<=",
                               threshold = NULL) {


  if(is.null(data) | nrow(data)<=1 | !is.data.frame(data)){
    stop("Please provide the dataset. Dataset should contain at least 2 surveys/rows")
  }

  if(!is.null(column_to_check) & (!is.character(column_to_check) | sum(stringr::str_detect(names(data), column_to_check) )<=0) ){
    stop("Please provide the correct column name you want to check. Make sure it exists on the dataset, the argument provided need to be character.")
  }

  if (!missing(operator) & !operator %in% c("==", ">", "<", "!=", "<=", ">=") ){
    stop("Please provide a correct operator. It could be ´==´, ´>´, ´<´, ´!=´, ´<=´, ´>=´.")
  }

  if(!is.null(threshold) & !is.numeric(threshold) ){
    stop("Please provide a numeric threshold.")
  }


  data<- data %>%
    dplyr::mutate(
      check_threshold_comparison = eval(parse(text = paste(column_to_check, operator, threshold)))
  )



  return(data %>% dplyr::pull(check_threshold_comparison) %>% as.numeric())
}






#' Checks by row if all values are the same in provided columns.
#'
#' @description
#' This function checks, per row/survey, if the values are the same across the provided columns in the dataset.
#' Applicable to Food Consumption questions.
#'
#'
#'
#' @param data dataframe. The dataset.
#' @param fcs_columns vector. The column names of FCS questions.
#' @param fcs_col_pattern character. The pattern to use for finding the FCS questions in the dataset.
#' @param n_distinct numeric. the number of distinct digits to check for.
#' @param ignore_condiment logical. If FALSE, the condiment column will be ignored in this check. Defaults to TRUE.
#'
#'
#' @return A numeric binary vector. 1 means the values are the same. 0 otherwise.
#' @export
#'
#' @examples
#' # Create a new column for the check as part of the dataset.
#' \code data <- data %>% mutate(CHECK__FCS_alter = check_FCS_alternating_values(, fcs_col_pattern = "fs_fcs_", n = 2, ignore_condiment = T))

check_FCS_alternating_values <- function(data,
                                         fcs_columns = NULL,
                                         fcs_col_pattern = NULL,
                                         n = 2,
                                         ignore_condiment = TRUE) {


  if (ignore_condiment) {nb<- 8} else {nb<- 9}

  if(!is.null(n) & n<2 ){
    stop("Please provide the number of distinct digits to check for. You need to provide a number >=2. n is not specified, default value is used")
  }

  if(is.null(data) | nrow(data)<=1 | !is.data.frame(data)){
    stop("Please provide the dataset. Dataset should contain at least 2 surveys/rows")
  }

  if(is.null(fcs_col_pattern) & is.null(fcs_columns)){
    stop("Please provide either a vector of FCS column names or a pattern to identify them on the dataset")
  } else if (is.null(fcs_columns)) {
    cols_fetched <- colnames(data)[stringr::str_detect(colnames(data),fcs_col_pattern)]
  } else if (is.null(fcs_col_pattern)) {
    fcs_columns_pattern <- paste0("^",paste(fcs_columns, collapse = "$|^"),"$")
    cols_fetched <- colnames(data)[stringr::str_detect(colnames(data),fcs_columns_pattern)]
  }

  if(!is.null(fcs_col_pattern) & !is.character(fcs_col_pattern)) {
    stop("Please provide the pattern to identify FCS columns on the dataset.")
    if (sum(stringr::str_detect(names(data), fcs_col_pattern) )<nb){
      stop("Please provide the pattern to identify FCS columns on the dataset. Make sure the pattern can be used to identify ALL columns needed, including ´condiment´")
    }
  }

  if(!is.null(fcs_columns) & (!is.character(fcs_columns) | length(fcs_columns)<nb)){
    if (sum(stringr::str_detect(names(data), cols_fetched) )<nb){
      stop("Please provide character vector containing the column names of FCS questions on the dataset. You need at least the 8 food groups.")
    }
  }

  if (missing(ignore_condiment) | ignore_condiment == T) { #if missing, ignore_condiment is at default value TRUE
    cols_fetched <- stringr::str_subset(cols_fetched, pattern = "condiment", negate = TRUE)
  } else if (sum(stringr::str_detect(colnames(data), pattern = "condiment"))<=0) {
    stop("Condiment column will not be ignored. Yet, cannot be found on the dataset. Try adding pattern ´condiment´ in the column name")
  }


  data$check_cols_alternating <- apply(data[, cols_fetched], 1,
                                        function(x) length(unique(x)) <= n
  )

  return(data %>% dplyr::pull(check_cols_alternating) %>% as.numeric())
}

# devtools::document()
