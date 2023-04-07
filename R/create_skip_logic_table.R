#' Creates a skip logic dataframe from the XLS form
#'
#' @description
#' This function returns dataframe containing the question names, question type, and cleaned skip logic based on the the survey sheet of the XLS form.
#' NOTE: Only questions with question-level defined skip logic are returned.
#'
#'
#'
#' @param survey_df dataframe containing ´survey´ sheet of the XLS form.
#'
#'
#' @return A dataframe containing the names of the survey questions, their types, and their skip logic.
#' @export
#'
#' @examples
#' # Creates input for cleaning up the questions based on skip logic.
#' \code skip_logic_t <- create_skip_logic_table(kobo_survey)


create_skip_logic_table <- function(survey_df) {

  if(!is.data.frame(survey_df) | is.null(survey_df) |
     sum(stringr::str_detect(names(survey_df), "^type$|^name$|^relevant$") )<3 ){
    stop("Please provide the dataframe holding the survey sheet of your XLS form with column names unchanged.")
  }


  # Preparation: cleaning up column names
  skip_df <- survey_df %>%
    dplyr::mutate(type = trimws(type, which = "both")) %>%
    dplyr::select(type, name,relevant) %>%
    dplyr::filter( !relevant %in% c(NA,"","NA","N/A") | stringr::str_detect(type,"group$")  ) %>%
    tidyr::separate(col = type, into = c("type", "list_name"), sep = " ")

  skip_df<- skip_df %>%
    dplyr::mutate(
    relevant_formatted = stringr::str_replace_all(relevant,
                                          c(" and | AND | And " = " & ",
                                            " or | OR | Or " = " | ",
                                            "selected\\(\\$\\{" = "stringr::str_detect(",
                                            "\\}\\, \\'|\\}\\,\\'|\\} \\, \\'|\\} \\,\\'" = ", '",
                                            "(?<!\\>)\\=|(?<!\\<)\\=|(?<!\\!)\\=|(?<!\\=)\\=" = "==",
                                            "NOT\\(|not\\(|Not\\(" = "!(",
                                            "\\$\\{|\\}" = "",
                                            "\\>\\=\\=" = ">=",
                                            "\\<\\=\\=" = "<=",
                                            "\\!\\=\\=" = "!=",
                                            "\\'(?![[:graph:]]+)" = "')",
                                            "\\=\\=\\s*\\'" = ",'",
                                            "\\s*([[:graph:]]+)\\s*\\!\\=\\s*\\'" = " !stringr::str_detect(\\1,'",
                                            "\\s*([[:graph:]]+)\\s*\\,\\s*\\'" = " stringr::str_detect(\\1,'",
                                            "\\s*([[:graph:]]+)\\s*\\=\\s*\\'" = " stringr::str_detect(\\1,'",
                                            "stringr\\:\\:str\\_detect\\(\\!stringr\\:\\:str\\_detect\\(" = "!stringr::str_detect(",
                                            "stringr\\:\\:str\\_detect\\(stringr\\:\\:str\\_detect\\(" = "stringr::str_detect(",
                                            "stringr\\:\\:str\\_detect\\(stringr\\:\\:str\\_detect\\("= "stringr::str_detect(",
                                            "\\‘|\\’|\\‛|\\´|\\`" = "'",
                                            "\\(\\(" = "(",
                                            "stringr\\:\\:str\\_detect\\(([[:graph:]]+)\\,\\'\\'\\)"= "is.na(\\1)"
                                          )
    )
    )




  return(skip_df)
}







#' Applies skip logic to dataframe (Work in progress)
#'
#' @description
#' This function returns the new dataset with the skip logic applied to the columns.
#' NOTE: Ignores group skip logic. Only questions with question-level skip logic is considered.
#'
#'
#'
#' @param survey_df dataframe containing ´survey´ sheet of the XLS form.
#'
#'
#' @return A list of two objects: 1. dataframe with skip logic applied. 2. change log
#' @export
#'
#' @examples
#' # Empties replies in certain questions based on skip logic.
#' \code data_cleaned <- apply_skip_logic(data, skip_logic_t, formula_col = "relevant_R")


apply_skip_logic <- function(data,
                             skip_logic_table,
                             formula_col) {


  if(is.null(data) | nrow(data)<=1 | !is.data.frame(data)){
    stop("Please provide the dataset. Dataset should contain at least 2 surveys/rows.")
  }

  if(is.null(formula_col) | !is.character(formula_col) |
     sum(stringr::str_detect(names(skip_logic_table), paste0("^", trimws(formula_col, "both"), "$")) )!=1 ){
    stop("Please provide the column name holding the skip logic in R syntax. You can use funtion ´create_skip_logic_table´ to generate this column.")
  }

  if(is.null(skip_logic_table) | !is.data.frame(skip_logic_table) |
     sum(stringr::str_detect(names(skip_logic_table), paste0("^type$|^name$|^", trimws(formula_col,"both"), "$")) )<3 ){
    stop("Please provide the dataframe holding the ´skip logic table´. You can use funtion ´create_skip_logic_table´ to generate this dataframe. Or input dataframe with columns ´type´, ´name´, ´formula´(XLS form relevant column but in R syntax)")
  }




  ## Prep
  return_list <- list()
  formula_col <- trimws(formula_col, "both")
  start_data <- data
  skip_table <- skip_logic_table %>%
    dplyr::mutate(
      relevant_formatted = stringr::str_replace_all(relevant_formatted,
                                                    c("stringr\\:\\:str\\_detect\\(\\!stringr\\:\\:str\\_detect\\(" = "!stringr::str_detect(",
                                                      "stringr\\:\\:str\\_detect\\(stringr\\:\\:str\\_detect\\(" = "stringr::str_detect(",
                                                      "stringr\\:\\:str\\_detect\\(stringr\\:\\:str\\_detect\\("= "stringr::str_detect(",
                                                      "\\‘|\\’|\\‛|\\´|\\`" = "'",
                                                      "\\(\\(" = "(")
      )
    ) %>%
    dplyr::filter(!is.na(name) & name %in% names(data) & !is.na(!!sym(formula_col)))




  for (aa in unique(skip_table$name)) {
    try(
      data[which(eval(parse(text = skip_table[skip_table$name==aa, formula_col]), envir = data)), aa] <- NA
    )
  }



  change_log <- generate_change_log(start_data, data)


  return_list[["data"]] <- data
  return_list[["change_log"]] <- change_log

  return(return_list)
}


# devtools::document()

