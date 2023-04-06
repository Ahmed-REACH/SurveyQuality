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
                                            "stringr\\:\\:str\\_detect\\(\\!stringr\\:\\:str\\_detect\\(" = "!stringr::str_detect("
                                          )
    )
    )




  return(skip_df)
}


# devtools::document()

