#' Creates a skip logic dataframe from the XLS form
#'
#' @description
#' This function returns dataframe containing the question names, question type, and cleaned skip logic based on the the survey sheet of the XLS form.
#' NOTE: Only questions with skip logic are returned.
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
#' \code{n} columns_other <- find_columns_other(data, kobo_survey)


create_skip_logic_table <- function(survey_df) {

  if(!is.data.frame(survey_df) | is.null(survey_df) |
     sum(stringr::str_detect(names(survey_df), "^type$|^name$|^relevant$") )<3 ){
    stop("Please provide the dataframe holding the survey sheet of your XLS form with column names unchanged.")
  }


  # Preparation: cleaning up column names
  skip_df <- survey_df %>%
    dplyr::mutate(type = trimws(type, which = "both")) %>%
    dplyr::select(type, name,relevant) %>%
    dplyr::filter( !relevant %in% c(NA,"","NA","N/A") ) %>%
    tidyr::separate(col = type, into = c("type", "list_name"), sep = " ")

  skip_df<- skip_df %>%
    dplyr::mutate(
    relevant_formatted = stringr::str_replace_all(relevant,
                                          c(" and | AND | And " = " & ",
                                            " or | OR | Or " = " | ",
                                            "selected(${" = "stringr::str_detect(",
                                            "}, '|},'|} , '|} ,'" = ", '",
                                            "(?<!\\>)\\=|(?<!\\<)\\=|(?<!\\!)\\=|(?<!\\=)\\=" = "==",
                                            "NOT(|not(|Not(" = "!("
                                          )

                                          # c(" & ",
                                          #   " | ",
                                          #   "stringr::str_detect(",
                                          #   ", '",
                                          #   "==",
                                          #   "!("
                                          #   )
    )
  )





  return(survey_df)
}


# devtools::document()

