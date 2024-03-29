#' Find count ratio of an answer option throughout surveys
#'
#' @description
#' This function returns a vector containing the count and ratio of a user-defined string (e.g. "other") per survey.
#' The function takes as arguments a dataframe, the string looked for, survey and choices sheets of the XLS forms.
#'
#'
#'
#' Note: the string needs to be harmonized across the dataframe. E.g. "other" should always be stored on the dataframe as "other". Hoyever, you can use regex to find "other" and its alternatives e.g. "other_specify".
#'
#'
#' @param data dataframe containing survey data.
#' @param string character string that will be counted. Regex allowed.
#' @param survey_df dataframe containing ´survey´ sheet of the XLS form.
#' @param choices_df dataframe containing ´choices´ sheet of the XLS form.
#'
#'
#' @return A numeric vector having the ratio(%) of occurrence of the string.
#' @export
#'
#' @examples
#' # Get the ratio of "other" (stored as "other" on the dataframe).
#' \code data <- data %>% mutate(check_others_ratio = ratio_of(., "dwta", kobo_survey, kobo_choices))
#'
#' # Get the ratio of "other" and its alternatives.
#' \code data <- data %>% mutate(check_others_etc_ratio = ratio_of(., "dont_know", kobo_survey, kobo_choices))


ratio_of <- function(data,
                     string,
                     survey_df,
                     choices_df) {

  # Preparation: input checks
  if(is.null(data) | nrow(data)<1 | !is.data.frame(data)){
    stop("Please provide the dataset. Dataset should contain at least 1 survey/row")
  }
  if(!is.character(string) | is.null(string)){
    stop("Please provide the string you are looking for in the dataset")
  }
  if(!is.data.frame(survey_df) | is.null(survey_df) |
     sum(stringr::str_detect(names(survey_df), "^type$|^name$") )<2 ){
    stop("Please provide the dataframe holding the survey sheet of your XLS form with column names unchanged.")
  }
  if(!is.data.frame(choices_df) | is.null(choices_df) |
     sum(stringr::str_detect(names(choices_df), "^list_name$|^name$") )<2 ){
    stop("Please provide the dataframe holding the survey sheet of your XLS form with column names unchanged.")
  }



  # Preparation: add Regex to string if inexistent
  string <- tolower(trimws(string, which = "both"))

  if (!grepl("\\$", string) ) {
    string <- paste0(string,"$")

  } else if (!grepl("\\^", string) ) {
    string <- paste0("^", string)

  } else if (grepl("\\|", string) ) {
    tmp<- stringr::str_split_1(string) %>%
      stringr::str_replace_all(., "^","") %>%
      stringr::str_replace_all(., "$","")
    string <- paste0("^",
                     paste(tmp, sep = "", collapse = "$|^"),
                     "$"
    )
  } else {
    string <- string
  }


  # Preparation: cleaning up column names and selecting the columns needed
  data<- data %>%
    dplyr::mutate_all(., as.character) %>%
    stats::setNames(gsub("\\/|\\.",".",names(.)))

  questions_answers <- survey_df %>%
    dplyr::select(type, name) %>%
    dplyr::rename(question_name = name) %>%
    tidyr::separate(., col = "type", into = c("type", "list_name")) %>%
    dplyr::right_join(.,
                      choices_df %>%
                        dplyr::mutate(answer_name = tolower(name)) %>%
                        dplyr::select(list_name, answer_name),
                      by = "list_name"
    )

  sm_questions <-  questions_answers %>%
    dplyr::filter(type=="select_multiple") %>%
    dplyr::pull(name)


  # Calculation: calculate count for all questions except select_multiple questions
  string__count_selectone <- apply(data[,-sm_questions], 1, function(x) sum(grepl(string), x))


  # Calculation: calculate count for select_multiple questions
  cols_mul <- gsub("^", ".", string)
  string__count_selectmultiple <- data %>%
    dplyr::mutate(
      string__count_selectmultiple_tmp = rowSums(select(., tidyselect::ends_with(cols_mul)))
    )


  unique_questions <- questions_answers %>%
    dplyr::filter(grepl(string, answer_name)) %>%
    dplyr::pull(question_name) %>% unique()

  numerator <- string__count_selectone+string__count_selectmultiple

  denominator <- rep(length(unique_questions), nrow(data))

  ratio <- round(100*numerator/denominator, digits = 3)


  return(ratio)
}

# devtools::document()

