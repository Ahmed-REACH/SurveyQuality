#' Generate change log with empty cleaning comments
#'
#' @description
#' This function creates a change log by comparing two datasets.
#'
#' @param data1 dataframe. The first dataset, inclusive of unique identifier column.
#' @param data2 dataframe. The second dataset, inclusive of unique identifier column.
#' @param identifier column name of the survey ID. Default: ´identifier´.
#' @param data1_priority Logical. only columns existent on data1 would be considered for both datatsets.
#'
#' @return A long table outlining each cell change (identified by the identifier) between the two datasets.
#' @export
#'
#' @examples
#' # Generate the cleaning log.
#' \code change_log <- generate_change_log(raw_data, clean_data, "uuid", data1_priority = T)


generate_change_log <- function(data1,
                                  data2,
                                  identifier = "identifier",
                                data1_priority = FALSE) {

  if( !is.character(identifier) | (is.null(identifier) & is.na(match("identifier",names(data1)))) | (!is.null(identifier) & is.na(match(identifier,names(data1)))) |
      (is.null(identifier) & is.na(match("identifier",names(data2)))) | (!is.null(identifier) & is.na(match(identifier,names(data2))))){
    stop("Please provide the column name of ´survey ID´. It must exist on both datasets.")
  }

  if(is.null(data1) | !is.data.frame(data1) |
     nrow(data1)!= length(unique(data1[[identifier]]))
     ){
    stop("Please provide the first dataset. Row idetifiers should be unique.")
  }

  if(is.null(data2) | !is.data.frame(data2) |
     nrow(data2)!= length(unique(data2[[identifier]]))
  ){
    stop("Please provide the second dataset. Row idetifiers should be unique.")
  }


  long1 <- data1 %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(-match(identifier, names(data1)),
                        names_to = "question",
                        values_to = "old_value"
    )

  long2 <- data2 %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(-match(identifier, names(data2)),
                        names_to = "question",
                        values_to = "new_value"
    )

  tmp_cleaning_log <- dplyr::full_join(long1, long2) %>%
    dplyr::filter(is.na(old_value) | is.na(new_value) | old_value != new_value)

  if (data1_priority) {

    tmp_cleaning_log <- tmp_cleaning_log %>%
      dplyr::filter(question %in% colnames(data1))
  }

  return(tmp_cleaning_log)
}








#' Appends a change log to another
#'
#' @description
#' This function appends a change log to another while keeping distinct records.
#'
#' @param old dataframe, the main change/cleaning log.
#' @param new dataframe, the new log file to be appended to the main log.
#' @param identifier character, column name of the survey ID. Default: ´identifier´.
#'
#'
#' @return A combined long table with distinct entries.
#' @export
#'
#' @examples
#' # Compiles two change/cleaning logs into one.
#' \code compiled_log<- append_2change_logs(old = cl_all, new = cl_new)

append_2change_logs <- function(old,
                               new,
                               identifier = "identifier",
                               q_colname = NULL) {



  if(!is.data.frame(old) | is.null(old) |
     sum(stringr::str_detect(names(old), "identifier|uuid$|question|value") )<4 ){  # here 4 = 1 for uuid/identifier, 1 for question col, 2 for old & new values
    stop("Please provide the dataframe holding the MAIN change log. You need identifier/uuid column, question column and old and new columns.")
  }

  if(!is.data.frame(new) | is.null(new) |
     sum(stringr::str_detect(names(new), "identifier|uuid$|question|value") )<4 ){  # here 4 = 1 for uuid/identifier, 1 for question col, 2 for old & new values
    stop("Please provide the dataframe holding the NEW change log. You need identifier/uuid column, question column and old and new columns.")
  }

  if( !is.character(identifier) |
      (is.null(identifier) & is.na(match("identifier",names(old)))) | (!is.null(identifier) & is.na(match(identifier,names(old)))) |
      (is.null(identifier) & is.na(match("identifier",names(new)))) | (!is.null(identifier) & is.na(match(identifier,names(new))))
  ){
    stop("Please provide the column name of ´survey ID´, uuid, identifier")
  }

  if( !is.character(q_colname) |
      (is.null(q_colname) & sum(stringr::str_detect(names(old), "question"))<1) | (!is.null(q_colname) & sum(stringr::str_detect(names(old), q_colname))<1) |
      (is.null(q_colname) & sum(stringr::str_detect(names(new), "question"))<1) | (!is.null(q_colname) & sum(stringr::str_detect(names(new), q_colname))<1)
  ){
    stop("Please provide the column name of ´question name´ on the input logs")
  }

  #Identify column names for proper for proper filtering as follows
  name_id_col <- ifelse(!missing(q_colname), identifier, stringr::str_extract(names(old), "identifier|^[:graph:]{1,2}uuid$"))
  name_q_col <- ifelse(!is.null(q_colname), q_colname, stringr::str_extract(names(old), "question"))
  name_val_col <- stringr::str_extract(names(old), "old\\s*value|old[:punct:]*value")

  tmp_cleaning_log <- dplyr::bind_rows(old, new) %>%
    tidyr::unite(., "tmp_filter_col", c(name_id_col, name_q_col, name_val_col)) %>%
    dplyr::distinct(tmp_filter_col) %>%
    dplyr::select(-tmp_filter_col)


  return(tmp_cleaning_log)
}














# devtools::document()
