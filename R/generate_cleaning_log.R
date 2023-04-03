#' Generate cleaning log
#'
#' @param data1 dataframe. The first dataset, inclusive of unique identifier column.
#' @param data2 dataframe. The second dataset, inclusive of unique identifier column.
#' @param identifier column name of the survey ID. Default: ´identifier´.
#'
#' @return A long table outlining each cell change (identified by the identifier) between the two datasets.
#' @export
#'
#' @examples
#' # Generate the cleaning log.
#' \code cleaning_log <- generate_cleaning_log(raw_data, clean_data, "uuid")


generate_cleaning_log <- function(data1,
                                  data2,
                                  identifier = "identifier") {

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


  raw_long <- data1 %>%
    tidyr::pivot_longer(-match(identifier, names(data1)),
                        names_to = "question",
                        values_to = "old_value"
    )

  clean_long <- data2 %>%
    tidyr::pivot_longer(-match(identifier, names(data2)),
                        names_to = "question",
                        values_to = "new_value"
    )

  tmp_cleaning_log <- dplyr::full_join(raw_long, clean_long) %>%
    dplyr::filter(old_value != new_value)

  return(tmp_cleaning_log)
}
