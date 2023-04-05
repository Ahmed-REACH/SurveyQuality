#' Replaces backslash with slash in the provided file path
#'
#' @description
#' \code{6} pathR function returns character string containing the file path fixed for use in R. It replaces backslash with slash.
#' NOTE: No checks are perfomed on the file path
#'
#'
#'
#' @param path Character, containing ´windows path´.
#'
#'
#' @return A character string containing fixed file path.
#' @export
#'
#' @examples
#' \code df <- pathR(path) %>% read_xlsx(., sheet = 1)


pathR <- function(path) {

  if( !is.character(path) | is.null(path) ){
    stop("Please provide the file path.")
  }


  return(path %>% stringr::str_replace_all(., "\\\\","/"))
}


# devtools::document()
