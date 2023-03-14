check_duplicate_uuid_krap <- function(data = NULL,
                       uuid_column=NULL,
                       survey_consent=NULL,
                       details=c(enumerator_id, uuid_column, survey_date),
                       deleteIsuniquerespondantIDDuplicated=FALSE){
  if(is.null(data) | nrow(data)==0 | !is.data.frame(data)){
    stop("Please provide the dataset")
  }
  if(is.null(survey_consent) | !is.character(survey_consent)){
    stop("Please provide the column name of ´survey consent´")
  }
  if(is.null(uuid_column) | !is.character(uuid_column) | is.na(match(uuid_column,names(data)))){
    stop("Please provide the column name of ´unique survey ID´")
  }
  if(is.null(details) | !is.character(details)){
    stop("Please provide the columns you want in the result (include the enumerator id column if you want to check by enumerator)")
  }
  if(is.null(deleteIsuniquerespondantIDDuplicated) | !is.logical(deleteIsuniquerespondantIDDuplicated)){
    stop("Please provide the delete action to be done (TRUE/FALSE)")
  }

  if(deleteIsuniquerespondantIDDuplicated){
    data[,survey_consent][duplicated(data[,uuid_column])] <- "deletedIsuniquerespondantIDDuplicated"
  }

  # TO BE BE CHANGED WITH DYNAMIC COLUMS
  ret_log <- subset(data,duplicated(data[,uuid_column])) %>%
    dplyr::select(all_of(details), survey_consent=survey_consent)

  check <- data.frame(categories=c("OK", "NOK"),
                      Nb = c(  length(data[,1])-length(ret_log[,1]),
                               length(ret_log[,1])))


  t1 <- check
  t1$fraction <- t1$Nb / sum(t1$Nb)
  t1 <- t1[order(t1$fraction), ]
  t1$ymax <- cumsum(t1$fraction)
  t1$ymin <- c(0, utils::head(t1$ymax, n=-1))



  graph  <- ggplot2::ggplot(t1, ggplot2::aes(fill=categories,
                                             ymax=ymax,
                                             ymin=ymin,
                                             xmax=4,
                                             xmin=3)) +
    ggplot2::geom_rect(colour="grey30") +
    ggplot2::coord_polar(theta="y") +
    ggplot2::xlim(c(1, 4)) +
    ggplot2::theme_void() +
    ggplot2::theme(panel.grid=ggplot2::element_blank()) +
    ggplot2::theme(axis.text=ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks=ggplot2::element_blank()) +
    ggplot2::theme(legend.position='none') +
    ggplot2::labs(title= "Are Unique ID Duplicated?",
                  subtitle = paste0("Errors: ", round(t1$fraction[t1$categories == 'NOK']*100,2), "%"))



  result <- list( data = data, #  same dataset as the inputed one but with survey marked for deletion if errors are found and delete=TRUE (or NULL)
                  ret_log  = ret_log , # list of the errors found otherwise NULL
                  var = NULL, #  a list of value (or NULL)
                  graph = graph) # graphical representation of the results (or NULL)


  return(result)
}
