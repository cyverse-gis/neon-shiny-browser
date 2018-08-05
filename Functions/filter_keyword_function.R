filter_keyword <- function(column, keywords) {
  boolean_list <- NULL
  if (is.null(keywords)) {
    return(TRUE)
  } else {
    for (i in 1:length(column)) {
      if (sum(column[[i]] %in% keywords) == length(keywords)) {
        boolean_list <- c(boolean_list, TRUE)
      } else {
        boolean_list <- c(boolean_list, FALSE)
      }
    }
    return(boolean_list)
  }
}