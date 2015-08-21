#' @export

the_date <- function(the.date){
  if(is.null(the.date)) {
    the.date <- as.character(Sys.Date())
  }
}
