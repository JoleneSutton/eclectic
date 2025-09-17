#' Split data frame column or string on delimiter
#'
#' @param x Data frame or string
#' @param delimiter Delimiter
#' @param col.names Names for the returned data frame columns
#' @return A data frame
#' @examples
#' string<-c("161293 - a" ,"155541 - b" , "161293 - c")
#' split_on_delimiter(string," - ")
#' @export
split_on_delimiter<-function(x,delimiter,col.names=NULL){
  x<-as.data.frame(x)
  x2<-as.data.frame(t(apply(x, 1, function(row) base::strsplit(row, delimiter)[[1]])))
  if(!is.null(col.names)){
  names(x2)=col.names
  }
  return(x2)
}