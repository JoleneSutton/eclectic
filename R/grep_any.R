#' Multiple pattern matching
#'
#' Apply `grep` to a pattern that can be comprised of multiple elements.
#' @param x A vector of elements to search for
#' @param y A vector to search within
#' @param match Either 'any', 'exact' or 'begins'. Default is 'any'.
#' @param ignore.case Ignore case? Default is TRUE
#' @examples
#' x<-c(1,5,8,25)
#' y<-seq(0,20)
#' grep_any(x,y,'exact')
#' x<-c('4t','4r')
#' y<-c('3PN','4VN','4T')
#' grep_any(x,y)
#' @export
grep_any<-function(x,y,match='any',ignore.case=TRUE){
  if(match=='exact'){x=paste0("^",x,"$")}
  if(match=='begins'){x=paste0("^",x)}

  if(isTRUE(ignore.case)){grep(pattern=paste0(as.character(x),collapse="|"), as.character(y), ignore.case = TRUE)}else{
    grep(pattern=paste0(as.character(x),collapse="|"), as.character(y), ignore.case = FALSE) }
}
