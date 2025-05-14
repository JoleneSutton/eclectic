#' Count occurrences of NA
#'
#' @param x A vector or string
#' @param count.empty Treat empty values (white space) as NA? Logical. Default is FALSE.
#' @examples
#' count_na(c(1,2,NA,4,NA))# 2
#' count_na(c(1,2,'',4,NA))# 1
#' count_na(c(1,2,'',4,NA),count.empty=TRUE)# 2
#' count_na(c(1,2,'    ',4,NA),count.empty=TRUE)# 2
#' @export
count_na<-function(x,count.empty=FALSE){

  if(isTRUE(count.empty)){
    x<-gsub("\\s+", "", x)
    x[which(x=="")]<-NA}

  length(which(is.na(x)))
}
