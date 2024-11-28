#' Find and replace
#'
#' Performs a find replace on a vector by comparing to another vector. When no matches occur, the original value is retained.
#' @param original The vector to search
#' @param to.match A vector that possibly contain matches to original. Must be same length as to.replace
#' @param to.replace A vector of values to use when a match is found between original and to.match. Must be same length as to.match
#' @return Returns a data frame showing the original vector and the replacement. If no match was found, the original be returned. Both columns are characters.
#' @examples
#' #E.g., Search "original" for "1", "2", "3", "4", and replace with "blue","green","purple","yellow"
#' original<-c(3,10,3,1,5,2)
#' to.find<-c(1,2,3,4)
#' to.replace<-c("blue","green","purple","yellow")
#' find_replace(original,to.find,to.replace)
#' @export
find_replace <- function(original,to.match,to.replace){#to.match and to.replace must have same lengths
  row<-array()
  codes<-array()
  master<-cbind(to.match,to.replace)
  for(i in 1:length(original)){
    if(length(which(master[,1] %in% original[i]))>0){
      row[i]<-(which(master[,1] %in% original[i]))
      codes[i]<-master[row[i],2]
    }
  }
  replacements<-codes[which(!is.na(row))]
  toreplace<-which(original %in% master[,1])
  replacement<-original
  replacement[toreplace]<-replacements
  replacement<-cbind(original,replacement)
  replacement[,1:2]
}
