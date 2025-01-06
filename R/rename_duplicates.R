#' Rename duplicates
#'
#' Rename duplicates by adding a suffix
#' @param x A vector
#' @param sep Specify the separator (delimiter) to use before the suffix
#' @param type Use letters (alphabet) or numbers to create the suffix
#' @param all Add the suffix to elements of the duplicated value, including the first instance (all=TRUE), or just to the subsequent instances
#' @examples
#' x<-c(404, 405,406, 407, 408, 408 ,408, 408,409 ,409,410,411)
#' rename_duplicates(x)
#' rename_duplicates(x,all=FALSE)
#' rename_duplicates(x,type="numeric")
#' rename_duplicates(x,type="numeric",all=FALSE)
#' @export
rename_duplicates<-function (x, sep="_",type="alphabet",all=TRUE) {

    x <- as.character(x)
    dup<- duplicated(x)
    dup.index <- x[dup]

    for(i in 1:length(dup.index)){

      len<-length(x[which(x%in%dup.index[i])])

      ifelse(type=="alphabet",
             suff<-paste0(sep,letters[1:len]),
             suff<-paste0(sep,1:len))

      x[which(x%in%dup.index[i])]<-paste0(x[which(x%in%dup.index[i])],suff)
      }

    if(all!=TRUE){
      cut<-suff[1]
      x<-gsub(cut,"",x,fixed = TRUE)}

    return(x)
    }
