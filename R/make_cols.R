#' Add new columns to a data frame
#'
#' Useful for preparing data for facet plots, especially when combining many data sets or adding many columns
#' @param x Data frame
#' @param new.cols Matrix or data frame of values of new columns.
#' @examples
#' x<-data.frame(rnorm(10))
#' names(x)<-'est'
#' new.cols<-cbind('Landings','Prediction')
#' colnames(new.cols)<-list('source','response')
#' make_cols(x,new.cols)
#' @export
make_cols<-function(x,new.cols){

  j<-ncol(x)

  for(i in 1:length(new.cols)){
    x[,j+i]<-new.cols[,i]
    names(x)[j+i]<-colnames(new.cols)[i]
    }

  return(x)
}



