#' Assign number variable to bins
#'
#' Useful before summarizing and plotting.
#' @param x A vector
#' @param bin.width The bin width. E.g., 20 will make each bin a maximum of 20 units wide
#' @importFrom plyr round_any
#' @return A data frame
#' @examples
#' x<-c(seq(1,23,1),seq(5,12,1))
#' bin_variable(x,5)
#' @export
bin_variable<-function(x,bin.width){
  STARTS<-seq(plyr::round_any(min(x), bin.width, floor),max(x),by=bin.width)
  STOPS<-STARTS+bin.width
  LEVELS<-paste0(STARTS,' - ', round(STOPS))

  out<-matrix(ncol=2,nrow=length(x))
  dim(out)

  for(i in 1:length(STARTS)){
    index<-STARTS[i]<=x&x<STOPS[i]
    out[index,1]<-paste0(STARTS[i],' - ', STOPS[i] )
    out[index,2]<-STARTS[i]+(0.5*bin.width)
  }
  out<-as.data.frame(out)
  names(out)<-c('bin','bin.midpt')
  out$bin<-factor(out$bin,levels=LEVELS)
  return(out)
}

