#' Assign number variable to bins
#'
#' Useful before summarizing and plotting.
#' @param x A vector
#' @param bin.width The bin width. E.g., 20 will make each bin a maximum of 20 units wide
#' @return A data frame
#' @examples
#' x<-c(seq(1,23,1),seq(5,12,1))
#' bin_variable(x,5)
#' @export
bin_variable<-function(x,bin.width){
  df<-as.data.frame(x)
  STARTS<-seq(min(x),max(x),by=bin.width)
  STOPS<-STARTS+(bin.width-1)

  grp<-NA
  for(i in 1:length(STARTS)){
    index<-which(df$x%in%STARTS[i]:STOPS[i])
    grp[index]<-paste0(min(df[index,'x']),' to ', max(df[index,'x']  ))
  }
  return(cbind.data.frame(grp))
}

