#' Save workspace with script name and today's date
#'
#' Must execute from within script, not in console
#' @import rstudioapi
#' @param path Path to save the workspace
#' @export
save_wsp<-function(path){
  nameScript <- function(){
    pth <- rstudioapi::getActiveDocumentContext()$path
    a <- unlist(strsplit(pth, "/"))
    b<-(a[length(a)])
    c<-gsub(".R", "_", b, fixed = TRUE)
    return(paste("WS_",c,sep=""))
  }
  save.image(paste0(path,nameScript(),Sys.Date(),".RData"))
}
