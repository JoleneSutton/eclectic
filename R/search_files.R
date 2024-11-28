#' Search for a pattern within text files
#'
#' Returns names of files whose text contains a pattern
#' @param pattern Case sensitive pattern to look for
#' @param files Text files to search (e.g., .R, .txt...)
#' @seealso readLines, grep
#' @examples
#' #files<-list.files(path="...",full.names=TRUE)
#' #search_files('some text',files)
#' @export
search_files<-function(pattern,files){
  #pattern is case sensitive

  my.list<-list()

  for(i in 1:length(files)){
    scrub<-suppressWarnings(grep(pattern, readLines(paste0("",files[i])), value = TRUE))
    out<-sub('\\.*$', '', basename(files[i]))
    if(length(scrub>0)){my.list[[i]]<-out}
  }

  if(length(my.list)>0){
  my.list<-my.list[-which(sapply(my.list, is.null))]
  }

  my.list<-do.call("rbind", my.list)
  return(my.list)
}




