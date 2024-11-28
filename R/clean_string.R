#' Clean string
#'
#' Remove letter accents; replace spaces with periods; make all names lowercase; remove periods if they separate single characters
#' @param x Character string
#' @param accents Should accents be removed? Default = TRUE
#' @param spaces Should spaces be removed? Default = TRUE
#' @param lowercase Should all letters all be lowercase? Default = TRUE
#' @param excess.periods Should excess periods be removed? Default = TRUE
#' @importFrom stringi stri_sub stri_trans_general
#' @importFrom stringr str_sub

#' @examples
#' fake<-c('    SOme DATA  here ','Données','n e b','A.S.O','.cfvn','ac.kg.tow','gh..kg.tow')
#' clean_string(fake)
#' clean_string(fake,accents=FALSE)
#' clean_string(fake,spaces=FALSE)
#' clean_string(fake,lowercase=FALSE)
#' clean_string(fake,excess.periods=FALSE)
#' @export
clean_string<-function(x,accents=TRUE,spaces=TRUE,lowercase=TRUE,excess.periods=TRUE){

  if(accents==TRUE){x <- stringi::stri_trans_general(str = x, id = "Latin-ASCII")}#remove accents

  if(spaces==TRUE){
    x<-trimws(x)# remove leading and trailing white space
    x<-gsub("\\s+"," ",x)# remove extra white space
    x<-gsub("\\. | ",".",x,fixed = F)#replace spaces with "."
  }

  if(lowercase==TRUE){x<-tolower(x)}#change to lower case


  if(excess.periods==TRUE){
    #remove repeating periods
    x<-gsub("\\.+",".",x)

    #remove periods from strings that begin or end in periods
    index<-which(stringi::stri_sub(x,-1,-1)%in%'.'|stringi::stri_sub(x,1,1)%in%'.')
    if(length(index>0)){
      x[index]<-gsub("\\.","",x[index],fixed = F)
    }

    # remove periods from strings in which the 2nd and 4th character are periods
    # crude way of identifying strings in which every 2nd character is a period
    index2<-paste0(stringr::str_sub(x, 2, 2),stringr::str_sub(x, 4, 4))
    index3<-grep('\\..',index2)
    if(length(index3>0)){
      x[index3]<-gsub("\\.","",x[index3],fixed = F)
    }
  }
}
