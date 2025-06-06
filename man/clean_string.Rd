% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_string.R
\name{clean_string}
\alias{clean_string}
\title{Clean string}
\usage{
clean_string(
  x,
  accents = TRUE,
  spaces = TRUE,
  lowercase = TRUE,
  excess.periods = TRUE
)
}
\arguments{
\item{x}{Character string}

\item{accents}{Should accents be removed? Default = TRUE}

\item{spaces}{Should spaces be removed? Default = TRUE}

\item{lowercase}{Should all letters all be lowercase? Default = TRUE}

\item{excess.periods}{Should excess periods be removed? Default = TRUE}
}
\description{
Remove letter accents; replace spaces with periods; make all names lowercase; remove periods if they separate single characters
}
\examples{
fake<-c('    SOme DATA  here ','Données','n e b','A.S.O','.cfvn','ac.kg.tow','gh..kg.tow')
clean_string(fake)
clean_string(fake,accents=FALSE)
clean_string(fake,spaces=FALSE)
clean_string(fake,lowercase=FALSE)
clean_string(fake,excess.periods=FALSE)
}
