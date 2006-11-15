\name{RefPlus-package}
\alias{RefPlus-package}
\alias{RefPlus}
\docType{package}
\title{
RMA based on reference microarrays: RMA+ and RMA++ methods
}
\description{
RMA+ is an extenstion of the RMA algorithm that calculates the probeset 
intensities of a microarray using a pre-stored RMA model fitted on previously 
obtained microarrays, e.g. reference microarrays. RMA++ is a further extension 
based on the RMA+ method. This package depends on the affyPLM package. 
}

\details{
\tabular{ll}{
Package: \tab RefPlus\cr
Type: \tab Package\cr
Version: \tab 1.2.0\cr
Date: \tab 2006-10-25\cr
License: \tab GPL version 2 or newer\cr
}
Use \code{rma.para} to obtain the reference quantiles and the probe effects from
a reference set, then use \code{rmaplus} to calculate the RMA+ intensities based 
on the fitted reference quantiles and probe effects.
}
\author{
By Kai-Ming Chang(Kai-Ming.Chang@astrazeneca.com, kaiming@gmail.com)
}
\references{Chang,K.M., Harbron,C., South,M.C. (2006) An Exploration of 
  Extensions to the RMA Algorithm. (Submitted)}
\keyword{package}
\examples{
## Use affybatch.example in affy package
data(affybatch.example)

##Calculate RMA intensities using the rma function.
Ex0<-exprs(rma(affybatch.example))

## Background correct, estimate the probe effects, and calculate the 
## RMA intensities using rma.para function.
Para<-rma.para(affybatch.example,bg=TRUE,exp=TRUE)
Ex1<-Para[[3]]
 
## Calculate the RMA+ intensity using rmaplus function. 
Ex2<-rmaplus(affybatch.example, rmapara=Para, bg = TRUE)

}
