\name{rmaplus}
\alias{rmaplus}
\title{Derive RMA+ intensities}
\description{
  Calculate the RMA+ intensities using pre-stored reference quantiles and probe
  effects. The reference quantiles and the probe effects are the estimated 
  parameter values from RMAing a set of microarrays (e.g. a reference set).  
}
\usage{
rmaplus(Future, rmapara, r.q, p.e, bg = TRUE)
}
\arguments{
  \item{Future}{An \code{affybatch} object of the microarrays to be 
    pre-processed using the RMA+ methods.}
  \item{rmapara}{Output of \code{rma.para} function that the contain reference 
    quantiles and the reference probe effects.}
  \item{r.q}{The pre-stored vector of the quantiles that the 
    probe intensity data of a microarray should be normalized to.}
  \item{p.e}{A pre-stored list of probe effects. It is a probe.coefs object of 
    PLMset class in \code{affyPLM} package.}
  \item{bg}{ A logical flag. If \code{True}(by default), background correct 
    \code{Train} using default \code{bg.correct.rma}. }
}

\value{
  The RMA+ intensities of \code{Future}.
}
\references{Chang,K.M., Harbron,C., South,M.C. (2006) An Exploration of 
  Extensions to the RMA Algorithm. Available with the RefPlus package.}
\author{Kai-Ming Chang(kaiming@gmail.com)}

\seealso{\code{\link[affyPLM]{PLMset-class}},\code{\link{rma.para}},
  \code{\link{rmaref.predict}}}
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
\keyword{manip}

