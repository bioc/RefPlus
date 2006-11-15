\name{rma.para}
\alias{rma.para}
\title{Fitting a RMA model}
\description{
  Obtain reference quantiles and reference probe effects based on reference set 
  Train, and calculate the gene expression
}
\usage{
rma.para(Train, bg = TRUE, exp = FALSE)
}

\arguments{
  \item{Train}{ An \code{AffyBatch} object of the reference set microarrays.}
  \item{bg}{ A logical flag. If \code{True}(by default), background correct 
    \code{Train} using default \code{bg.correct.rma}. }
  \item{exp}{ A logical flag. If \code{True}, calculate the RMA measurements of
    \code{Train}. If \code{False}, return 0.}
}

\value{
  \item{Reference.Quantiles}{Reference quantiles derived from \code{Train}.}
  \item{probe.effects}{Estimated probe effects derived from \code{Train}.}
  \item{expression}{RMA measurements of \code{Train}.}
}
\references{Chang,K.M., Harbron,C., South,M.C. (2006) An Exploration of 
  Extensions to the RMA Algorithm. (Submitted)}
\author{ Kai-Ming Chang(Kai-Ming.Chang@astrazeneca.com, kaiming@gmail.com) }
\note{ The RMA procedure requires a lot of computer memory. 
}
\seealso{\code{\link{rmaplus}},\code{\link{rmaref.predict}}}
\examples{
## Use affybatch.example in Affy package
data(affybatch.example)

## Background correct, estimate the probe effects, and calculate the 
## RMA intensities using rma.para function.
Ex<-rma.para(affybatch.example,bg=TRUE,exp=TRUE)

## Calculate the rma intensities using rma function.
Ex0<-exprs(rma(affybatch.example))

plot(Ex$express[,1],Ex0[,1])
}
\keyword{manip}


