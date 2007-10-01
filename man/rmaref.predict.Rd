\name{rmaref.predict}
\alias{rmaref.predict}
\title{A function used by the rmaplus function}
\description{
  Derive RMA+ expression. Used by \code{rmaplus}. The function does not 
  background correct and normalize the probe-level data. 
}
\usage{
rmaref.predict(Future, p.e)
}
\arguments{
  \item{Future}{An \code{affybatch} object of the microarrays to be summarized 
    by the RMA+ method.}
  \item{p.e}{The pre-stored list of the probe.effects.It is a probe.coefs object 
    of PLMset class in \code{affyPLM} package.}
}
\value{
  The RMA+ intensities of \code{Future}.
}
\references{Chang,K.M., Harbron,C., South,M.C. (2006) An Exploration of 
  Extensions to the RMA Algorithm. Available with the RefPlus package.}
\author{ Kai-Ming Chang(kaiming@mail.kfcc.org.tw) }
\note{Use the \code{rmaplus} function to normalize \code{Future} to pre-stored 
  reference quantiles and correct the probe effects to obtain the RMA+ 
  intensities. 
}
\seealso{\code{\link[affyPLM]{PLMset-class}},\code{\link{rma.para}},
\code{\link{rmaplus}}}
\examples{
}
\keyword{manip}
