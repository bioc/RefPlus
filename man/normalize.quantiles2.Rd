\name{normalize.quantiles2}
\alias{normalize.quantiles2}
\title{Reference quantile normalization}
\description{
  Quantile normalization to a reference set.
}
\usage{
normalize.quantiles2(X, Reference.Quantiles)
}
\arguments{
  \item{X}{A matrix of probe intensity data to be reference quantile 
    normalized.}
  \item{Reference.Quantiles}{A vector of the reference quantiles that the probe 
    intensities of a sample is normalized to.}
}
\details{
  The function quantile normalized the probe intensities of a set of microarrays
  to a set of reference quantiles which are formed by a set of reference 
  microarrays. 
}
\value{
  The reference quantile normalized probe intensities.
}
\references{Chang,K.M., Harbron,C., South,M.C. (2006) An Exploration of 
  Extensions to the RMA Algorithm. Available with the RefPlus package.}
\author{ Kai-Ming Chang(kaiming@gmail.com) }
\seealso{\code{\link[preprocessCore]{normalize.quantiles}}}
\examples{
A<-matrix(rnorm(30),10,3)
core<-1:10
An<-normalize.quantiles2(A,core)
rank(A[,1])==An[,1]
}
\keyword{manip}


