\name{colMedians}
\alias{colMedians}
\title{Derive column medians of a numerical matrix}
\description{
  Form column medians of a numerical array.
}
\usage{
colMedians(mat)
}
\arguments{
  \item{mat}{A numerical matrix.}
}
\details{
  Form column medians of a numerical array.
}
\value{
  A vector of column medians is returned.
}
\references{}
\author{Kai-Ming Chang(kaiming@gmail.com)}
\seealso{\code{\link[Biobase]{rowQ}}}
\examples{ 
A<-matrix(rnorm(30),10,3)
colMedians(A)
}
\keyword{manip}
