% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rqa_diag.R
\name{summary.chaos01.rqa}
\alias{summary.chaos01.rqa}
\title{Print all the settings and results of the RQA computation.}
\usage{
\method{summary}{chaos01.rqa}(object, ...)
}
\arguments{
\item{object}{the object of "chaos01.rqa" class, produced by fast.rqa function.}

\item{...}{further arguments passed to or from other methods.}
}
\description{
This function prints structured results of the RQA computation.
}
\examples{
vec.x <- gen.logistic(mu = 3.55, iter = 2000)

res <- fast.rqa(vec.x, dim = 3, lag = 10, eps = 0.3)
summary(res)
}
\references{
N. Marwan; M. C. Romano; M. Thiel; J. Kurths (2007). "Recurrence Plots for the Analysis of Complex Systems". Physics Reports. 438 (5-6): 237. Bibcode:2007PhR...438..237M. doi:10.1016/j.physrep.2006.11.001.
}
\seealso{
\code{\link{rqa.seq}}, \code{\link{fast.rqa}}
}
\keyword{print}
\keyword{rqa}
\keyword{summary}
