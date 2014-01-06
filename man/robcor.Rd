\name{robcor}
\alias{robcor}
\title{
Robust Pairwise Correlations.
}
\description{
Compute a robust estimate of the correlation coefficient or correlation matrix
via pairwise correlations.
}
\usage{
robcor(x, y = NULL, method = c("ssd", "quadrant", "mcd"), partial = FALSE,
       post = "psdcor", scaler = "s_FastQn", regress = "lmrob")
}
\arguments{
  \item{x}{
a numeric vector, matrix or data frame.
}
  \item{y}{
\code{NULL} (default) or a vector, matrix or data frame with compatible dimensions to \code{x}.
The default is equivalent to \code{y = x} (but more efficient).
}
  \item{method}{
a character string indicating which correlation coefficient is to be computed.
}
  \item{partial}{
logical. Should a partial correlation algorithm be used?
}
  \item{post}{
function to apply after the matrix is built or \code{NULL}. By default, positive semidefinite
correction is applied (\code{\link{psdcor}()}).
}
  \item{scaler}{
function to use as a location-scale estimator in \code{"ssd"} method.
By default, \code{\link{s_FastQn}()} is used.
}
  \item{regress}{
function to use as a regression estimator in partial correlations algorithm.
By default, \code{\link[robustbase]{lmrob}()} is used.
}
}
\details{
This function is a robust replacement for \code{\link[stats:cor]{cor}()}.

Note, that implementation and documentation is not finished/polished yet.
}
\value{
Either a single correlation coefficient or a correlation matrix estimate.
}
\references{
Shevlyakov, G. L., Smirnov, P. O. (2011). \emph{Robust Estimation of the Correlation Coefficient: An Attempt of Survey}.
Austrian Journal of Statistics, 40(1&2), 147-156.
}
\author{
Paul Smirnov <s.paul@mail.ru>
}
\note{
WORK-IN-PROGRESS status.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ robust }
\keyword{ multivariate }
