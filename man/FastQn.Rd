\name{FastQn}
\alias{FastQn}
\alias{fqn}
\alias{s_FastQn}
\title{
Robust, Efficient and Fast Scale Estimate
}
\description{
Compute the robust scale estimator FastQn, an efficient alternative to the MAD,
a fast alternative to the Qn.
}
\usage{
FastQn(x, center = median(x), scale = mad(x, center))

fqn(x, center = median(x), scale = mad(x, center))

s_FastQn(x, mu.too = FALSE, center = median(x), ...)
}
\arguments{
  \item{x}{
numeric vector of observations.
}
  \item{center}{
optionally, the center: defaults to the median.
}
  \item{scale}{
optionally, the basic scale: defaults to the median absolute deviation.
}
  \item{mu.too}{
logical indicating if the \code{center} should also be returned for \code{s_FastQn()}.
}
  \item{...}{
potentially further arguments for \code{s_FastQn()} passed to \code{FastQn()}.
}
}
\details{
This function computes one-step M-estimate of scale based on provided
robust estimate (defaults to the MAD). It gives 50\% breakdown point
and Gaussian efficiency about 80\%.

The \code{fqn} function is a shorter alias, like \code{\link{sd}} and
\code{\link{mad}}.
}
\value{
\code{FastQn()} returns a number, the FastQn robust scale estimator.

\code{s_FastQn(x, mu.too=TRUE)} returns a length-2 vector with location and scale; 
this is typically only useful for \code{\link[robustbase:covOGK]{covOGK}(*, sigmamu = s_FastQn)} or 
\code{robcor(*, scaler = s_FastQn)}.
}
\references{
Smirnov, P. O., Shevlyakov, G. L. (2010). \emph{On Approximation of the Qn-Estimate of Scale by Fast M-Estimates}.
In Book of Abstracts: International Conference on Robust Statistics (ICORS 2010) (pp. 94-95). Prague, Czech Republic.
}
\author{
Paul Smirnov <s.paul@mail.ru>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{mad}}, \code{\link[robustbase:Qn]{Qn}}.
}
\examples{
set.seed(153)
x <- sort(c(rnorm(80), rt(20, df = 1)))
s_FastQn(x, mu.too=TRUE)
FastQn(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
