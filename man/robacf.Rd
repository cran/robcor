\name{robacf}
\alias{robacf}
\title{
Robust Autocovariance and Autocorrelation Function Estimation
}
\description{
Compute (and by default plot) an estimate of the autocovariance or autocorrelation function.
}
\usage{
robacf(x, lag.max = NULL, type = c("correlation", "covariance"), plot = TRUE,
       scaler = "s_FastQn", ...)
}
\arguments{
  \item{x}{
a univariate numeric time series object or a numeric vector.
}
  \item{lag.max}{
maximum lag at which to calculate the acf. Default is \code{10*log10(N)} where \code{N} is the number of observations.
Will be automatically limited to one less than the number of observations in the series.
}
  \item{type}{
character string giving the type of acf to be computed.
Allowed values are \code{"correlation"} (the default) or \code{"covariance"}.
}
  \item{plot}{
logical. If \code{TRUE} (the default) the acf is plotted.
}
  \item{scaler}{
location-scale estimator to use in the algorithm.
By default, \code{\link{s_FastQn}()} is used.
}
  \item{\dots}{
further arguments to be passed to \code{plot.acf}.
}
}
\details{
This function is a robust replacement for \code{\link[stats:acf]{acf}()}.

Note, that implementation and documentation is not finished/polished yet.
}
\value{
A list of class \code{"acf"}. For description of elements see \code{\link[stats]{acf}()}.
}
\references{
Shevlyakov, G. L., Lyubomishchenko, N. S. and Smirnov, P. O. (2013).
\emph{Some remarks on robust estimation of power spectra}.
Proceedings of the 11th International Conference on Computer Data Analysis and Modeling, Minsk, Belarus, 97--104.
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
\keyword{ ts }
