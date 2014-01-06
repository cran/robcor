\name{robar}
\alias{robar}
\title{
Robust Fit Autoregressive Models to Time Series
}
\description{
Fit an autoregressive time series model to the data using robust algorithms.
}
\usage{
robar(x, order = 2, scaler = "s_FastQn")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a univariate time series.
}
  \item{order}{
an order of model to fit.
}
  \item{scaler}{
location-scale estimator to use in the algorithm.
By default, \code{\link{s_FastQn}()} is used.
}
}
\details{
This function is a robust replacement for \code{\link[stats:ar]{ar}()}.

Note, that implementation and documentation is not finished/polished yet.
}
\value{
A list of class \code{"ar"}. For description of elements see \code{\link[stats]{ar}()}.
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
\examples{
n <- 100
set.seed(361)
eps <- as.ts(rnorm(n))
x <- arima.sim(list(ar=c(1,-0.9)), n, innov=eps) # basic signal
z <- as.ts(rbinom(n, 1, 0.1) * rnorm(n, sd=10))  # noise
y <- x + z
spec.ar(robar(y, order=2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ robust }
\keyword{ ts }
