\name{psdcor}
\alias{psdcor}
\title{
Positive Semidefinite Correlation Matrix Correction
}
\description{
Correct pseudo-correlation matrices to make them positive semidefinite ones.
}
\usage{
psdcor(m, method = c("higham", "eigen"), ...)
}
\arguments{
  \item{m}{
pseudo-correlation matrix to correct.
}
  \item{method}{
optionally, correction method to use.
}
  \item{\dots}{
potentially further arguments for the particular correction method.
}
}
\details{
This function applies transformation to the given matrix in order to make it
positive semidefinite correlation matrix. 

When method is \code{higham}, use Higham projections algorithm via \code{\link[Matrix:nearPD]{nearPD}()} function.

When method is \code{eigen}, use direct eigenvalues correction via \code{\link[sfsmisc:posdefify]{posdefify}()} function.
}
\value{
The corrected matrix.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Paul Smirnov <s.paul@mail.ru>
}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
