\name{trim_pbl}
\alias{trim_pbl}
\alias{trim_zero}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Trim lists of parcels
}
\description{
Omit parcels after they have reached a height of zero or are above the boundary layer height. Called as a utility by \code{hyspdisp_fac_model}.
}
\usage{
trim_zero(Min)
trim_pbl(Min, hpbl_file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Min}{
data table of particle positions. Expected variables are:
 \enumerate{
   \item particle_no (particle number)
   \item hour (hours since particle released---not required for \code{trim_zero})
   \item height (particle height)
 }}
  \item{hpbl_file}{
monthly mean boundary layer heights from NOAA's Earth System Research Library: \url{https://www.esrl.noaa.gov/psd/data/gridded/data.20thC_ReanV2.monolevel.mm.html}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns trimmed data tables with same columns as input \code{Min}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
LH & CC
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
