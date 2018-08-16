\name{link_zip}
\alias{link_zip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Link particles to ZIP codes and take concentration
}
\description{
Takes as input particle locations, zip code spatial object, and a ZIP-ZCTA crosswalk file, and outputs a data table linking particles with zip codes. Rarely called on its own, many of the inputs default to values called by \code{hyspdisp_fac_model}.
}
\usage{
link_zip(d, zc = zcta2, cw = crosswalk, gridfirst = F, hpbl_file = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
Data table of particle positions. Expected variables are:
\enumerate{
   \item lon (particle longitude)
   \item lat (particle latitude)
 }
}
  \item{zc}{
ZIP code \code{SpatialPolygonsDataFrame} object.
 Expected variables are:
 \enumerate{
   \item ZCTA5CE10
 }
}
  \item{cw}{
ZIP - ZCTA crosswalk file. Must include columns named \code{ZIP} and \code{ZCTA}.
}
  \item{gridfirst}{
Logical. If TRUE, count parcels in a fine grid before allocating to ZIP codes. This is preferred, as allocating parcels to ZIP codes using only their locations inflates values in larger ZIP codes.
}
  \item{hpbl_file}{
monthly mean boundary layer heights from NOAA's Earth System Research Library: \url{https://www.esrl.noaa.gov/psd/data/gridded/data.20thC_ReanV2.monolevel.mm.html}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function returns a data table of zip codes that contain particles.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
