\name{hyspdisp_fac_model}
\alias{hyspdisp_fac_model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run Hysplit dispersion, save data, link to zips
}
\description{
\code{hyspdisp_fac_model}  takes multiple inputs and outputs counts of particles per zip code.
}
\usage{
hyspdisp_fac_model(dh = NULL,
                   date_ref_h,
                   unit,
                   species,
                   npart = 100,
                   current_dir = getwd(),
                   prc_dir = NULL,
                   zcta2,
                   crosswalk,
                   hpbl_raster,
                   overwrite = F,
                   link2zip = T,
                   met_dir = getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dh}{
row numbers of \code{date_ref_h}. If NULL (default), defaults to 1, i.e., \code{SplitR} run on first row of \code{date_ref_h}
}
  \item{date_ref_h}{
table of parameters for the dispersion run.
}
  \item{unit}{
single emissions unit information, including columns named \code{ID}, \code{Latitude}, \code{Longitude}, \code{Height}
}
  \item{species}{
particle species. One either of 'so2' or 'so4'.
}
  \item{npart}{
number of particles emitted per hour. Defaults to 100
}
  \item{current_dir}{
working directory. Defaults to current directory
}
  \item{prc_dir}{
run directory. Defaults to \code{NULL}. If \code{NULL}, creates a directory named \code{prc_`dh`_TODAYSDATE}
}
  \item{zcta2}{
ZCTA code \code{SpatialPolygonsDataFrame} object. The expected variable name is: \code{ZCTA5CE10}. The file can be downloaded from the US census page: \url{http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_zcta510_500k.zip}.

}
  \item{crosswalk}{
ZIP - ZCTA crosswalk file. Must include columns named \code{ZIP} and \code{ZCTA}
}
  \item{hpbl_raster}{
monthly mean boundary layer heights from NOAA's Earth System Research Library: \url{https://www.esrl.noaa.gov/psd/data/gridded/data.20thC_ReanV2.monolevel.mm.html}
}
  \item{overwrite}{
Logical. Overwrite files?
}
  \item{link2zip}{
Logical. If true, calculate ZIP code concentrations using air parcel locations.
}
  \item{met_dir}{
Where are the meteorological files stored? Defaults to current directory.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
If \code{overwrite = TRUE}, \code{hyspdisp_fac_model} writes HYSPLIT output files that includes information about each parcel such as the time, date, location, and height.

If \code{link2zip = TRUE}, \code{hyspdisp_fac_model} returns a data.table with two columns named \code{ZIP} and \code{N}. \code{N} is the air parcel concentration in that ZIP code.
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
