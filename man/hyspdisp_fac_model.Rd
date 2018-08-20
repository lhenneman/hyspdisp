\name{hyspdisp_fac_model}
\alias{hyspdisp_fac_model}
\alias{hyspdisp_fac_model_parallel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run Hysplit dispersion, save data, link to zips
}
\description{
\code{hyspdisp_fac_model} takes multiple inputs and outputs counts of particles per zip code. \code{hyspdisp_fac_model_parallel} is a parallelized version applicable for functions such as \code{mclapply}.
}
\usage{
hyspdisp_fac_model(run_ref_tab,
                   start_day,
                   start_hour,
                   duration_emiss_hours,
                   duration_run_hours,
                   unit,
                   species,
                   zcta2,
                   crosswalk,
                   hpbl_raster,
                   npart = 100,
                   overwrite = F,
                   link2zip = T,
                   prc_dir = NULL,
                   hyo_dir = NULL,
                   current_dir = getwd(),
                   met_dir = file.path( getwd(), 'metfiles'),
                   binary_path = NULL,
                   keep.hysplit.files = FALSE)

hyspdisp_fac_model_parallel(x,
                            run_ref_tab,
                            unit,
                            species,
                            zcta2,
                            crosswalk,
                            hpbl_raster,
                            npart = 100,
                            overwrite = F,
                            link2zip = T,
                            prc_dir = NULL,
                            hyo_dir = NULL,
                            current_dir = getwd(),
                            met_dir = file.path( getwd(), 'metfiles'),
                            binary_path = NULL,
                            keep.hysplit.files = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
row numbers of \code{run_ref_tab}, to be used in looping function, e.g., \code{lapply} or \code{parallel:mclapply}.
}
  \item{run_ref_tab}{
table of parameters in required in \code{hyspdisp_fac_model_parallel} and \code{hyspdisp_fac_model} if \code{start_hour}, \code{start-day}, \code{duration_emiss_hours}, and \code{duration_run hours} are not provided. Should include columns named "start_hour", "start_day", "duration_emiss_hours", and "duration_run hours".
}
  \item{start_hour}{
  numeric starting hour of dispersion event (0,23). Optional if \code{run_ref_tab} included.
}
  \item{start_day}{
  starting date of dispersion event in format YYYY-MM-DD. Optional if \code{run_ref_tab} included.
}
  \item{duration_emiss_hours}{
  numeric duration of emission event. Optional if \code{run_ref_tab} included.
}
  \item{duration_run}{
  numeric duration of simulation. Optional if \code{run_ref_tab} included.
}
  \item{unit}{
single emissions unit information, including columns named \code{ID}, \code{Latitude}, \code{Longitude}, \code{Height}.
}
  \item{species}{
particle species name as character. One either of 'so2' or 'so4'.
}
  \item{npart}{
number of particles emitted per hour. Defaults to 100.
}
  \item{current_dir}{
working directory. Defaults to current directory.
}
  \item{prc_dir}{
run directory. Defaults to \code{NULL}. If \code{NULL}, creates a directory in the \code{current_dir} named \code{hyspdisp_TODAYSDATE}.
}
  \item{hyo_dir}{
HYSPLIT output directory. Defaults to \code{NULL}. If \code{NULL}, creates a directory in the \code{prc_dir} named \code{partial_trimmed_parcel_locs}.
}
  \item{binary_path}{
path to HYSPLIT binaries. Defaults to \code{NULL}. If \code{NULL}, uses the \code{SplitR} default install location.
}
  \item{zcta2}{
ZCTA code \code{SpatialPolygonsDataFrame} object. The expected variable name is: \code{ZCTA5CE10}. The file can be downloaded from the US census page: \url{http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_zcta510_500k.zip}.
}
  \item{crosswalk}{
ZIP - ZCTA crosswalk file. Must include columns named \code{ZIP} and \code{ZCTA}
}
  \item{hpbl_raster}{
monthly mean boundary layer heights from NOAA's Earth System Research Library: \url{https://www.esrl.noaa.gov/psd/data/gridded/data.20thC_ReanV2.monolevel.mm.html}.
}
  \item{overwrite}{
Logical. Overwrite previous hysplit files?
}
  \item{link2zip}{
Logical. If true, calculate ZIP code concentrations using air parcel locations.
}
  \item{met_dir}{
Where are the meteorological files stored? Defaults to a directory named "metfiles" in the working directory.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
If \code{overwrite = TRUE}, \code{hyspdisp_fac_model} and \code{hyspdisp_fac_model_parallel} write HYSPLIT output files "[prc_dir]/[hyo_dir]/hyspdisp_[unit$ID]_[start_date]_[start_hour].csv" that includes information about each parcel such as the time, date, location, and height.

If \code{link2zip = TRUE}, \code{hyspdisp_fac_model} and \code{hyspdisp_fac_model_parallel} write output files "[prc_dir]/zip_counts/hyspdisp_[unit$ID]_[start_date]_[start_hour].csv".

If \code{link2zip = FALSE}, \code{hyspdisp_fac_model} returns a data.table of HYSPLIT parcel locations with locations trimmed below 0m and above the highest height in \code{hpbl_raster}. If \code{link2zip = FALSE}, the returned list also includes a data.table with ZIP codes and air parcel counts.

\code{hyspdisp_fac_model_parallel} returns data.tables of saved locations of the relavent files. It is recommended to run \code{hyspdisp_fac_model} first as a test.
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
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
