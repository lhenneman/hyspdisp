\name{subset_nc_date}
\alias{subset_nc_date}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read netcdf file and extract date
}
\description{
takes as input a netcdf file location, date, and variable name and outputs the data as a raster layer.
}
\usage{
subset_nc_date(hpbl_file, varname, vardate)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hpbl_file}{
monthly mean boundary layer heights from NOAA's Earth System Research Library: \url{https://www.esrl.noaa.gov/psd/data/gridded/data.20thC_ReanV2.monolevel.mm.html}
}
  \item{varname}{
variables name to be extracted from the netcdf file
}
  \item{vardate}{
variable date to be extracted from the netcdf file
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
