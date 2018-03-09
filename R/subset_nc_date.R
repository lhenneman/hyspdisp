#' Read netcdf file and extract date
#'
#' \code{subset_nc_date}  takes as input a netcdf file location, date, and variable name
#' and outputs the data as a raster layer.
#'
#' @param hpbl_file netcdf file path
#' @param varname variables name
#' @param vardate variable date
#' @return raster layer of input file, variable, and date

subset_nc_date <- function( hpbl_file,
                                varname,
                                vardate){

  Sys.setenv(TZ='UTC')
  rasterin <- rotate(brick(hpbl_file, varname = varname ))

  #get time vector to select layers
  dates <- names( rasterin)
  dates <- gsub( 'X', '', dates)
  dates <- as.Date( gsub( '\\.', '-', dates))

  #select layer
  layer <- which( dates == vardate)
  rastersub <- raster::subset(rasterin, subset = layer)

  return(rastersub)
}
