#' Read netcdf file and extract date
#'
#' \code{subset_nc_date}  takes as input a netcdf file location, date, and variable name
#' and outputs the data as a raster layer.
#'
#' @param hpbl_file netcdf file path
#' @param varname variables name
#' @param vardate variable date
#' @return raster layer of input file, variable, and date

subset_nc_date <- function( hpbl_file = NULL,
                            hpbl_brick = NULL,
                            varname = NULL,
                            vardate){

  if( (is.null( hpbl_file)  & is.null( hpbl_brick)) |
      (!is.null( hpbl_file) & !is.null( hpbl_brick)))
    stop( "Uh oh! Please define EITHER hpbl_file OR hpbl_brick")

  Sys.setenv(TZ='UTC')

  if( !is.null( hpbl_file))
    rasterin <- rotate( brick( hpbl_file, varname = varname ))
  if( !is.null( hpbl_brick))
    rasterin <- hpbl_brick

  #get time vector to select layers
  dates <- names( rasterin)
  dates <- gsub( 'X', '', dates)
  dates <- gsub( '\\.', '-', dates)

  # Get first day of the month for vardate
  vardate_month <- as.Date( paste( year( vardate),
                          month( vardate),
                          '01',
                          sep = '-'))

  #select layer
  layer <- which( dates == vardate_month)
  if( length( layer) == 0)
    stop( "Cannot match the dates of PBL raster file. Did you set the time zone to UTC before reading it in? (Sys.setenv(TZ='UTC'))")

  rastersub <- raster::subset(rasterin, subset = layer)

  return(rastersub)
}
