#' Trim particles that exceed pbl height
#'
#' \code{trim_pbl} saves takes as input particle position and monthly PBL heights and outputs
#' a trimmed data table.
#'
#' @param Min data table of particle positions.
#' Expected variables are:
#' \enumerate{
#'   \item ZIP (zipcode)
#'   \item Pdate (particle date)
#'   \item height (particle height)
#' }
#' @param pb planetary boundary layer heights in netcdf format (download \code{hpbl.mon.mean.nc} from:
#' https://www.esrl.noaa.gov/psd/data/gridded/data.20thC_ReanV2.monolevel.mm.html )
#' @return This function returns a trimmed dataset.

trim_pbl <- function(Min,
                     rasterin){
  Sys.setenv(TZ='UTC')
  M <- copy(Min)
  M[, ref := 1:nrow(M)]

  #Find unique month-year combinations
  M[,Pmonth := formatC(month(Pdate), width = 2, format = "d", flag = "0")]
  M[,Pyear  := formatC(year(Pdate), width = 2, format = "d", flag = "0")]
  my <- data.table( expand.grid( data.table( mo = unique(M[,Pmonth]),
                                             yr = unique(M[,Pyear]))))

  #Convert M to spatial points data frame
  xy <- M[,.(lon, lat)]
  spdf <- SpatialPointsDataFrame(coords = xy, data = M,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  # identify cells for each parcel location
  spdf$rastercell <- cellFromXY(rasterin, spdf)
  spdf.dt <- na.omit( data.table(spdf@data))

  for( m in 1:nrow( my)){
    mon <- my[m,mo]
    yer <- my[m,yr]
    day <- paste( yer, mon, '01', sep='-')

    pbl_layer <- subset_nc_date(hpbl_brick = rasterin,
                                varname = 'hpbl',
                                vardate = day)

    spdf.dt[Pmonth %in% mon & Pyear %in% yer,
            pbl := pbl_layer[spdf.dt[Pmonth %in% mon & Pyear %in% yer, rastercell]]]
  }
  spdf.dt <- spdf.dt[height < pbl]
  return(M[spdf.dt$ref,
           .(lon, lat, height, Pdate, hour)])
}







