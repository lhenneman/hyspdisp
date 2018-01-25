#' Trim particles that exceed pbl height
#'
#' \code{trim_pbl} saves takes as input particle position and monthly PBL heights and outputs
#' a trimmed data table.
#'
#' @param M data table of particle positions.
#' Expected variables are:
#' \enumerate{
#'   \item ZIP (zipcode)
#'   \item Pdate (particle date)
#'   \item height (particle height)
#' }
#' @param pb planetary boundary layer heights in netcdf format (download \code{hpbl.mon.mean.nc} from:
#' https://www.esrl.noaa.gov/psd/data/gridded/data.20thC_ReanV2.monolevel.mm.html )
#' @return This function returns a trimmed dataset.

trim_pbl <- function(M,
                     hpbl.nc){
  Sys.setenv(TZ='UTC')

  #get time vector to select layers
  ncin = nc_open(hpbl.nc)
  time <- ncvar_get(ncin,'time')
  time.date <- as.Date(as.POSIXct(time*3600,origin='1800-01-01 00:00'))

  #read in pbl file as raster brick
  rasterin <- rotate(brick(hpbl.nc, varname = 'hpbl' ))

  #Find unique month-year combinations
  M[,Pmonth := formatC(month(Pdate), width = 2, format = "d", flag = "0")]
  M[,Pyear  := formatC(year(Pdate), width = 2, format = "d", flag = "0")]
  my <- data.table( expand.grid( data.table( mo = unique(M[,Pmonth]),
                                             yr = unique(M[,Pyear]))))

  #Convert M to spatial points data frame
  xy <- M[,.(lon, lat)]
  spdf <- SpatialPointsDataFrame(coords = xy, data = M,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  spdf$rastercell <- cellFromXY(rasterin, spdf)
  spdf.dt <- na.omit( data.table(spdf@data))

  for( m in 1:nrow( my)){
    mon <- my[m,mo]
    yer <- my[m,yr]
    day <- paste( yer, mon, '01', sep='-')
    layer <- which( time.date == day)

    rastersub <- raster::subset(rasterin, subset = layer)
    spdf.dt[Pmonth %in% mon & Pyear %in% yer,
            pbl := rastersub[spdf.dt[Pmonth %in% mon & Pyear %in% yer, rastercell]]]
  }
  spdf.dt <- spdf.dt[height < pbl]


  return(M[as(spdf.dt$V1,'integer'),])
 }







