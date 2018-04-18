#' Link particles to zip codes
#'
#' \code{link_zip}  takes as input particle locations, zip code spatial object,
#' and a ZIP-ZCTA crosswalk file, and outputs
#' a data table linking particles with zip codes.
#'
#' @param d data table of particle positions.
#' Expected variables are:
#' \enumerate{
#'   \item lon (particle longitude)
#'   \item lat (particle latitude)
#' }
#' @param zc zip code \code{SpatialPolygonsDataFrame} object.
#' Expected variables are:
#' \enumerate{
#'   \item ZCTA5CE10
#' }
#' @param cw ZIP - ZCTA crosswalk file
#' @return This function returns a data table of zip codes that contain particles.


link_zip <- function( d,
                      zc = zcta2,
                      cw = crosswalk,
                      gridfirst = F,
                      hpbl_file = NULL){
  date <- d$Pdate[1]

  xy <- d[,.( lon, lat)]
  spdf.in <- SpatialPointsDataFrame( coords = xy,
                                     data = d,
                                     proj4string = CRS( "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  spdf <- spTransform( spdf.in,
                       proj4string(zcta2))

  if( gridfirst == F){
    o <- over( spdf, zc)
    D <- data.table( na.omit( cbind(d, o)))
  } else {
    # extract data layer from raster, disaggregate to .1°x.1°
    if( is.null( hpbl_file) == T)
      stop( "Need PBL raster file!")
    pbl_layer <- subset_nc_date( hpbl_file = hpbl_file,
                                 varname = 'hpbl',
                                 vardate = date)
    pbl_layer.t <- projectRaster( pbl_layer,
                                  crs = CRS( proj4string( spdf)))
    pbl_layer.d <- disaggregate( pbl_layer.t,
                                 fact = 20)


    # count number of particles in each cell,
    # find original raster cells, divide number by pbl
    r <- pbl_layer.d
    values( r) <- NA
    cells <- cellFromXY( r, spdf)
    tab <- table( cells)
    pbls <- pbl_layer.d[as.numeric( names( tab))]
    r[as.numeric( names( tab))] <- tab / pbls

    # crop around point locations for faster extracting
    e <- extent(spdf)
    r2 <- crop( trim(r,
                     padding = 1),
                e)

    #extract average concentrations over zip codes
    or <- data.table( extract( r,
                               zcta2,
                               fun = mean,
                               na.rm = T))

    setnames( or, 'V1', 'N')
    D <- data.table( cbind( zc@data,
                            or))
  }
  setnames( D, 'ZCTA5CE10', 'ZCTA')
  cw$ZCTA <- formatC( cw$ZCTA,
                      width = 5,
                      format = "d",
                      flag = "0") # to merge on zcta ID
  M <- merge( D, cw, by = "ZCTA", all = F, allow.cartesian = TRUE) # all.x = TRUE, all.y = FALSE, allow.cartesian = TRUE)
  M[, ZIP:= formatC( ZIP,
                     width = 5,
                     format = "d",
                     flag = "0")]
  M$ZIP <- as(M$ZIP, 'character')
  M <- na.omit( M)
  return(M)
}
