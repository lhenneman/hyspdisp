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
                      rasterin = NULL){

  xy <- d[,.( lon, lat)]
  spdf.in <- SpatialPointsDataFrame( coords = xy,
                                     data = d,
                                     proj4string = CRS( "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  spdf <- spTransform( spdf.in,
                       proj4string(zc))

  if( gridfirst == F){
    o <- over( spdf, zc)
    D <- data.table( na.omit( cbind(d, o)))
  } else {
    # extract data layer from raster, disaggregate to .1°x.1°
    if( is.null( rasterin) == T)
      stop( "Need PBL raster!")
    pbl_layer <- rasterin

    suppressWarnings(
      pbl_layer.t <- projectRaster( pbl_layer,
                                    crs = CRS( proj4string( spdf)))
    )
    pbl_layer.d <- disaggregate( pbl_layer.t,
                                 fact = 10)


    # count number of particles in each cell,
    # find original raster cells, divide number by pbl
    r <- pbl_layer.d
    values( r) <- NA
    cells <- cellFromXY( r, spdf)
    tab <- table( cells)
    pbls <- pbl_layer.d[as.numeric( names( tab))]
    r[as.numeric( names( tab))] <- tab / pbls

    # crop around point locations for faster extracting
    # and convert to polygons for faster extracting
    e <- extent(spdf)
    r2 <- crop( trim(r,
                     padding = 1),
                e)
    r3 <- rasterToPolygons(r2)

    #crop zip codes to only use ones over the extent
    zc_trim <- crop( zc,
                     snap = 'out',
                     e)

    zc_groups <- ceiling(seq_along(zc_trim) / 5000)

    #extract average concentrations over zip codes
    #name column as 'N', combine with zip codes
    #define function to not run out of memory
    over_fn <- function( group,
                         zc_dt,
                         groups,
                         raster_obj) {
      over( zc_dt[ groups %in% group,],
            raster_obj,
            fn = mean)
    }

    or <- data.table( rbindlist( lapply( unique( zc_groups),
                                         over_fn,
                                         zc_trim,
                                         zc_groups,
                                         r3)))



    setnames(or, names(pbl_layer), 'N')
    D <- data.table( cbind( zc_trim@data,
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
