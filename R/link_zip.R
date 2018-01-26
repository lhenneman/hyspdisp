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


link_zip <- function( d, zc = zcta2, cw = crosswalk, gridfirst = F){
  xy <- d[,.(lon, lat)]
  spdf <- SpatialPointsDataFrame(coords = xy, data = d,
                                 proj4string = CRS(proj4string(zcta2)))
  if( gridfirst == F){
    o <- over( spdf, zc)
    D <- data.table( na.omit( cbind(d, o)))
  } else {
    r <- raster(xmn = -130,
                ymn = 24,
                xmx = -60,
                ymx = 75,
                res = .1)
    r[] <- 0
    tab <- table(cellFromXY(r, spdf))

    r[as.numeric(names(tab))] <- tab
    r[r==0] <- NA
    r2 <- as(r, "SpatialGridDataFrame")
    o <- over( zcta2, r2, fn = mean)
    setnames( o, 'layer', 'N')
    D <- data.table( cbind(zc@data, o))

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
