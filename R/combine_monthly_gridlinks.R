combine_monthly_gridlinks <- function( month_YYYYMMs,
                                       zpc_dir,
                                       rda_dir = NULL,
                                       p4s = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m",
                                       crop.extent = NULL){

  # Create directory to store RData files if it does not exist
  if( is.null( rda_dir)){
    rda_dir <- file.path( getwd(), 'rdata_hyspdisp')
    print( paste( 'No rda_dir provided. Defaulting to', rda_dir))
  }
  dir.create(rda_dir, recursive = TRUE, showWarnings = F)


  if( length( unique( substr( month_YYYYMMs, 1, 4))) > 1)
    stop('please provide only month_YYYYMMs from only one year')

  names.map <- c()

  for( ym in month_YYYYMMs){
    year.h <- substr( ym, 1, 4)
    month.m <- as.integer( substr( ym, 5, 6))
    month.h <- formatC( month.m, width = 2, format = "d", flag = "0")
    pattern <- paste0( 'gridlinks.*', year.h,'-', month.h)

    files.month <- list.files( path = zpc_dir,
                               pattern = pattern,
                               full.names = T)
    if( length(files.month) == 0)
      next
    print( paste( 'Reading and merging month', month.h, 'in year', year.h))

    unitnames <- gsub( paste0( '.*gridlinks_|_', year.h, '-', month.h, '.*csv$'),
                       '', files.month)
    names(files.month) <- unitnames

    data.h <- lapply( seq_along(files.month),
                      function( i,
                                files){
                        d <- fread( files[i],
                                    drop = 'V1')
                        setnames( d, 'hyspdisp', names(files)[i])
                        # d[, `:=` (uID = names(files)[i] )]

                        r <- rasterFromXYZ( d)
                        if( !is.null( crop.extent))
                          r <- crop( r, crop.extent)
                        
                        return(r)
                      },
                      files.month)

    #calculate consistent extent
    data.h.e <- extent( Reduce( extend, data.h))

    #apply extent to all rasters, brick it!
    data.h <- lapply( data.h, extend, data.h.e)
    MergedDT <- brick( data.h)
    #crs( MergedDT) <- CRS( p4s)

    # assign to mappings
    name.map <- paste0("MAP", month.m, ".", year.h)
    names.map <- append( names.map, name.map)
    assign( name.map,
            MergedDT)
    rm( "MergedDT")
  }

  # gather output
  out.r <- mget(names.map)

  #calculate consistent extent
  out.e <- extent( Reduce( extend, out.r)) #lapply( out.r, extent)

  #apply extent to all rasters
  out.b <- lapply( out.r, extend, out.e)

  #convert to data.table
  out.dt <- lapply( out.b, function( x) data.table( rasterToPoints( x)))

  #round to nearest meter to ease re-rasterizing later
  out.dt <- lapply( out.dt,
                    function( dt)
                      dt[, `:=`( x = round( x),
                                 y = round( y))])

  #extract from list
  lapply( names( out.dt),
          function( x, l){
            names( l[[x]]) <- gsub( '^X', '', names( l[[x]]))
            assign( x, l[[x]], envir = parent.env( environment()))},
          out.dt)

  # save to rdata file
  rda.filename <- file.path( rda_dir, paste0('hyspdisp_grid_', year.h, '.RData'))
  save( list = names.map, file = rda.filename)

  print( paste("Monthly RData file written to", rda.filename))
  return( mget( names.map))
}

