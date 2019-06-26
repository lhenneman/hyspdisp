combine_monthly_gridlinks <- function( month_YYYYMMs,
                                       zpc_dir,
                                       rda_dir = NULL){

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
                        return(r)
                      },
                      files.month)

    MergedDT  <- brick( data.h)

    # assign to mappings
    name.map <- paste0("MAP", month.m, ".", year.h)
    names.map <- append( names.map, name.map)
    assign( name.map,
            MergedDT)
    rm( "MergedDT")
  }

  # gather output
  out.sum <- suppressWarnings( Reduce( "+", mget(names.map)))

  extend_all <- function( rasters){
    extent( Reduce( extend, rasters))
  }

  sum_all <- function(rasters, extent){
    re = lapply(rasters, function(r){extend(r,extent)})
    Reduce("+", re)
  }

  #calculate consistent extent
  out.e <- extent( Reduce( extend, out.r)) #lapply( out.r, extent)

  #apply extent to all rasters, brick it!
  out <- lapply( out.r, extend, out.e2)
  out.b <- brick( out)

  # save to rdata file
  rda.filename <- file.path( rda_dir, paste0('hyads_grid_unwgted_', year.h, '.RData'))
  save( out.b, file = rda.filename)

  print( paste("Monthly RData file written to", rda.filename))
  return( out.b)
}


