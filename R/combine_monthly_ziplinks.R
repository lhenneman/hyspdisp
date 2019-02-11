combine_monthly_ziplinks <- function( month_YYYYMMs,
                                      zpc_dir,
                                      rda_dir = NULL){

  # Create process directory in current directory if not defined
  # Create temporary data to save output
  # Create directory to store met files if it does not exist
  if( is.null( rda_dir)){
    rda_dir <- file.path( getwd(), 'rdata_hyspdisp')
    print( paste( 'No rda_dir provided. Defaulting to', current_dir))
  }
  dir.create(rda_dir, recursive = TRUE)


  if( length( unique( substr( month_YYYYMMs, 1, 4))) > 1)
    stop('please provide only month_YYYYMMs from only one year')

  names.map <- c()

  for( ym in month_YYYYMMs){
    year.h <- substr( ym, 1, 4)
    month.m <- as.integer( substr( ym, 5, 6))
    month.h <- formatC( month.m, width = 2, format = "d", flag = "0")
    pattern <- paste0( 'ziplinks.*', year.h,'-', month.h)

    print( paste( 'Reading and merging month', month.h, 'in year', year.h))
    files.month <- list.files( path = zpc_dir,
                               pattern = pattern,
                               full.names = T)

    unitnames <- gsub( paste0( '.*ziplinks_|_', year.h, '-', month.h, '.*csv$'),
                       '', files.month)
    names(files.month) <- unitnames

    data.h <- lapply( seq_along(files.month),
                      function( i,
                                files){
                        names.use <- c('ZIP',
                                       'hyads')
                        d <- fread( files[i],
                                    drop = 'V1',
                                    col.names = names.use)
                        d[, `:=` (ZIP = as( d$ZIP, 'character'),
                                  uID = names(files)[i] )]
                        d <- d[ hyads > 0]
                        return(d)
                      },
                      files.month)

    MergedDT  <- rbindlist( data.h)
    Merged_cast <- dcast(MergedDT,
                         ZIP ~ uID,
                         fun.aggregate = sum,
                         value.var = "hyads")

    # assign to mappings
    name.map <- paste0("MAP", month.m, ".", year.h)
    names.map <- append( names.map, name.map)
    assign( name.map,
            Merged_cast)
    rm( "MergedDT", "Merged_cast")
  }

  rda.filename <- file.path( rda_dir, paste0('hyads_unwgted_', year.h, '.RData'))
  save( names.map,
        file = rda.filename)

  return( paste("Monthly RData file written to", rda.filename))
}


