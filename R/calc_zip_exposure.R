#======================================================================#
#define function to calculate total zip-facility exposure relationships
#======================================================================#
calc_zip_exposure <- function(year.E,
                              year.H,
                              pollutant = 'SO2..tons.',
                              units.mo,
                              rda_file = 'loaded',
                              exp_dir = NULL,
                              source.agg = c('total', 'facility', 'unit'),
                              time.agg = c('year', 'month'),
                              return.monthly.data = F){
  `%ni%` <- Negate(`%in%`)

  #define defaults if none provided
  if( length( source.agg) > 1){
    print( 'Multiple source.agg provided, deaulting to "total".')
    source.agg <- 'total'
  }
  if( source.agg %ni% c('total', 'facility', 'unit')){
    stop( 'source.agg not recognized, please provide one of c("total", "facility", "unit").')
  }
  if( length( time.agg) > 1){
    print( 'Multiple time.agg provided, deaulting to "year".')
    time.agg <- 'year'
  }
  if( time.agg %ni% c('year', 'month')){
    stop( 'time.agg not recognized, please provide one of c("year", "month").')
  }

  #define defaults if none provided
  if( rda_file != 'loaded')
    load( rda_file, envir = environment())

  map.name1 <- paste0( "MAP", 1, ".", year.H)
  if( rda_file == 'loaded'
      & map.name1 %ni% ls( envir = globalenv())
      & map.name1 %ni% ls())
    stop( paste0("Data is loaded, but does not include ", map.name1, ', which is required for now.'))

  # Create directory to store output files if it does not exist
  if( is.null( exp_dir)){
    exp_dir <- file.path( getwd(), 'rdata_hyspdisp')
    print( paste( 'No exp_dir provided. Defaulting to', exp_dir))
  }
  dir.create(exp_dir, recursive = TRUE, showWarnings = F)

  #initiate exposure data.table
  ZIPexposures <-  data.table()

  #initiate list of monthly files
  monthly.filelist <- c()

  #Iterate over months of the year
  print(paste("Calculating ZIP code exposures for HYSPLIT year ",
              year.H, " and emissions year ", year.E, "!", sep=''))
  for (i in 1:12){

    PP.units_monthly <- subset(units.mo, month == i & year == year.E)
    setnames(PP.units_monthly, pollutant, 'pollutant')

    #Aggregate unit power plant emissions to unit level
    PP_monthly <- PP.units_monthly[!duplicated(uID)]
    PP_monthly <- PP_monthly[is.na(pollutant), pollutant := 0]

    #get HYPSPLIT mappings
    map.name <- paste0( "MAP", i, ".", year.H)
    if( map.name %ni% ls( envir = globalenv())
        & map.name %ni% ls( )){
      warning( paste( map.name, 'not loaded in environment. If you want it linked, either load RData file before or specify rda_file'))
      next
    }
    if( map.name %in% ls()) {
      month_mapping <- eval( parse( text = map.name))[ZIP != 'ZIP']
    } else{
      month_mapping <- eval( parse( text = map.name),
                             envir = globalenv())[ZIP != 'ZIP']
    }

    month_mapping[is.na(month_mapping)] <- 0
    names( month_mapping) <- gsub('_|-|\\*', '.', names( month_mapping))

    #melt them to long format
    month_mapping_long <- melt(month_mapping, id.vars = c('ZIP'),
                               variable.factor = FALSE,
                               variable.name = "uID",
                               value.name = "TrajPercent")
    if( sapply( month_mapping_long, class)['TrajPercent'] == 'character')
      month_mapping_long[,`:=`( TrajPercent = as.double( TrajPercent))]

    #This is what I want - pollutant-weighted emissions trajectories
    PP.linkage <- merge( month_mapping_long, PP_monthly, by = 'uID', all.x = T)

    #  clean house
    rm( list = c('month_mapping_long', 'PP_monthly', 'month_mapping'))

    # Sum by ZIP and uID if calculating annual
    if( time.agg == 'year') {
      # define aggregation strings
      if( source.agg == 'total')
        sum.by <- c( 'ZIP')
      if( source.agg == 'facility')
        sum.by <- c( 'ZIP', 'FacID')
      if( source.agg == 'unit')
        sum.by <- c( 'ZIP', 'uID')

      # calculate exposure, label year/month
      PP.linkage[, `:=` (Exposure  = pollutant * TrajPercent)]

      # Append running data frame
      ZIPexposures <- data.table(rbind(ZIPexposures,
                                       PP.linkage[, list(Exposure = sum(Exposure)),
                                                  by = sum.by]))

      # sum over the year so far
      ZIPexposures <- ZIPexposures[, list(Exposure = sum(Exposure)),
                                   by = sum.by]
    } else {
      # define aggregation strings
      if( source.agg == 'total')
        sum.by <- c( 'ZIP', 'yearmonth')
      if( source.agg == 'facility')
        sum.by <- c( 'ZIP', 'FacID', 'yearmonth')
      if( source.agg == 'unit')
        sum.by <- c( 'ZIP', 'uID', 'yearmonth')

      # add month
      PP.linkage[, `:=` (Exposure  = pollutant * TrajPercent,
                         yearmonth = paste0( year.E, '_', formatC( i, width = 2, flag = '0')))]

      # Append running data frame
      ZIPexposures <- data.table(rbind(ZIPexposures,
                                       PP.linkage[, list(Exposure = sum(Exposure)),
                                                  by = sum.by]))[ Exposure > 0]

      setnames( ZIPexposures,
                c( 'Exposure'),
                c( 'hyads'))


      # write to file, add monthly file to list if not empty data.table
      file.mo <- file.path( exp_dir,
                            paste0( 'ZIPexposures_byunit_',
                                    paste0( year.E, '_', formatC( i, width = 2, flag = '0')),
                                    '.csv'))
      if( nrow( ZIPexposures[ZIP != '   NA']) != 0){
        write.csv(ZIPexposures[ZIP != '   NA'],
                  file = file.mo)
        monthly.filelist[i] <- file.mo
      }
      #re-initiate ZIP exposure data.table
      ZIPexposures <-  data.frame()
    }

  }

  if( time.agg == 'year'){
    setnames( ZIPexposures,
              c( 'Exposure'),
              c( 'hyads'))

    #convert 3-digit zip code to 5, add emissions and hysplit years
    ZIPexposures$ZIP <- formatC( as.integer( ZIPexposures$ZIP), width = 5, flag = "0", format = "d")
    ZIPexposures$year.E <- year.E
    ZIPexposures$year.H <- year.H
    return( ZIPexposures[ZIP != '   NA'])
  } else {
    if( return.monthly.data){
      out <- rbindlist( lapply( na.omit( monthly.filelist),
                                fread))[, V1 := NULL]
      out[,  `:=` ( ZIP = formatC( as.integer( out$ZIP), width = 5, flag = "0", format = "d"),
                    hyads = as( hyads, 'numeric'))]
      return( out[ZIP != '   NA'])
    } else
      return( monthly.filelist)
  }
}

#==============
