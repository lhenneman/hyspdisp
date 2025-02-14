#' Run Hysplit dispersion, save data, link to zips

hyspdisp_fac_model <- function(run_ref_tab,
                               start_day,
                               start_hour,
                               duration_emiss_hours,
                               duration_run_hours,
                               unit,
                               species,
                               zcta2,
                               crosswalk,
                               hpbl_raster,
                               npart = 100,
                               overwrite = F,
                               link2zip = T,
                               prc_dir = NULL,
                               hyo_dir = NULL,
                               zpc_dir = NULL,
                               current_dir = getwd(),
                               met_dir = file.path( getwd(), 'metfiles'),
                               bin_path = NULL,
                               keep.hysplit.files = FALSE){

  # check if all required model setup parameters are entered
  has_run_ref_tab <- hasArg( run_ref_tab)
  has_individual_refs <- sum( hasArg( start_day),
                              hasArg( start_hour),
                              hasArg( duration_emiss_hours),
                              hasArg( duration_run_hours))
  if( !has_run_ref_tab & has_individual_refs < 4)
    stop( "Please include either {run_ref_tab} OR {start_day, start_hour, duration_emiss_hours, and duration_run_hours}")
  if( has_run_ref_tab & has_individual_refs == 4)
    print( "{run_ref_tab} AND {start_day, start_hour, duration_emiss_hours, and duration_run_hours} included, defaulting to data in {run_ref_tab}")
  if( has_run_ref_tab & nrow( run_ref_tab) > 1){
    print( "{run_ref_tab} has > 1 row; defaulting to the first row")
    run_ref_tab <- run_ref_tab[1,]
  }
  if( !has_run_ref_tab)
    run_ref_tab <- data.table( start_hour = start_hour,
                               start_day = start_day,
                               duration_emiss_hours = duration_emiss_hours,
                               duration_run_hours = duration_run_hours)
  
  ## Check if Height parameter in unit is NA
  if( is.na( unit$Height))
    stop("Check to make sure your Height is defined in the unit!")

  ## define species parameters
  species_param <- define_species( species)

  # Create process directory in current directory if not defined
  # Create temporary data to save output
  # Create directory to store met files if it does not exist
  if( is.null( prc_dir)){
    prc_dir <- file.path(current_dir, paste0( 'hyspdisp_', Sys.Date()))
  }
  if( is.null( hyo_dir)){
    hyo_dir <- file.path( prc_dir, 'partial_trimmed_parcel_locs')
  }
  if( is.null( zpc_dir)){
    zpc_dir <- file.path( prc_dir, 'zip_counts')
  }
  dir.create(prc_dir, showWarnings = FALSE, recursive = TRUE)
  print(hyo_dir)
  dir.create(hyo_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(zpc_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(met_dir, showWarnings = FALSE, recursive = TRUE)

  ## select date and hour
  date_ref <- run_ref_tab[1,]
  print(paste0('Date: ', format(date_ref$start_day, format = "%Y-%m-%d"), ', Hour: ', date_ref$start_hour))

  ## Define output file names
  output_file <- file.path( hyo_dir,
                            paste0("hyspdisp_",
                                   unit$ID, "_",
                                   date_ref$start_day, "_",
                                   formatC(date_ref$start_hour, width = 2, format = "d", flag = "0"),
                                   ".csv"))
  zip_output_file <- file.path( zpc_dir,
                                paste0("ziplinks_",
                                       unit$ID, "_",
                                       date_ref$start_day, "_",
                                       formatC(date_ref$start_hour, width = 2, format = "d", flag = "0"),
                                       ".csv"))

  ## Check if output parcel locations file already exists
  tmp.exists <- list.files( hyo_dir,
                            full.names = T)

  `%ni%` <- Negate(`%in%`)
  if( output_file %ni% tmp.exists | overwrite == T){
    print( "Defining HYSPLIT model parameters and running the model.")


    ## Create run directory
    run_dir <- file.path( prc_dir,
                          paste0( unit$ID, '_',
                                  paste( date_ref,
                                         collapse = '_')))

    ## preemptively remove if run_dir already exists, then create
    unlink(run_dir, recursive = T)
    dir.create(run_dir, showWarnings = FALSE)

    ## Move to run directory
    setwd(run_dir)
    print(run_dir)


    ## Define the dispersion model
    dispersion_model <-
      create_disp_model() %>%
      add_emissions(
        rate = 1,
        duration = date_ref$duration_emiss_hours,
        start_day = as( date_ref$start_day,'character'),
        start_hour = date_ref$start_hour) %>%
      add_species(
        name = species_param$name,
        pdiam = species_param$pdiam, # okay
        density = 0, # okay
        shape_factor = 0, # okay
        #resuspension = species_param$resuspension
        ddep_vel = species_param$ddep_vel) %>% # okay
      add_grid(
        range = c(0.5, 0.5),
        division = c(0.1, 0.1)) %>%
      add_params(
        lat = unit$Latitude,
        lon = unit$Longitude,
        height = unit$Height,
        duration = date_ref$duration_run_hours,
        start_day = as( date_ref$start_day,'character'),
        start_hour = date_ref$start_hour,
        direction = "forward",
        met_type = "reanalysis",
        met_dir = met_dir#,
  #      binary_path = bin_path
      ) %>%
      run_model(npart = npart)

    ## Extract output from the dispersion model
    dispersion_df <-
      dispersion_model %>% get_output_df() %>% data.table()

    ## trim particles if they go below zero
    disp_df <- trim_zero(dispersion_df)

    ## Add parcel date and time
    disp_df$Pdate <- date_ref$start_day + disp_df$hour / 24

    # trims particles that are above the global max boundary value
    disp_df_trim <- disp_df[height <= 2665]

    ## Move back to main directory
    setwd(current_dir)
    print(current_dir)

    ## Save R data frame
    save.vars <- c('lon', 'lat', 'height', 'Pdate', 'hour')
    partial_trimmed_parcel_locs <- disp_df_trim[,save.vars, with = F]
    write.csv( partial_trimmed_parcel_locs,
               output_file)
    print( paste( "Partial trimmed parcel locations (below height 0 and the highest PBL height) written to", output_file))

    ## Erase run files
    if( !keep.hysplit.files)
      unlink(run_dir, recursive = TRUE)
  } else
    partial_trimmed_parcel_locs <- fread( output_file)

  if( link2zip == T){
    print( "Linking parcel locations to ZIP codes. This could take a few minutes...")

    # Check if hpbl_raster is defined
    if( !hasArg( hpbl_raster))
      stop( "Please define a hpbl_raster file")

    # Check if crosswalk is defined
    if( !hasArg( zcta2))
      stop( "Please define a zcta2 file to link zips")

    # Check if crosswalk is defined
    if( !hasArg( crosswalk))
      stop( "Please define a crosswalk file to link zips")

    #Read output file from hysplit
    disp_df <- fread(output_file)

    #Check if extent matches the hpbl raster
    d_xmin <- min( disp_df$lon)
    e_xmin <- extent( hpbl_raster)[1]
    if( d_xmin < e_xmin)
      hpbl_raster <- rotate( hpbl_raster)

    ## trim values above PBL
    disp_df_trim <- trim_pbl(disp_df,
                             rasterin = hpbl_raster)
    ## link to zips
    disp_df_link <- link_zip( disp_df_trim,
                              gridfirst = T,
                              rasterin = hpbl_raster)

    # Write to output csv file
    write.csv( disp_df_link[, .(ZIP, N)],
               zip_output_file)
    print( paste( "ZIP code parcel counts written to", zip_output_file))

  }


  if( link2zip == F){
    out <- partial_trimmed_parcel_locs
  } else {
    out <- list( partial_trimmed_parcel_locs = partial_trimmed_parcel_locs,
                 zip_parcel_counts = disp_df_link)
  }
  return( out)

}
