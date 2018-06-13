#' Run Hysplit dispersion, save data, link to zips
#'
#' \code{hyspdisp_fac_model}  takes multiple inputs and
#' outputs counts of particles per zip code.
#'
#' @param dh row numbers of \code{date_ref_h}. If NULL (default), defaults to 1, i.e., SplitR run on first row of \code{date_ref_h}
#' @param date_ref_h table of parameters for the dispersion run.
#' @param unit emissions unit information.
#' @param species particle species. One either of 'so2' or 'so4'.
#' @param npart number of particles emitted per hour. Defaults to 100
#' @param current_dir current directory. Defaults to current directory
#' @param prc_dir run directory. Defaults to NULL. If NULL, creates a directory named prc_`dh`_TODAYSDATE
#' @param zcta2 zip code \code{SpatialPolygonsDataFrame} object.
#' Expected variables are:
#' \enumerate{
#'   \item ZCTA5CE10
#' }
#' @param crosswalk ZIP - ZCTA crosswalk file
#' @param hpbl_raster planetary boundary layer heights by zip code
#' @param link2zip overwrite files?
#' @param overwrite overwrite files?
#' @param met_dir where are the meteorological files stored
#' @return This function returns a data table of zip codes with associated number of particles.


hyspdisp_fac_model <- function(dh = NULL,
                               date_ref_h,
                               unit,
                               species,
                               npart = 100,
                               current_dir = getwd(),
                               prc_dir = NULL,
                               zcta2,
                               crosswalk,
                               hpbl_raster,
                               overwrite = F,
                               link2zip = T,
                               met_dir = getwd()){

  # Check if hpbl_raster is defined
  if( !hasArg( hpbl_raster))
    stop( "Please define a hpbl_raster file")

  ## define species parameters
  species_param <- define_species( species)

  # Create process directory in current directory if not defined
  if( is.null( prc_dir)){
    prc_dir <- file.path(current_dir, paste0( 'prc_',
                                              dh,
                                              '_',
                                              Sys.Date()))
  }
  dir.create(prc_dir, showWarnings = FALSE)


  ## select date and hour
  date_ref <- date_ref_h[dh]
  print(paste0('Date: ', format(date_ref[,2], format = "%Y-%m-%d"), ', Hour: ', date_ref[,1]))

  ## Check if output exists
  output_file <- file.path(prc_dir,
                           "tmp",
                           paste0("hyspdisp_",
                                  unit$ID, "_",
                                  date_ref$start_day, "_",
                                  formatC(date_ref$start_hour, width = 2, format = "d", flag = "0"),
                                  ".csv"))
  print(output_file)
  tmp.exists <- list.files( file.path( prc_dir, "tmp"), full.names = T)

  `%ni%` <- Negate(`%in%`)
  if( output_file %ni% tmp.exists | overwrite == T){
    ## Create run directory
    run_dir <- file.path(prc_dir, dh)
    
    ## preemptively remove if run_dir already exists, then create
    unlink(run_dir)
    dir.create(run_dir, showWarnings = FALSE)

    ## Move to run directory
    setwd(run_dir)
    print(run_dir)


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
        met_dir = met_dir,
        binary_path = "/nfs/home/C/cchoirat/shared_space/ci3_l_zigler/software/hysplit/trunk/exec/hycs_std") %>%
      run_model(npart = npart)

    dispersion_df <-
      dispersion_model %>% get_output_df() %>% data.table()

    ## trim particles if they go below zero
    disp_df <- trim_zero(dispersion_df)

    ## Add parcel date and time
    disp_df$Pdate <- date_ref$start_day + disp_df$hour / 24

    # trims particles that are above the global max boundary value
    disp_df_trim <- disp_df[height <= max( values( hpbl_raster))]

    ## Move back to main directory
    setwd(current_dir)
    print(current_dir)

    ## Save R data frame
    save.vars <- c('lon', 'lat', 'height', 'Pdate', 'hour')
    write.csv(disp_df_trim[,save.vars, with = F], output_file)

    ## Erase run files
    unlink(run_dir, recursive = TRUE)
  }

  if( link2zip == T){
    # Check if crosswalk is defined
    if( !hasArg( zcta2))
      stop( "Please define a zcta2 file to link zips")

    # Check if crosswalk is defined
    if( !hasArg( crosswalk))
      stop( "Please define a crosswalk file to link zips")


    disp_df <- fread(output_file)

    ## trim values above PBL
    disp_df_trim <- trim_pbl(disp_df,
                             rasterin = hpbl_raster)
    ## link to zips
    disp_df_link <- link_zip( disp_df_trim,
                              gridfirst = T)

    ## find fraction of particles per zip
    # tot_by_zip <- zip_count(disp_df_link)

    out <- disp_df_link[, .(ZIP, N)]
    return( out)
  }
}
