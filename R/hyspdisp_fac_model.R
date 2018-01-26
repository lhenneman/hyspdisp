#' Run Hysplit dispersion, save data, link to zips
#'
#' \code{hyspdisp_fac_model}  takes multiple inputs and
#' outputs counts of particles per zip code.
#'
#' @param dh row numbers of \code{date_ref_h}.
#' @param date_ref_h table of parameters for the dispersion run.
#' @param unit emissions unit information.
#' @param species_param particle species information.
#' @param npart number of particles emitted per hour.
#' @param current_dir current directory.
#' @param prc_dir run directory.
#' @param zcta2 zip code \code{SpatialPolygonsDataFrame} object.
#' Expected variables are:
#' \enumerate{
#'   \item ZCTA5CE10
#' }
#' @param crosswalk ZIP - ZCTA crosswalk file
#' @param pbl_hts planetary boundary layer heights by zip code
#' Expected variables are:
#' \enumerate{
#'   \item ZIP (zipcode)
#'   \item hpbl (PBL height in same units as particle heights)
#' }
#' @param p4s proj4string consistent across spatial objects
#' @return This function returns a data table of zip codes with associated number of particles.


hyspdisp_fac_model <- function(dh,
                               date_ref_h,
                               unit,
                               species_param,
                               npart,
                               current_dir,
                               prc_dir,
                               zcta2,
                               crosswalk,
                               hpbl_file){
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
  if( output_file %ni% tmp.exists){
    ## Create run directory
    run_dir <- file.path(prc_dir, dh)
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
        pdiam = 0, # okay
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
        met_dir = "/nfs/home/C/cchoirat/shared_space/ci3_nsaph/projects/hysplit/weather_data/",
        binary_path = "/nfs/home/C/cchoirat/shared_space/ci3_l_zigler/software/hysplit/trunk/exec/hycs_std") %>%
      run_model(npart = npart)

    dispersion_df <-
      dispersion_model %>% get_output_df() %>% data.table()

    ## trim particles if they go below zero
    disp_df <- trim_zero(dispersion_df)

    ## Add parcel date and time
    disp_df$Pdate <- date_ref$start_day + disp_df$hour / 24

    # trims particles that are above the global max boundary value
    disp_df_trim <- disp_df[height <= max(pbl_hts$hpbl)]

    ## Move back to main directory
    setwd(current_dir)
    print(current_dir)

    ## Save R data frame
    save.vars <- c('lon', 'lat', 'height', 'Pdate')
    write.csv(disp_df_trim[,save.vars, with = F], output_file)

    ## Erase run files
    unlink(run_dir, recursive = TRUE)
  } else
    disp_df <- fread(output_file)

  ## trim values above PBL
  disp_df_trim <- trim_pbl(disp_df,
                           hpbl.nc = hpbl_file)
  ## link to zips
  disp_df_link <- link_zip( disp_df_trim,
                            gridfirst = T)

  ## find fraction of particles per zip
  # tot_by_zip <- zip_count(disp_df_link)

  #   return( disp_df[,.(lon, lat, height)])
  return( disp_df_link[, .(ZIP, N)])
}
