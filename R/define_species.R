#' Define species parameters
#'
#' \code{define_species}  takes as input a species name and
#' provides a table of HYSPLIT species parameters accepted by SplitR.
#'
#' @param species species name character string. Current
#' accepted values are:
#' \enumerate{
#'   \item so2
#'   \item so4
#' }
#' @return This function returns a data table of species parameter accepted by SplitR.

## define species parameters
define_species <- function(species) {
  if (species == 'so2'){
    # so2
    species_param <- data.table( name = 'so2',
                                 pdiam = 0,
                                 density = 0,
                                 shape_factor = 0,
                                 resuspension = 1e-10,
                                 ddep_vel = 0.002)
  } else if (species == 'so4'){
    # so4p (particulate sulfate)
    species_param <- data.table(  name = 'so4p',
                                  pdiam = 2.5,
                                  density = 1,
                                  shape_factor = 1,
                                  resuspension = 0,
                                  ddep_vel = 0.002)
  } else
    stop( "No species defined!")
  return(species_param)
}
