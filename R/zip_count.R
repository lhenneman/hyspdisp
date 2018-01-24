#' Count particles in zip codes
#'
#' \code{zip_count}  takes as input particle-zip code links from link_zip and
#' outputs counts of particles per zip code.
#'
#' @param M data table of Zip codes that contain particles.
#' Expected variables are:
#' \enumerate{
#'   \item ZIP (zip code numbers)
#' }
#' @return This function returns a data table of zip codes with associated number of particles.

zip_count <- function(M){
  M$ZIP <- as(M$ZIP, 'character')
  M_num_by_zip <- M[, .N,by = ZIP]
  return( M_num_by_zip)
}
