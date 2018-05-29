#' Trim particles that exceed pbl height
#'
#' \code{trim_zero}  takes as input particle heights and outputs
#' a trimmed data table.
#'
#' @param Min data table of particle positions.
#' Expected variables are:
#' \enumerate{
#'   \item particle_no (particle number)
#'   \item hour (hours since particle released)
#'   \item height (particle height)
#' }
#' @return This function returns a trimmed dataset.

trim_zero <- function( Min){
  M <- copy(Min)

  p_zero_df <- M[height == 0,]
  particles <- unique(p_zero_df$particle_no)

  for( p in particles){
    h_zero <- p_zero_df[particle_no == p, hour]
    M[particle_no == p & hour >= h_zero,] <- NA
  }
  M <- na.omit( M)
  return(M)
}
