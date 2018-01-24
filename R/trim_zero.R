#' Trim particles that exceed pbl height
#'
#' \code{trim_zero}  takes as input particle heights and outputs
#' a trimmed data table.
#'
#' @param disp_df data table of particle positions.
#' Expected variables are:
#' \enumerate{
#'   \item particle_no (particle number)
#'   \item hour (hours since particle released)
#'   \item height (particle height)
#' }
#' @return This function returns a trimmed dataset.

trim_zero <- function( disp_df){
  p_zero_df <- disp_df[height == 0,]
  particles <- unique(p_zero_df$particle_no)

  for( p in particles){
    h_zero <- p_zero_df[particle_no == p, hour]
    disp_df[particle_no == p & hour >= h_zero,] <- NA
  }
  disp_df <- na.omit( disp_df)
  return(disp_df)
}
