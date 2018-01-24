#' Trim particles that exceed pbl height
#'
#' \code{trim_pbl} saves takes as input particle position and monthly PBL heights and outputs
#' a trimmed data table.
#'
#' @param M data table of particle positions.
#' Expected variables are:
#' \enumerate{
#'   \item ZIP (zipcode)
#'   \item Pdate (particle date)
#'   \item height (particle height)
#' }
#' @param pb planetary boundary layer heights by zip code
#' Expected variables are:
#' \enumerate{
#'   \item ZIP (zipcode)
#'   \item hpbl (PBL height in same units as particle heights)
#' }
#' @return This function returns a trimmed dataset.

trim_pbl <- function(M,
                     pb = pbl){
  M[, `:=` (ZIP   = as( ZIP, 'character'),
            month = as( month( Pdate), 'integer'),
            year  = as( year(  Pdate), 'integer'))]
  M_pbl <- merge( M, pb, by = c('ZIP', 'month', 'year'))

  ## cutoff parcels with height greater than PBL
  M_pbl <- M_pbl[height < hpbl]
  return(M_pbl)
}
