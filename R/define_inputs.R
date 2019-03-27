#' Define species parameters
#'
#' \code{define_inputtimes}  takes as input a species name and
#' provides a table of HYSPLIT species parameters accepted by SplitR.
#'
#' @param startday starting day for HyADS runs, must be in standard YYY-MM-DD format
#' @param endday starting day for HyADS runs, must be in standard YYY-MM-DD format
#' @return This function returns a data table of run parameters accepted by hyspdisp_fac_model and hyspdisp_fac_model_parallel.

## define species parameters
define_inputs <- function( units,
                           startday,
                           endday,
                           start_hours =  c( 0, 6, 12, 18),
                           duration = 240){
  startday.date <- as.Date( startday)
  endday.date   <- as.Date( endday)

  out <- data.table( expand.grid( ID = units$ID,
                                  start_hour = start_hours,
                                  start_day = seq.Date( from = as.Date( startday.date),
                                                        to =   as.Date( endday.date),
                                                        by = '1 day'),
                                  duration_emiss_hours = 1,
                                  duration_run_hours = duration))

  out <- merge( out, units, by = 'ID')

  return(out)
}
