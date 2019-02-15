rankfacs_by_popwgt_location <- function( link.ampd.files = NULL,
                                         link.dt,
                                         census.dt,
                                         cw,
                                         metrics = c( 'hyads'),
                                         zip.value = '*',
                                         state.value = '*',
                                         city.value = '*'){

  # read in HyADS link file and ampd file
  if( !is.null( link.ampd.files) & !is.null( link.ampd.files)){
    ziplinks_hyads <- fread(link.ampd.files[['hyads.file']])[, V1 := NULL]
    ziplinks_ampd  <- fread(link.ampd.files[['ampd.file']])[, V1 := NULL]

    ziplinks_hyads[, `:=` (ZIP  = formatC( ZIP, width = 5, format = "d", flag = "0"),
                           uID  = gsub( '_|-|\\*', '.', uID),
                           year = as.integer( gsub( '_.*$', '', yearmonth)))]
    ziplinks_ampd[,  `:=` (ZIP  = formatC( ZIP, width = 5, format = "d", flag = "0"),
                           uID  = gsub( '_|-|\\*', '.', uID),
                           year = as.integer( gsub( '_.*$', '', year_month)))]

    link.dt <- merge( ziplinks_hyads, ziplinks_ampd,
                      by = c( 'ZIP', 'uID', 'year'),
                      all = T)

  }

  ## Merge ZIP code and census info with link.dt
  link.dt <- merge( link.dt, cw, by = 'ZIP')
  link.dt <- merge( link.dt, census.dt, by = c( 'ZIP', 'year'))

  ## limit data table to subset.value in subset.feature
  zip.search   <- paste0( zip.value,   collapse = '|')
  state.search <- paste0( state.value, collapse = '|')
  city.search  <- paste0( city.value,  collapse = '|')

  link.dt.trim <- link.dt[ Reduce( intersect, list( grep( zip.search, ZIP),
                                                    grep( state.search, State.zip),
                                                    grep( city.search, City.zip)))]

  ## Weight metric by popultion
  names.py <- paste0( metrics, '.py')
  link.dt.trim[, (names.py) := lapply(metrics, function(x) { TOTPOP_CY * get(x)})]

  ## Sum pop-weighted metrics by uID
  names.py.sum <- paste0( metrics, '.py.sum')
  uID.pw <- link.dt.trim[, lapply(names.py, function(x) { sum( get( x))}),
                         by = c("uID", "year")]
  setnames(uID.pw, paste0( 'V', 1:length( names.py.sum)), names.py.sum)

  ## Rank facilities by metric in each year
  names.py.rank <- paste0( metrics, '.rank')
  uID.pw[, (names.py.rank) := lapply(names.py.sum, function(x) { frankv( get(x), order = -1)}),
         by = year]

  return( uID.pw)
}
