rankfacs_by_popwgt_location <- function( link.files = NULL,
                                         link.dt = NULL,
                                         census.dt,
                                         rank.by = c( 'hyads'),
                                         zip.value = '*',
                                         state.value = '*',
                                         city.value = '*',
                                         census.pop.name,
                                         census.state.name = 'STATE',
                                         census.city.name = 'PO_NAME'){

  # make sure either link.files or link.dt edist
  if( (is.null(link.files) & is.null(link.dt)) | (!is.null(link.files) & !is.null(link.dt)) )
    stop( "Please provide EITHER link.files OR link.dt")

  # make sure year column is in both data.tables
  if( ('year' %ni% names( link.dt)) | ('year' %ni% names( census.dt)) )
    stop( "link.dt and census.dt should both include a column named 'year'.")

  ## Change name of census population variable
  census.dt.use <- copy( census.dt)
  setnames( census.dt.use,
            c( census.pop.name, census.state.name, census.city.name),
            c( 'TOTPOP_CY', 'STATE', 'CITY'))

  # read in HyADS link file and ampd file
  if( !is.null( link.files)){
    link.dt <- fread(link.files)[, V1 := NULL]

    link.dt[, `:=` (ZIP  = formatC( ZIP, width = 5, format = "d", flag = "0"),
                    uID  = gsub( '_|-|\\*', '.', uID),
                    year = as.integer( gsub( '_.*$', '', yearmonth)))]
  }

  ## Merge ZIP code and census info with link.dt
  link.dt <- merge( link.dt, census.dt.use, by = c( 'ZIP', 'year'))

  ## limit data table to subset.value in subset.feature
  zip.search   <- paste0( zip.value,   collapse = '|')
  state.search <- paste0( state.value, collapse = '|')
  city.search  <- paste0( city.value,  collapse = '|')

  link.dt.trim <- link.dt[ Reduce( intersect, list( grep( zip.search, ZIP),
                                                    grep( state.search, STATE),
                                                    grep( city.search, CITY)))]

  ## Weight metric by popultion
  names.py <- paste0( rank.by, '.py')
  link.dt.trim[, (names.py) := lapply(rank.by, function(x) { TOTPOP_CY * get(x)})]

  ## Sum pop-weighted rank.by by uID
  names.py.sum <- paste0( rank.by, '.py.sum')
  uID.pw <- link.dt.trim[, lapply(names.py, function(x) { sum( get( x))}),
                         by = c("uID", "year")]
  setnames(uID.pw, paste0( 'V', 1:length( names.py.sum)), names.py.sum)

  ## Rank facilities by metric in each year
  names.py.rank <- paste0( rank.by, '.rank')
  uID.pw[, (names.py.rank) := lapply(names.py.sum, function(x) { frankv( get(x), order = -1)}),
         by = year]

  return( uID.pw)
}
