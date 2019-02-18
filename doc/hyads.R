## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
echo = T, 
results = 'hide',
collapse = TRUE,
comment = "#>"
)

## ----install, message = FALSE, warning = FALSE---------------------------
devtools::install_github("lhenneman/hyspdisp@dev2")
library( hyspdisp)

## ----crosswalk-----------------------------------------------------------
library( openxlsx)
crosswalkin <- data.table( read.xlsx( xlsxFile =
                                        "https://www.udsmapper.org/docs/zip_to_zcta_2017.xlsx"))
setnames( crosswalkin, 
          old = "ZIP_CODE", 
          new = "ZIP")

## ----census--------------------------------------------------------------
data( ZCTApop2017)
ZCTApop2017[, ZCTA := formatC( Id2, format = 'd', flag = '0', width = 5)]
crosswalkin <- merge( ZCTApop2017, crosswalkin,
                      by = 'ZCTA')

## ----zcta_shapefile------------------------------------------------------
# Define the storage directory, create it if it does not exist
zcta_dir <- file.path('~', 'Desktop', 'run_hyspdisp', 'zcta_500k')
zcta_file <- file.path( zcta_dir, 'cb_2017_us_zcta510_500k.zip')
dir.create( zcta_dir, 
            recursive = T, 
            showWarnings = F)

# Download the file if it is not present 
zcta_file_url <- 'ftp://ftp2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_zcta510_500k.zip'
if( !file.exists( zcta_file)){
  download.file( url = zcta_file_url,
                 destfile = zcta_file)
  unzip( zcta_file, exdir = zcta_dir)
}

# Load the ZCTA shapefile using the shapefile command
zcta_shapefile <- file.path( zcta_dir, 'cb_2017_us_zcta510_500k.shp')
zcta <- shapefile( x = zcta_shapefile)

## ----proj_shapefile------------------------------------------------------
p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
zcta.trans <- spTransform( x = zcta, 
                           CRSobj = p4s)

## ----pbl_height_download-------------------------------------------------
# Define a directory, create it if it does not exist
hpbl_dir <- file.path('~', 'Desktop', 'run_hyspdisp', 'hpbl')
dir.create( hpbl_dir, 
            recursive = T, 
            showWarnings = F)

# Download the PBL file from NOAA for years up to and including
hpbl_file_NOAA <- file.path( hpbl_dir, 'hpbl.mon.mean.nc')
url_NOAA <- "ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV2/Monthlies/gaussian/monolevel/hpbl.mon.mean.nc"
if( !file.exists( hpbl_file_NOAA))
  download.file( url = url_NOAA,
                 destfile = hpbl_file_NOAA)

# Download the PBL file from NCAR for 2013
hpbl_file_NCAR2013 <- file.path( hpbl_dir, 'hpbl.mon.mean.nc_2013.grb')
url_NCAR <- "https://rda.ucar.edu/thredds/catalog/files/e/ds630.1/e5.mnth.mean.an.sfc/2013/catalog.html?dataset=files/e/ds630.1/e5.mnth.mean.an.sfc/2013/e5.mnth.mean.an.sfc.128_159_blh.regn320sc.2013010100_2013120100.grb"
if( !file.exists( hpbl_file_NCAR2013))
  download.file( url = url_NCAR,
                 destfile = hpbl_file_NCAR2013)


## ----pbl_height_read, message = FALSE------------------------------------
Sys.setenv(TZ='UTC')
hpbl_rasterin <- brick( x = hpbl_file_NOAA, 
                                varname = 'hpbl' )

## ----select_unit---------------------------------------------------------
data( units2005)
units2005 <- data.table( units2005)[, V1 := NULL]
units.run <- units2005[ order( -SOx)][1:2]
units.run

## ----printunits, echo = FALSE, results = 'markup'------------------------
knitr::kable( units.run)

## ----out_directories,results = 'markup'----------------------------------
## proc_dir defines where runs happen and output is saved
## up_dir is where input data is saved (upper file structure)
proc_dir <- file.path( '~', 'Desktop', 'run_hyspdisp')
hysraw_dir <- file.path( proc_dir, 'output_hysplit')
ziplink_dir <- file.path( proc_dir, 'output_ziplinks')
meteo_dir <- file.path( proc_dir, 'meteo')
rdata_dir <- file.path( proc_dir, 'output_rdata')
exposure_dir <- file.path( proc_dir, 'exposure_data')

## ----met_files,results = 'markup'----------------------------------------
metfiles.vignette <- c( 'RP200412.gbl',
                        'RP200501.gbl',
                        'RP200502.gbl',
                        'RP200503.gbl',
                        'RP200504.gbl',
                        'RP200505.gbl',
                        'RP200506.gbl',
                        'RP200507.gbl')
dir.create( meteo_dir,
            recursive = T,
            showWarnings = F)
metfiles.vignette <- metfiles.vignette[!(metfiles.vignette %in% list.files(meteo_dir))]
if( length( metfiles.vignette) > 0)
    get_met_reanalysis(files = metfiles.vignette, 
                       path_met_files = meteo_dir)


## ----define_inputs-------------------------------------------------------
## combine dates and hours
input_refs <- define_inputs( units = units.run,
                             startday = '2005-01-01',
                             endday = '2005-06-30')

names( input_refs)

## ----results = 'markup', echo = F----------------------------------------
knitr::kable( names( input_refs))

## ----hyspdisp_fac--------------------------------------------------------
library(parallel)
run_sample <- round(seq( 1, dim( input_refs)[1], length.out = 20))
hysp_raw <- mclapply( X = run_sample,
                      FUN = hyspdisp_fac_model_parallel,
                      run_ref_tab = input_refs,
                      zcta2 = zcta.trans,
                      crosswalk = crosswalkin,
                      hpbl_raster = hpbl_rasterin,
                      prc_dir = proc_dir,
                      hyo_dir = hysraw_dir,
                      met_dir = meteo_dir,
                      mc.cores = 6)
hysp_raw[1]

## ----hyspdisp_fac_print, echo = FALSE, results = 'markup'----------------
library(parallel)
knitr::kable( head( hysp_raw, 1))

## ----ziplink, message = FALSE--------------------------------------------
yearmons <- paste0( 2005, 1:12)
linked_zips1 <- mclapply( yearmons,
                          hyspdisp_zip_link,
                          unit = units.run[1],
                          hpbl_raster = hpbl_rasterin,
                          zcta2 = zcta.trans,
                          crosswalk = crosswalkin,
                          prc_dir = proc_dir,
                          zpc_dir = ziplink_dir,
                          hyo_dir = hysraw_dir,
                          mc.cores = 6)

linked_zips2 <- mclapply( yearmons,
                          hyspdisp_zip_link,
                          unit = units.run[2],
                          hpbl_raster = hpbl_rasterin,
                          zcta2 = zcta.trans,
                          crosswalk = crosswalkin,
                          prc_dir = proc_dir,
                          zpc_dir = ziplink_dir,
                          hyo_dir = hysraw_dir,
                          mc.cores = 6)

## ----plot_ziplinks, results = 'markup', message = FALSE, warning = FALSE, fig.width = 7----
library(sf)
library(viridis)
library(ggplot2)
library(scales)

zips.sf <- st_read(zcta_shapefile)
setnames( zips.sf, 'ZCTA5CE10', 'ZCTA')

zips_crosswalk.sf <- merge( zips.sf, 
                            crosswalkin, 
                            by = "ZCTA", all = F, allow.cartesian = TRUE) 

zip_dataset_sf <- data.table( merge( zips_crosswalk.sf, 
                                     linked_zips1[[1]], 
                                     by = c('ZIP'), all.y = T))

ziplink_plot <- plot_hyspdisp(hyspdisp_out.sf = zip_dataset_sf,
                              metric = 'N',
                              plot.title = paste('Partial Jan. 2005 HyADS raw exposure, uID:', 
                                                 units.run[1, ID]),
                              legend.title = 'HyADS raw exposure',
                              facility.loc = data.table( x = units.run[1, Longitude],
                                                         y = units.run[1, Latitude]))
ziplink_plot

## ----combine_ziplinks----------------------------------------------------
combined_ziplinks <- combine_monthly_ziplinks( month_YYYYMMs = yearmons,
                                               zpc_dir = ziplink_dir,
                                               rda_dir = rdata_dir)
names( combined_ziplinks)

## ----combine_ziplinks_print, results = 'markup', echo = F----------------
knitr::kable( names( combined_ziplinks))

## ----loadpps-------------------------------------------------------------
data( PP.units.monthly1995_2017)

## ----exposure_ziplinks_annual, results = 'markup', message = FALSE, warning = FALSE, fig.width = 7----
zip_exp_ann <- calc_zip_exposure(rda_file = "~/Desktop/run_hyspdisp/output_rdata/hyads_unwgted_2005.RData",
                                 units.mo = PP.units.monthly1995_2017,
                                 year.E = 2005,
                                 year.H = 2005,
                                 pollutant = 'SO2.tons',
                                 source.agg = 'total',
                                 time.agg = 'year',
                                 exp_dir = exposure_dir)

zip_exp_ann_sf <- data.table( merge( zips_crosswalk.sf,
                                     zip_exp_ann,
                                     by = c('ZIP'), all.y = T))

zip_exp_ann_plot <- plot_hyspdisp(hyspdisp_out.sf = zip_exp_ann_sf,
                              metric = 'hyads',
                              plot.title = paste('2005 HyADS exposure from',
                                                 units.run[1, ID], 'and', units.run[2, ID]),
                              legend.title = 'HyADS exposure',
                              facility.loc = data.table( x = units.run[, Longitude],
                                                         y = units.run[, Latitude]))

zip_exp_ann_plot

## ----exposure_ziplinks_monthly, warning = FALSE, fig.height = 3, fig.width = 7----
zip_exp_unit_mo <- calc_zip_exposure(rda_file = "~/Desktop/run_hyspdisp/output_rdata/hyads_unwgted_2005.RData",
                                     units.mo = PP.units.monthly1995_2017,
                                     year.E = 2005,
                                     year.H = 2005,
                                     pollutant = 'SO2.tons',
                                     source.agg = 'unit',
                                     time.agg = 'month',
                                     exp_dir = exposure_dir,
                                     return.monthly.data = T)

zip_exp_unit_mo[, uID := as( uID, 'character')]
qplot( x = yearmonth,
       y = hyads,
       group = uID,
       color = uID,
       data = zip_exp_unit_mo[ZIP == 21613],
       geom = 'line')


## ----rankfacilities, warning = FALSE, fig.height = 3, fig.width = 7------
zip_exp_ann_unit <- calc_zip_exposure(year.E = 2005,
                                      year.H = 2005,
                                      pollutant = 'SO2.tons',
                                      rda_file = file.path( rdata_dir, 
                                                            "hyads_unwgted_2005.RData"),
                                      exp_dir = exposure_dir,
                                      units.mo = PP.units.monthly1995_2017,
                                      source.agg = 'unit',
                                      time.agg = 'year')


crosswalkin2005 <- copy( crosswalkin)[, year := 2005]
zip_exp_ann_unit[, year := 2005]

unitRanks2005 <- rankfacs_by_popwgt_location( link.dt = zip_exp_ann_unit,
                                              census.dt = crosswalkin2005,
                                              census.pop.name = 'Estimate; Total',
                                              rank.by = c( 'hyads'),
                                              state.value = 'PA')
unitRanks2005

## ----rankfacilities_print, results = 'markup', echo = F------------------
knitr::kable( unitRanks2005)

## ----rankfacilitiesplot, results = 'markup', fig.height = 3, fig.width = 3.4, fig.show='hold'----
unitRanks2005 <- merge( unitRanks2005, units2005, by = 'uID')
SOx_emiss_plot <- plot_ranked_facs( ranks.dt = unitRanks2005,
                                    size.var = 'SOx',
                                    size.name = "2005 SOx emissions",
                                    plot.title = NULL,
                                    xlims = c( -81, -75),
                                    ylims = c( 38.5, 42.5),
                                    dist.scalebar = 150)

SOx_emsposure_plot <- plot_ranked_facs( ranks.dt = unitRanks2005,
                                        size.var = 'hyads.py.sum',
                                        size.name = "2005 SOx HyADS exposure",
                                        plot.title = NULL,
                                        xlims = SOx_emiss_plot$latlonrange$xlim,
                                        ylims = SOx_emiss_plot$latlonrange$ylim,
                                        dist.scalebar = 150)

