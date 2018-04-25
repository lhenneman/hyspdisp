## Retrieve arguments passed from the command line.
devtools::install_github("lhenneman/SplitR") # ref = "rce_met_dir",
devtools::install_github("lhenneman/hyspdisp", force = T) # ref = "rce_met_dir",
process <- as.integer(as.character(commandArgs(trailingOnly = TRUE)))

## Load libraries
library(hyspdisp)

## Get main working directory
current_dir <- getwd()

## Create process direcory
prc_dir <- file.path("/nfs/bigdata_nobackup_ci3/n/ci3_nsaph/scratch/scratch_hysp", process)
tmp_dir <- file.path("/nfs/bigdata_nobackup_ci3/n/ci3_nsaph/scratch/scratch_hysp", process, "tmp")
dir.create(prc_dir, showWarnings = FALSE)
dir.create(tmp_dir, showWarnings = FALSE)

## Define species
species = 'so2'

## Define number of particles
npart = 500

## Define start day
start_day = as.Date('2005-01-01')

## Read input units and select run unit
units <- fread("final_merge_nei_ampd.csv")
unit <- units[process + 1]

## define emission params
start_year <- year(start_day)
start_hour_h <- c(0, 6, 12, 18)
duration_emiss_hours_h <- 1

## define run params
duration_run_hours <- 10*24

## combine emiss params
days_in_year <- function(year)
  365 + (year %% 4 == 0) - (year %% 100 == 0) + (year %% 400 == 0)
n_runs <- days_in_year(start_year)
vec_dates <- seq.Date( start_day,
                       length.out = n_runs,
                       by = '1 day')

## combine dates and hours
date_ref_h <- data.table( expand.grid( start_hour = start_hour_h,
                                       start_day = vec_dates,
                                       duration_emiss_hours = duration_emiss_hours_h,
                                       duration_run_hours = duration_run_hours))

## define species parameters
species_param <- define_species( species)

## read shapefiles to link with zip codes
zcta_shapefile = "~/shared_space/ci3_nsaph/software/inmap/zcta/cb_2015_us_zcta510_500k.shp"
crosswalk_csv = "~/shared_space/ci3_nsaph/software/inmap/crosswalk/Zip_to_ZCTA_crosswalk_2015_JSI.csv"
p4s <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
zcta <- shapefile(zcta_shapefile)
crosswalk <- fread( crosswalk_csv)
zcta2 <- spTransform( zcta, p4s)

## link with pbl heights
hpbl_file <- 'hpbl.mon.mean.nc'

## ========================= ##
## run the model over dates
## ========================= ##
system.time(
  l <- mclapply(seq_len( nrow( date_ref_h)),
                hyspdisp_fac_model,
                date_ref_h = date_ref_h,
                unit = unit,
                species_param = species_param,
                npart = npart,
                current_dir = current_dir,
                prc_dir = prc_dir,
                zcta2 = zcta2,
                crosswalk = crosswalk,
                hpbl_file = hpbl_file)
)


## Move back to main directory
setwd(current_dir)

## Combine all particles into single data table
d <- rbindlist(l)
M_fract_by_zip <- d[, sum(N),by = ZIP]
setnames( M_fract_by_zip, "V1", "N")

## write to zip code
write.csv( M_fract_by_zip,
           paste0( "output/hyspdisp_",
                   unit$ID, "_",
                   year( date_ref_h$start_day[1]),
                   ".csv"))

## Erase run files
#unlink(prc_dir, recursive = TRUE)
# unlink(tmp_dir, recursive = TRUE)





v
