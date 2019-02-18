---
title: "hyspdisp"
author: "Lucas Henneman"
date: "3/9/2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

# ```hyspdisp```

```hyspdisp``` is an R package that runs HYSPLIT (https://ready.arl.noaa.gov/HYSPLIT.php) many times. The results can then be aggregated to ZIP code level to create national estimates of exposure from various sources.  For example, plumes from several power plants can be tracked for many days and cumulative impacts estimated. The package relies on a modified version of the ```SplitR``` package, originally developed by https://github.com/rich-iannone/SplitR.

This example walks through an example run of ```hyspdisp``` functions.

# ```hyspdisp``` workflow
### install ```hyspdisp``` from github and load 
First, download and install ```hyspdisp```
```{r}
devtools::install_github("lhenneman/hyspdisp@dev2")
library(hyspdisp)
```

### Modeling a plume with HYSPLIT from SplitR
Here we present an example of a single emissions event (i.e., a "puff") emitted from one source on 1 January, 2005 at midnight. You can track a single emissions puff of 100 particles emitted in a single hour. These commands are adapted from [SplitR](https://github.com/rich-iannone/SplitR), though ```hyspdisp``` employs an edited version of ```SplitR``` (\url{https://github.com/lhenneman/SplitR}) that allows for the number of particles to be defined explicitely. This allows for more control over runtimes of parallel runs.

Define species parameters using utility function - two ooptions, so2 and so4
```{r}
species_param <- define_species( "so2")
species_param
```

Load the included dataset of emissions, locations, and stack information for each coal facility in the United States (data from USEPA Air Markets Program Data and National Emissions inventory). Select the first unit for an example run, and display the data.
```{r}
data(units2005)
unit <- units2005[1,]
unit
```

Define run and emissions parameters. Four inputs are required, and the command ```expand.grid``` creates a table with all combinations. The four function parameters in this example are:
- four start hours throughout the day (```start_hour = c( 0, 6, 12, 18)```)
- 4 days starting on 1 January, 2005
- for emission durations of 1 hour (```duration_emiss_hours = 1```)
- air parcels tracked for 10 hours after emission (```duration_run_hours = 10```)
```{r}
## combine dates and hours
date_ref_h <- data.table( expand.grid( start_hour = c( 0, 6, 12, 18),
                                       start_day = seq.Date( from = as.Date( "2005-01-01"),
                                                             length.out = 4,
                                                             by = '1 day'),
                                       duration_emiss_hours = 1,
                                       duration_run_hours = 10))
```


The ZIP code linkage procedure requires a ZCTA-to-ZIP code Crosswalk file. ZCTAs are not exact geographic matches to ZIP codes, and multiple groups compile and maintain Crosswalk files. One example is the Crosswalk mainted by UDS Mapper. It can be retrieved and its names changed for consistency with ```hyspdisp``` functions with the following commands:
```{r}
library( openxlsx)
crosswalk <- data.table( read.xlsx( xlsxFile = "https://www.udsmapper.org/docs/zip_to_zcta_2017.xlsx"))
setnames( crosswalk, 
          old = "ZIP_CODE", 
          new = "ZIP")
```

Equally important is the ZCTA shapefile. These are available from multiple locations, including the US census website accessible using the tigris library:
```{r}
library(tigris)
options(tigris_use_cache = TRUE)
zcta <- zctas(cb = TRUE, year = 2015)
```

It is recommended to transform the ZCTA shapefile to a known projection to maintain consistency throughout the allocation process. Lat-lon projections are preferred, such as the [North American Albers Equal Area Conic](https://epsg.io/102008):
```{r}
p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
zcta2 <- spTransform( x = zcta, 
                      CRS = p4s)
```

The final input file is the monthly mean boundary layer heights from [NOAA's Earth System Research Library](https://www.esrl.noaa.gov/psd/data/gridded/data.20thC_ReanV2.monolevel.mm.html). Raster's approach with handling dates does not mesh well with the NOAA file, so my (not very satisfying) solution is to change the system time zone to UTC to ensure it is read in correctly.

```{r}
hpbl_file <- "./hpbl.mon.mean.nc"
download.file( url = "ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV2/Monthlies/gaussian/monolevel/hpbl.mon.mean.nc",
               destfile = hpbl_file)
Sys.setenv(TZ='UTC')
hpbl_rasterin <- rotate( brick( x = hpbl_file, 
                                varname = 'hpbl' ))
```

\code{hyspdisp_fac_model} will perform two tasks: run HYSPLIT and link the resulting parcel locations to corresponding ZIP codes. The following code will run HYSPLIT for 100 parcels (```npart = 100```) for the emissions event defined by the first row of ```date_ref_h``` from the unit specified by ```unit```, apply the appropriate trimming (i.e., after a parcel reaches 0m elevation and for parcels that exceed the boundary layer), and calculate ZIP code concentrations. The results and intermediate files will be saved in the current directory or as specified by ```prc_dir```. 

```{r}
singlepuff <- hyspdisp_fac_model( run_ref_tab = date_ref_h,
                                  unit = unit,
                                  species = "so2",
                                  npart = 100,
                                  zcta2 = zcta2,
                                  crosswalk = crosswalk,
                                  hpbl_raster = hpbl_rasterin,
                                  link2zip = T)
```

Because it will typically be of interest to run ```hyspdisp``` for all of the emission events in ```date_ref_h```, ```hyspdisp_fac_model_parallel``` provides syntax to allow for parallelization. However, some systems will not allow multiple cores to interact with the file system simultaneously (tested on Mac OS High Sierra). In this case, use ```lapply```.
```{r}
multipuff <- mclapply( seq_len( nrow( date_ref_h)),
                       hyspdisp_fac_model_parallel,
                       run_ref_tab = date_ref_h,
                       unit = unit,
                       species = "so2",
                       npart = 100,
                       zcta2 = zcta2,
                       crosswalk = crosswalk,
                       hpbl_raster = hpbl_rasterin,
                       link2zip = T)
```


```{r}
linked_zips <- hyspdisp_zip_link( start_date = '2005-01-03',
                                  end_date = '2005-01-05',
                                  duration_run_hours = 10,
                                  hpbl_raster = hpbl_rasterin,
                                  zcta2 = zcta2,
                                  crosswalk = crosswalk,
                                  unit = unit)

```























