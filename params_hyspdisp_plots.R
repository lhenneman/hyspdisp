library(hyspdisp)
library(sf)
library(ggplot2)
library(viridis)
library(scales)
library(lubridate)

#define units number
unitnum <- '1572-2' #'10-1' #'1010-3'
outpath = '~/Dropbox/Harvard/RFMeval_Local/HYSPLIT/hyspdisp_plots/'
unit <- fread( '~/Dropbox/Harvard/ARP/inMAP/Merge_AMPD_NEI/final_merge_nei_ampd.csv')[ID == unitnum]

#load zipcode data
zcta_shapefile = "~/Dropbox/GeorgiaTech/GTResearch/Accountability_files/Shapefiles/cb_2015_us_zcta510_500k/cb_2015_us_zcta510_500k.shp"
crosswalk_csv = "~/Dropbox/Harvard/ARP/inMAP/Crosswalk/Zip_to_ZCTA_crosswalk_2015_JSI.csv"
crosswalk <- fread( crosswalk_csv)
## plot linked month
load('~//Dropbox/Harvard/RFMeval_Local/Comparisons_Intermodel/evaluate_RFMs_intermediates/zip_dataset_CBA.RData')
zip_dataset <- data.table( Master)[year %in% 2005,
                                   .(ZIP, AREA)]



location <- '~/Dropbox/Harvard/RFMeval_Local/HYSPLIT/data'
file.list <- list.files(path = location,
                        pattern = paste0('hyspdispzerofirst_',unitnum,'_2005-'),
                        full.names = T)

files.data <- lapply(file.list,
                     function(file, zipdata){
                       f <- fread(file)
                       f_m <- merge( f, zipdata, by = 'ZIP')
                       return( f_m)
                     },
                     zip_dataset)
data_annual <- rbindlist(files.data)
data_annual_zip <- data_annual[, .( N = sum(N)), by = 'ZIP']

plot_hyspdisp(hyspdisp_out = data_annual_zip[N>0],
              zc = zcta_shapefile,
              cw = crosswalk,
              outpath = outpath,
              filename = paste0('hd_',unitnum,'_2005.png'),
              legend_lims = c(0, 1), #10),
              plot.title = 'Annual Impacts, 2005',
              facility.loc = c(unit$Longitude,unit$Latitude),
              legend.text.angle = 0)

filenames <- paste0( 'hd_',unitnum,'_2005_',
                     formatC( 1:12,
                              width = 2,
                              format = "d",
                              flag = "0"),
                     '.png')
titles <- month.name
for( z in seq_along( files.data)){
  m <- formatC( z,
                width = 2,
                format = "d",
                flag = "0")
  plotdata <- files.data[[z]][N>0]
  plot_hyspdisp(hyspdisp_out = plotdata,
                zcta_shapefile,
                crosswalk,
                outpath = outpath,
                filename = filenames[z],
                legend_lims = c(0, 1.0),
                plot.title = paste(titles[z], 'Impacts, 2005'),
                facility.loc = c(unit$Longitude,unit$Latitude),
                legend.text.angle = 30)
}

