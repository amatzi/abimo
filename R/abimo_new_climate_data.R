# prepare climate data for ABIMO 1991-2019

source("R/abimo_functions_am.R")
source(system.file("extdata/load_evaporation.R", package = "kwb.dwd"))

###get potential evaporation data-----------------------------------

# Base URL to potential evaporation files on DWD server
base_url <- kwb.dwd:::ftp_path_cdc("grids_germany/monthly/evapo_p")

# List data files
relative_urls <- grep(
  "\\.asc\\.gz$", kwb.dwd::list_url(base_url), value = TRUE
)

# Provide full paths
urls <- file.path(base_url, relative_urls)

# Read all files into a list of matrices  
evaporation_matrices <- lapply(urls, read_evaporation_matrix_from_url)

# Provide metadata: file name, year, month  
file_info <- data.frame(
  file = relative_urls,
  year = sapply(evaporation_matrices, kwb.utils::getAttribute, "year"),
  month = sapply(evaporation_matrices, kwb.utils::getAttribute, "month")
)

head(file_info)

str(evaporation_matrices[[1]])

# Get Berlin matrix, same size as DWD evpo matrix (Berlin grid cells set to 1, rest of cells = NA)
berlin_dwd_mask <- get_berlin_dwd_mask() 

# calculate monthly stats for Berlin
berlin_evap_monthly <- evaporation_stats(evaporation_matrices = evaporation_matrices,
                                         file_info = file_info,
                                         geo_mask = berlin_dwd_mask)

###get precipitation data---------------------------------------------

#get data for one station
path_rain_data <- 'C:/Aendu_lokal/ABIMO_Paper/Daten/DWD/Regen'
file_Dahlem_hist <- '/monatswerte_RR_00403_19500101_20181231_hist/produkt_nieder_monat_19500101_20181231_00403.txt'
file_Dahlem_recent <- '/monatswerte_RR_00403_akt/produkt_nieder_monat_20180901_20200331_00403.txt'
file_ID <- '/monatswerte_RR_00403_akt/Metadaten_Geographie_00403.txt'

rain_hist <- read.table(file.path(path_rain_data, file_Dahlem_hist), header = TRUE, 
                        sep = ';', dec = '.')
rain_recent <- read.table(file.path(path_rain_data, file_Dahlem_recent), header = TRUE, 
                          sep = ';', dec = '.')
station_meta <- read.table(file.path(path_rain_data, file_ID), header = TRUE, 
                           sep = ';', dec = '.', stringsAsFactors = FALSE)

#add columns year and month
year_month <- kwb.utils::extractSubstring("(\\d{4})(\\d{2})", rain_recent$MESS_DATUM_BEGINN, 1:2)
names(year_month) <- c('year', 'month')
year_month$year <- as.integer(year_month$year)
year_month$month <- as.integer(year_month$month)
rain_recent <- cbind(year_month, rain_recent)

year_month <- kwb.utils::extractSubstring("(\\d{4})(\\d{2})", rain_hist$MESS_DATUM_BEGINN, 1:2)
names(year_month) <- c('year', 'month')
year_month$year <- as.integer(year_month$year)
year_month$month <- as.integer(year_month$month)
rain_hist <- cbind(year_month, rain_hist)

#join historic and recent rain data
rain_all <- rbind(rain_hist, rain_recent)


Berlin_climate_monthly <- Berlin_evap_monthly[,-1]

names(Berlin_climate_monthly) <- c("year",  "month", "pev_mean",  "pev_sd",    "pev_min",   "pev_max")

rain_col <- paste0('MORR_', unique(rain_all$STATIONS_ID))

for (i in (1:length(Berlin_climate_monthly$year))) {
  
  year <- Berlin_climate_monthly$year[i]
  month <- Berlin_climate_monthly$month[i]
  
  index <- which(rain_all$year == year & rain_all$month == month)
  
  
  Berlin_climate_monthly[[rain_col]][i] <- rain_all$MO_RR[index]
  
}

Berlin_climate_monthly$MORR_403[Berlin_climate_monthly$MORR_403 == -999.0] <- 0




###assemble and write ABIMO input values-------------------

#remove last year if not completed
months_per_year <- aggregate(x = Berlin_climate_monthly$year, 
                             by = list(Berlin_climate_monthly$year), FUN = 'length')

year_skip <- months_per_year$Group.1[months_per_year[2] < 12]

Berlin_climate_monthly_wholeyears <- Berlin_climate_monthly[Berlin_climate_monthly$year != year_skip,]

#assemble ABIMO climate data
ABIMO_climate_data <- data.frame(year = unique(Berlin_climate_monthly_wholeyears$year),
                                 rain_yr = NA,
                                 rain_sum = NA,
                                 pot_ev_yr = NA,
                                 pot_ev_sum = NA)

ABIMO_climate_data$rain_yr <- aggregate(x= Berlin_climate_monthly_wholeyears$MORR_403,
                                        by = list(Berlin_climate_monthly_wholeyears$year),
                                        FUN = 'sum')[,2]
ABIMO_climate_data$pot_ev_yr <- aggregate(x= Berlin_climate_monthly_wholeyears$pev_mean,
                                          by = list(Berlin_climate_monthly_wholeyears$year),
                                          FUN = 'sum')[,2]

index_summer <- which(Berlin_climate_monthly_wholeyears$month > 2 & 
                        Berlin_climate_monthly_wholeyears$month < 9)

ABIMO_climate_data$rain_sum <- aggregate(x= Berlin_climate_monthly_wholeyears$MORR_403[index_summer],
                                        by = list(Berlin_climate_monthly_wholeyears$year[index_summer]),
                                        FUN = 'sum')[,2]
ABIMO_climate_data$pot_ev_sum <- aggregate(x= Berlin_climate_monthly_wholeyears$pev_mean[index_summer],
                                          by = list(Berlin_climate_monthly_wholeyears$year[index_summer]),
                                          FUN = 'sum')[,2]

write.csv(ABIMO_climate_data, file = 'C:/Aendu_lokal/ABIMO_Paper/Daten/DWD/Regen/ABIMO_climate_data.csv')
