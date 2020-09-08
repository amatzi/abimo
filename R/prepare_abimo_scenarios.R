# prepare scenarios for paper

source("R/abimo_functions_am.R")

#path for info files
path_data <- "data/"

#path for scenarios (in dbf format)
path_scenarios <- 'C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_input/scenarios/'

#path & filename of ABIMO input file, 2019 version
file_ABIMO_in <- 'C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_input/abimo_2019_mitstrassen.dbf'

#####2019 version--------------------------------

#load ABIMO input file, 2019 version
x_in <- foreign::read.dbf(file_ABIMO_in, as.is = TRUE)

#write 2019 basic scenario
write.dbf.abimo(df_name = x_in, new_dbf = file.path(path_scenarios, 'vs_2019/vs_2019.dbf'))


#####natural state 0, no imperviousness, geoportal version for model testing----------------------------

# no impervious areas/sewers

index <- c('VG','PROBAU','PROVGU','BELAG1','BELAG2','BELAG3','BELAG4',
           'VGSTRASSE','STR_BELAG1','STR_BELAG2','STR_BELAG3','STR_BELAG4',
           'KANAL','KAN_BEB','KAN_VGU','KAN_STR', 'STR_FLGES')

x_in_noimp_geoportal <- x_in
x_in_noimp_geoportal[, index] <- 0 

write.dbf.abimo(df_name = x_in_noimp_geoportal, new_dbf = file.path(path_scenarios, 'vs_2019_noimp_geoportal.dbf'))


#####natural state 1, no imperviousness, paper version----------------------------

# increase area to account for streets (that were set to 0)

x_in_noimp <- x_in_noimp_geoportal
x_in_noimp$FLGES <- x_in$FLGES + x_in$STR_FLGES

write.dbf.abimo(df_name = x_in_noimp, new_dbf = file.path(path_scenarios, 'vs_2019_noimp/vs_2019_noimp.dbf'))

#changed config.xml to cancel out irrigation (BER=false)
abimo_xml_BER(file_out = file.path(path_scenarios, 'vs_2019_noimp/config.xml'))


#####natural state 2, all forest----------------------------

# load types
typ_struktur <- read.table(file.path(path_data, "strukturtypen_berlin.csv"), sep = ";", dec = ".", 
                           as.is = TRUE, header = TRUE, colClasses = c("integer", "character"))
typ_nutz <- read.table(file.path(path_data, "nutzungstypen_berlin.csv"), sep = ";", dec = ".", 
                           as.is = TRUE, header = TRUE, colClasses = c("integer", "character"))

# type numbers for forest and SUW
typ_struktur_wald <- typ_struktur$Typ[typ_struktur$Typ_klar == "Wald"]
typ_nutz_wald <- typ_nutz$Typ_Nutzung[typ_nutz$Typ_nutzung_klar == "Wald"]
typ_nutz_SUW <- typ_nutz$Typ_Nutzung[typ_nutz$Typ_nutzung_klar == "Gewässer"]

# assign forest to all BKF not covered by SUW
index_SUW <- which(x_in_noimp$NUTZUNG == typ_nutz_SUW)

x_in_forest <- x_in_noimp

x_in_forest$NUTZUNG[-index_SUW] <- typ_nutz_wald
x_in_forest$TYP[-index_SUW] <- typ_struktur_wald

write.dbf.abimo(df_name = x_in_forest, new_dbf = file.path(path_scenarios, 'vs_2019_forest/vs_2019_forest.dbf'))

######climate, single years 1991-2019-----------------------------

# load annual climate data
climate_data <- read.csv(file.path(path_data, "ABIMO_climate_data.csv"))

#result_df
x_in_year <- x_in

#create one dbf input file and one config.xml per year
for (i in seq_along(climate_data$year)) {
  
  #exchange rain data 
  x_in_year$REGENJA <- as.integer(round(climate_data$rain_yr[i], digits = 0))
  x_in_year$REGENSO <- as.integer(round(climate_data$rain_sum[i], digits = 0))
  
  dbf_name <- paste0("x_in_", climate_data$year[i], ".dbf")
  dbf_path <- paste0(path_scenarios, "climate_", climate_data$year[i])
  
  if (file.exists(dbf_path)){
  } else {
    dir.create(file.path(dbf_path))
  }
  
  #write dbf file with changed rain data
  write.dbf.abimo(df_name = x_in_year, new_dbf = file.path(dbf_path, dbf_name))
  
  #write config.xml with changed potential evaporation data
  abimo_xml_evap(file_out = file.path(dbf_path, "config.xml"), 
                 evap_annual = round(climate_data$pot_ev_yr[i], digits = 0),
                 evap_summer = round(climate_data$pot_ev_sum[i], digits = 0))
  
  
}



