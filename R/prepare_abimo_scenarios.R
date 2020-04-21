source(file = 'C:/R_Development/trunk/RScripts/_OTHERS/ABIMO_AM/ABIMO_functions_am.R')

#path for scenarios (in dbf format)
path_scenarios <- 'C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_input/scenarios/'

#load ABIMO input file, 2019 version
file_ABIMO_in <- 'C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_input/abimo_2019_mitstrassen.dbf'
x_in <- foreign::read.dbf(file_ABIMO_in, as.is = TRUE)

#write 2019 basic scenario
write.dbf.ABIMO(df_name = x_in, new_dbf = file.path(path_scenarios, 'vs_2019.dbf'))


#####natural state----------------------------

# no impervious areas/sewers

index <- c('VG','PROBAU','PROVGU','BELAG1','BELAG2','BELAG3','BELAG4',
           'VGSTRASSE','STR_BELAG1','STR_BELAG2','STR_BELAG3','STR_BELAG4',
           'KANAL','KAN_BEB','KAN_VGU','KAN_STR')

x_in_noimp <- x_in
x_in_noimp[, index] <- 0 

write.dbf.ABIMO(df_name = x_in, new_dbf = file.path(path_scenarios, 'vs_2019_noimp.dbf'))

# all forest

