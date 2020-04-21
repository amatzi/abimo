source(file = 'C:/R_Development/trunk/RScripts/_OTHERS/ABIMO_AM/ABIMO_functions_am.R')

#####Test ABIMO 2019 calculation----------------------------




#join ABIMO 2019, output and input files
file_ABIMO_out <- 'C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_output/abimo_2019_mitstrassenout.dbf'
file_ABIMO_in <- 'C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_input/abimo_2019_mitstrassen.dbf'

x.ABIMO2019.calc <- ABIMO_comb_in_out(file_ABIMO_out = file_ABIMO_out, file_ABIMO_in = file_ABIMO_in)

#order ABIMO 2019 file as official calculation on Geoportal

file_WHH_2019 <- 'C:/_UserProgData/GIS/Wasserhaushalt/Wasserhaushalt_2017.dbf'
out_file <- 'C:/Aendu_lokal/ABIMO_Paper/Daten/Karten/ABIMO_output/ABIMO_2019.dbf'


x.ABIMO2019.calc <- ABIMO_adapt_map(ABIMO_out = x.ABIMO2019.new, file_georef = file_WHH_2019, out_file = out_file)

