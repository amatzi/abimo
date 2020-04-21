source("R/abimo_functions_am.R")

#path official versions
path.geoportal <- "C:/Aendu_lokal/ABIMO_Paper/Daten/Geportal"
#path own calculation
path.local <- "C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_output"


#####Test ABIMO 2019 calculation----------------------------

# load ABIMO 2019 no Ver (Berlin geoportal version)
file_name <- "Wasserhaushalt_2017_ohne_Versiegelung.dbf"
x_geoportal <- foreign::read.dbf(file.path(path.geoportal, file_name), as.is = TRUE)

# load own calculation
file_name <- "vs_2019_noimpout.dbf"
x_own <- foreign::read.dbf(file.path(path.local, file_name), as.is = TRUE)

# match order, skip SUW
index <- match(x_geoportal$schl5, x_own$CODE)

x_own_match <- x_own[index,]

# match col names to compare
names(x_geoportal) <- c("CODE", "R", "VERDUNSTUN", "ROW", "RI", names(x_geoportal[,6:16]))

# compare by BTF
diff_tab <- abimo_compare_output(x_reference = x_geoportal, x_new = x_own_match)

# compare sums




#join ABIMO 2019, output and input files
file_ABIMO_out <- 'C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_output/abimo_2019_mitstrassenout.dbf'
file_ABIMO_in <- 'C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_input/abimo_2019_mitstrassen.dbf'

x.ABIMO2019.calc <- ABIMO_comb_in_out(file_ABIMO_out = file_ABIMO_out, file_ABIMO_in = file_ABIMO_in)

#order ABIMO 2019 file as official calculation on Geoportal

file_WHH_2019 <- 'C:/_UserProgData/GIS/Wasserhaushalt/Wasserhaushalt_2017.dbf'
out_file <- 'C:/Aendu_lokal/ABIMO_Paper/Daten/Karten/ABIMO_output/ABIMO_2019.dbf'


x.ABIMO2019.calc <- ABIMO_adapt_map(ABIMO_out = x.ABIMO2019.new, file_georef = file_WHH_2019, out_file = out_file)

