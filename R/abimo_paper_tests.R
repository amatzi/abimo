source("R/abimo_functions_am.R")

#path official versions
path.geoportal <- "C:/Aendu_lokal/ABIMO_Paper/Daten/Geportal"
#path own calculation
path.local <- "C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_output"


#####Test ABIMO calculation----------------------------

# load ABIMO 2019 no Ver (Berlin geoportal version)
file_name <- "Wasserhaushalt_2017_ohne_Versiegelung.dbf"
x_geoportal <- foreign::read.dbf(file.path(path.geoportal, file_name), as.is = TRUE)

# load own calculation
file_name <- "vs_2019_noimp_geoportalout.dbf"
x_own <- foreign::read.dbf(file.path(path.local, file_name), as.is = TRUE)

# match order, skip SUW
index <- match(x_geoportal$schl5, x_own$CODE)

x_own_match <- x_own[index,]

# match col names to compare
names(x_geoportal) <- c("CODE", "R", "VERDUNSTUN", "ROW", "RI", names(x_geoportal[,6:16]))

# compare by BTF
diff_tab <- abimo_compare_output(x_reference = x_geoportal, x_new = x_own_match)



######Compare Files "no Imp"----------------------------------

# load Josef's input/output file
x.josef <- foreign::read.dbf("C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_Input/Daten_Josef/abimo_171201_b_novg_ges.dbf", as.is = TRUE)

# compare Josef's output to mine
diff_outputs <- abimo_compare_output(x_reference = x.josef, x_new = x_own)

# load my input file
x.own_in <- foreign::read.dbf("C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_Input/scenarios/vs_2019_noimp.dbf", as.is = TRUE)

index <- match(x.josef$CODE, x.own_in$CODE)
x.own_in <- x.own_in[index,]

#compare by colname

index <- match(names(x.josef[,2:30]), names(x.own_in))
comp_names <- names(x.own_in[,index])


pdfFile <- file.path(tempdir(), "pdf_comp_input.pdf")
kwb.utils::preparePdf(pdfFile)

for (comp in comp_names) {
  plot(x = x.josef[[comp]][1:100], y = x.own_in[[comp]][1:100], main = comp, xlab = "reference [mm]", ylab = "new run [mm]")
  max_value <- max(c(x.josef[[comp]][1:100], x.own_in[[comp]][1:100]), na.rm = TRUE)
  lines(x = (0:max_value), y = (0:max_value), col = "red")
}

dev.off()

kwb.utils::hsShowPdf(pdfFile)



