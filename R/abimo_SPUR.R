source("R/abimo_functions_am.R")

### paths--------------------------------------

#path & filename of ABIMO input and output file, 2019 version
path_ABIMO <- 'Y:/SUW_Department/Projects/SpuR/Data-Work_packages/AP1_Modellierung/ABIMO/2019_version/'

#path for SPUR scenarios (ABIMO input files in dbf format)
path_scenarios_in <- 'Y:/SUW_Department/Projects/SpuR/Data-Work_packages/AP1_Modellierung/ABIMO/scenarios_in/'

#path for SPUR scenario results (ABIMO output files in dbf format)
path_scenarios_out <- 'Y:/SUW_Department/Projects/SpuR/Data-Work_packages/AP1_Modellierung/ABIMO/scenarios_out/'

#path for OgRe concentrations
path_OgRe_Model_files <- 'Y:/SUW_Department/Projects/SpuR/Data-Work_packages/AP1_Modellierung/OgRe-Modell/data_LoadModel/'

### prepare scenarios----------------------------

##load ABIMO input file, 2019 version
x_in <- foreign::read.dbf(file.path(path_ABIMO, 'abimo_2019_mitstrassen.dbf'), as.is = TRUE)

##roofs only
x_in_roofs <- x_in

#columns to be set to zero
index <- c('PROVGU','BELAG1','BELAG2','BELAG3','BELAG4',
           'VGSTRASSE','STR_BELAG1','STR_BELAG2','STR_BELAG3','STR_BELAG4')

#set impervious areas which are not roofs to zero
x_in_roofs[,index] <- 0

#write ABIMO input file
write.dbf.abimo(df_name = x_in_roofs, new_dbf = file.path(path_scenarios_in, 'vs_2019_roofs.dbf'))

##streets only
x_in_streets <- x_in

#columns to be set to zero
index <- c('PROBAU','PROVGU','BELAG1','BELAG2','BELAG3','BELAG4')

#set impervious areas which are not roofs to zero
x_in_streets[,index] <- 0

#write ABIMO input file
write.dbf.abimo(df_name = x_in_streets, new_dbf = file.path(path_scenarios_in, 'vs_2019_streets.dbf'))

##yards only
x_in_yards <- x_in

#columns to be set to zero
index <- c('PROBAU','VGSTRASSE','STR_BELAG1','STR_BELAG2','STR_BELAG3','STR_BELAG4')

#set impervious areas which are not roofs to zero
x_in_yards[,index] <- 0

#write ABIMO input file
write.dbf.abimo(df_name = x_in_yards, new_dbf = file.path(path_scenarios_in, 'vs_2019_yards.dbf'))


###add separate runoff to output of full 2019 version------------------

##combine input and output, full 2019 version
#model output file
file_name_out <- file.path(path_ABIMO, paste0("abimo_2019_mitstrassen", "out.dbf"))

#model input file
file_name_in <- file.path(path_ABIMO, paste0("abimo_2019_mitstrassen", ".dbf"))

#read and merge output and input files
assign("x_comb_SPUR", 
       abimo_comb_in_out(file_ABIMO_out = file_name_out, file_ABIMO_in = file_name_in))

##load separate output files
x_out_roofs <- foreign::read.dbf(file.path(path_scenarios_out, 'vs_2019_roofsout.dbf'), as.is = TRUE)
x_out_streets <- foreign::read.dbf(file.path(path_scenarios_out, 'vs_2019_streetsout.dbf'), as.is = TRUE)
x_out_yards <- foreign::read.dbf(file.path(path_scenarios_out, 'vs_2019_yardsout.dbf'), as.is = TRUE)

##add ROW columns
x_comb_SPUR$ROW_roof <- x_out_roofs$ROW
x_comb_SPUR$ROW_streets <- x_out_streets$ROW
x_comb_SPUR$ROW_yards <- x_out_yards$ROW


##add OgRe Type
OgRe_Types <- read.table(file = file.path("data/", "OgRe_Typen_def.csv"), header = TRUE, sep = ";", dec = ".", as.is = TRUE)

index_typ <- match(x_comb_SPUR$TYP, OgRe_Types$Flaechentyp)

x_comb_SPUR$OgRe_Type <- OgRe_Types$OgRe_Typ_klar[index_typ]

##calculate loads by BTF

#find OgRe-Model concentrations for SPUR substances
OgRe_conc <- read.table(file = file.path(path_OgRe_Model_files, "annual_mean_conc.csv"), 
                        header = TRUE, sep = ";", dec = ".", as.is = TRUE)

SPUR_subs <- kwb.ogre::OGRE_VARIABLES()[c(26,48,55,57,65,66,67),]

index <- match(SPUR_subs$VariableName, OgRe_conc$VariableName)

OgRe_conc_SPUR <- OgRe_conc[index,]

names(OgRe_conc_SPUR)[9] <- "AND"

#calculate load for each BTF in kg/yr
index <- match(x_comb_SPUR$OgRe_Type, names(OgRe_conc_SPUR))

for (subs in OgRe_conc_SPUR$VariableName) {
  
  x_comb_SPUR[[paste0(subs,"_kg_yr")]] <- x_comb_SPUR$ROW/1000 * #runoff in m
                                          x_comb_SPUR$FLAECHE *  #area in m2
                                          as.vector(t(OgRe_conc_SPUR[which(OgRe_conc_SPUR$VariableName == subs), index])) / 1e6 #concentration in kg/m3
  
}

##order and write combined file to match map

x_comb_SPUR_map <- ABIMO_adapt_map(ABIMO_out = x_comb_SPUR, 
                out_file = file.path(path_scenarios_out, 'vs_2019_SPUR.dbf'),
                file_georef = "C:/Aendu_lokal/ABIMO_Paper/Daten/Karten/Basis_ISU5_Daten_2015/ISU5_ID.dbf")  



