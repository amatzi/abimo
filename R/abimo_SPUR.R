source("R/abimo_functions_am.R")

### paths--------------------------------------

#path & filename of ABIMO input and output file, 2019 version
path_ABIMO <- 'Y:/SUW_Department/Projects/SpuR/Data-Work_packages/AP1_Modellierung/ABIMO/2019_version/'

#path for SPUR scenarios (ABIMO input files in dbf format)
path_scenarios_in <- 'Y:/SUW_Department/Projects/SpuR/Data-Work_packages/AP1_Modellierung/ABIMO/scenarios_in/'

#path for SPUR scenario results (ABIMO output files in dbf format)
path_scenarios_out <- 'Y:/SUW_Department/Projects/SpuR/Data-Work_packages/AP1_Modellierung/ABIMO/scenarios_out/'

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

#load separate output files
x_out_roofs <- foreign::read.dbf(file.path(path_scenarios_out, 'vs_2019_roofsout.dbf'), as.is = TRUE)
x_out_streets <- foreign::read.dbf(file.path(path_scenarios_out, 'vs_2019_streetsout.dbf'), as.is = TRUE)
x_out_yards <- foreign::read.dbf(file.path(path_scenarios_out, 'vs_2019_yardsout.dbf'), as.is = TRUE)

#add ROW columns
x_comb_SPUR$ROW_roof <- x_out_roofs$ROW
x_comb_SPUR$ROW_streets <- x_out_streets$ROW
x_comb_SPUR$ROW_yards <- x_out_yards$ROW

#write combined dbf
foreign::write.dbf(x_comb_SPUR, file.path(path_scenarios_out, 'vs_2019_SPUR.dbf'))
