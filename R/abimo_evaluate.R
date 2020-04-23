source("R/abimo_functions_am.R")

###paths
#path of scenarios
path_scenarios <- "C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_output"
#paths of model input data
path_input <- 'C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_input/scenarios/'
#path of data on github
path_data <- "data/"

###load scenarios-----------------------------------------------

##scenario files
scenario_names <- c(
  "vs_2019",            # status quo 2019
  "vs_2019_noimp",      # no impervious areas
  "vs_2019_forest"      # only forest
)


##read combined input and output files
for (scenario_name in scenario_names) {
  
  #model output file
  file_name_out <- file.path(path_scenarios, paste0(scenario_name, "out.dbf"))
  
  #model input file
  file_name_in <- file.path(path_input, paste0(scenario_name, ".dbf"))
  
  #read and merge output and input files
  assign(scenario_name, 
         abimo_comb_in_out(file_ABIMO_out = file_name_out, file_ABIMO_in = file_name_in))
  
}

##differentiate groundwater recharge and interflow
for (scenario_name in scenario_names) {
  
 assign(scenario_name,
        abimo_grwater_interflow(abimo_df = eval(as.symbol(scenario_name))))
  
}


#######general comparison, forest, no imp, 2019------------------------------

##limit to BTF that are not SUW or forest

#Nutzungs-Typ Wald und SUW
typ_nutz <- read.table(file.path(path_data, "nutzungstypen_berlin.csv"), sep = ";", dec = ".", 
                       as.is = TRUE, header = TRUE, colClasses = c("integer", "character"))

typ_nutz_wald <- typ_nutz$Typ_Nutzung[typ_nutz$Typ_nutzung_klar == "Wald"]
typ_nutz_SUW <- typ_nutz$Typ_Nutzung[typ_nutz$Typ_nutzung_klar == "Gewässer"]

#index for BTF that are not forest or SUW
index_city <- which(vs_2019$NUTZUNG != typ_nutz_wald & vs_2019$NUTZUNG != typ_nutz_SUW)





##box plots

#colums to plot
plot_cols <- c("VERDUNSTUN", "RI_K", "INTERF", "ROW")

#scenarios to plot
plot_scenarios <- c(
  "vs_2019_forest",      # only forest
  "vs_2019_noimp",      # no impervious areas
  "vs_2019"            # status quo 2019
)

#assemble plot file

plot_df <- matrix(data = NA, nrow = length(vs_2019$CODE[index_city]), ncol = length(plot_cols)*length(plot_scenarios))
plot_df <- as.data.frame(plot_df)

j <- 0

for (my_col in plot_cols) {
  
  for (plot_scenario in plot_scenarios) {
    
    j = j + 1
    
    plot_df[,j] <- eval(as.symbol(plot_scenario))[[my_col]][index_city]
    
  }
  
}

#boxplot
boxplot(plot_df, col = c("dark green", "green", "grey"))



