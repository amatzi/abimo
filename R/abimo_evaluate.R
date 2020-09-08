source("R/abimo_functions_am.R")

###paths
#path of scenarios
path_scenarios <- "C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_output"
#paths of model input data
path_input <- 'C:/Aendu_lokal/ABIMO_Paper/Daten/ABIMO_input/scenarios/'
#path of data on github
path_data <- "data/"

#should forest be included?
include_forest <- TRUE

####general comparison, forest, no imp, 2019---------------------

###load scenarios

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
  file_name_in <- file.path(path_input, paste0(scenario_name,"/",scenario_name, ".dbf"))
  
  #read and merge output and input files
  assign(scenario_name, 
         abimo_comb_in_out(file_ABIMO_out = file_name_out, file_ABIMO_in = file_name_in))
  
}

##differentiate groundwater recharge and interflow
for (scenario_name in scenario_names) {
  
 assign(scenario_name,
        abimo_grwater_interflow(abimo_df = eval(as.symbol(scenario_name))))
  
}


##limit to BTF that are not SUW or forest

#Nutzungs-Typ Wald und SUW
typ_nutz <- read.table(file.path(path_data, "nutzungstypen_berlin.csv"), sep = ";", dec = ".", 
                       as.is = TRUE, header = TRUE, colClasses = c("integer", "character"))

typ_nutz_wald <- typ_nutz$Typ_Nutzung[typ_nutz$Typ_nutzung_klar == "Wald"]
typ_nutz_SUW <- typ_nutz$Typ_Nutzung[typ_nutz$Typ_nutzung_klar == "Gewässer"]

#index for BTF that are not forest or SUW
if (include_forest) {
  
  index_city <- which(vs_2019$NUTZUNG != typ_nutz_SUW)
  
} else {
  
  index_city <- which(vs_2019$NUTZUNG != typ_nutz_wald & vs_2019$NUTZUNG != typ_nutz_SUW)

}







###box plots

##colums to plot
plot_cols <- c("VERDUNSTUN", "RI_K", "INTERF", "ROW")

##scenarios to plot
plot_scenarios <- c(
  "vs_2019_forest",      # only forest
  "vs_2019_noimp",      # no impervious areas
  "vs_2019"            # status quo 2019
)

##assemble plot file

plot_df <- matrix(data = NA, nrow = length(vs_2019$CODE[index_city]), ncol = length(plot_cols)*length(plot_scenarios))
plot_df <- as.data.frame(plot_df)

j <- 0

for (my_col in plot_cols) {
  
  for (plot_scenario in plot_scenarios) {
    
    j = j + 1
    
    plot_df[,j] <- eval(as.symbol(plot_scenario))[[my_col]][index_city]
    
  }
  
}

##boxplot
#boxplot(plot_df, col = c("dark green", "green", "grey"))

x <- boxplot(plot_df, col = c("dark green", "green", "grey"), plot = FALSE)

for (i in (1:length(plot_df[1,]))) {

x$stats[1,i] <- quantile(plot_df[,i], probs = 0.05)
                
x$stats[5,i] <- quantile(plot_df[,i], probs = 0.95)

}

col_vec <- c("dark green", "green", "grey")

bxp(x, outline = FALSE, boxcol = col_vec, medcol = col_vec, whiskcol = col_vec, staplecol = col_vec,
    boxwex = 0.4, xaxt = "n", ylab = "water balance [mm]")
#abline(v = 0, lty = 2)
axis(side = 1, at = c(0.5, 3.5, 6.5, 9.5, 12.5), labels = FALSE)
axis(side = 1, at = c(2, 5, 8, 11), labels = c("evaporation", "infiltration", "interflow", "runoff"), las = 1, tick = FALSE)

legend(x= 8, y = 600, legend=c("forest",
                              "impervious areas",
                              "status quo (city)"), 
       pch=c(12,12,12), col = c("dark green", "green", "grey"), cex = 0.9, pt.cex=1.2, 
       xpd = TRUE, y.intersp=1, bg = "white")



####climate scenarios---------------------------------

###load scenarios


## scenario files

# load annual climate data
climate_data <- read.csv(file.path(path_data, "ABIMO_climate_data.csv"))
scenario_names <- climate_data$year

# one scenario file by year
for (i in seq_along(climate_data$year)) {
  
  scenario_names[i] <- paste0("x_in_", climate_data$year[i]) 
  
}

##read combined input and output files
for (scenario_name in scenario_names) {
  
  #model output file
  file_name_out <- file.path(path_scenarios, paste0(scenario_name, "out.dbf"))
  
  #model input file
  file_name_in <- file.path(path_input, paste0("climate_", stringr::str_sub(scenario_name, 6),"/",scenario_name, ".dbf"))
  
  #read and merge output and input files
  assign(scenario_name, 
         abimo_comb_in_out(file_ABIMO_out = file_name_out, file_ABIMO_in = file_name_in))
  
}

##differentiate groundwater recharge and interflow
for (scenario_name in scenario_names) {
  
  assign(scenario_name,
         abimo_grwater_interflow(abimo_df = eval(as.symbol(scenario_name))))
  
}

##limit to BTF that are not SUW or forest

#Nutzungs-Typ Wald und SUW
typ_nutz <- read.table(file.path(path_data, "nutzungstypen_berlin.csv"), sep = ";", dec = ".", 
                       as.is = TRUE, header = TRUE, colClasses = c("integer", "character"))

typ_nutz_wald <- typ_nutz$Typ_Nutzung[typ_nutz$Typ_nutzung_klar == "Wald"]
typ_nutz_SUW <- typ_nutz$Typ_Nutzung[typ_nutz$Typ_nutzung_klar == "Gewässer"]

#index for BTF that are not forest or SUW

if (include_forest) {
  
  index_city <- which(x_in_2019$NUTZUNG != typ_nutz_SUW)
  
} else {
  
  index_city <- which(x_in_2019$NUTZUNG != typ_nutz_wald & x_in_2019$NUTZUNG != typ_nutz_SUW)
  
}



### validation data Klaerwerksdaten
BWB_data <- read.table(file.path(path_data, "Regen_Klaerwerke_BWB.csv"), sep = ";", dec = ".", 
                       as.is = TRUE, header = TRUE)
CSO_average <- 5555823   #average rain volume that enters Berlin SUW via CSO
index <- match(BWB_data$year, climate_data$year)

#scale CSO by summer rain amount
BWB_data$CSO <- CSO_average / mean(climate_data$rain_sum[index]) * climate_data$rain_sum[index]

#calculate expected rain runoff [m3/yr] in CS area
BWB_data$rain_runoff_CS <- BWB_data$Regenmengen_KW - BWB_data$Regenmengen_aus_Trenngebiet + BWB_data$CSO

#estimate standard deviation
BWB_data$stdev_BWB <- ((BWB_data$Regenmengen_KW*0.1)^2 +
                        (BWB_data$Regenmengen_aus_Trenngebiet*0.1)^2 + 
                         (BWB_data$CSO*0.2)^2)^0.5


###plot annual 

##assemble plot files

evaporation <- matrix(nrow = length(index_city), ncol = length(scenario_names))
evaporation <- as.data.frame(evaporation)

colnames(evaporation) <- climate_data$year

runoff <- evaporation
infiltration <- evaporation
interflow <- evaporation
avg_wat_bal <- data.frame(climate_data, "evaporation" = NA,
                          "infiltration" = NA,
                          "interflow" = NA,
                          "runoff" = NA)

counter <- 0

for (scenario_name in scenario_names) {
  
  counter <- counter + 1
  evaporation[, counter] <- eval(as.symbol(scenario_name))[index_city, "VERDUNSTUN"]
  runoff[, counter] <- eval(as.symbol(scenario_name))[index_city, "ROW"]
  infiltration[, counter] <- eval(as.symbol(scenario_name))[index_city, "RI_K"]
  interflow[, counter] <- eval(as.symbol(scenario_name))[index_city, "INTERF"]
  avg_wat_bal[counter, 7:10] <- abimo_Berlin_average(abimo_df = eval(as.symbol(scenario_name))[index_city,])
  
  #matching simulation for BWB validation (in m3/yr)
  if (avg_wat_bal$year[counter] >= min(BWB_data$year) &
      avg_wat_bal$year[counter] <= max(BWB_data$year)) {
    
    index_BWB <- which(BWB_data$year == avg_wat_bal$year[counter])
    index_CS <- which(eval(as.symbol(scenario_name))[, "KANART"] == 1)
    BWB_data$abimo_calc[index_BWB] <- sum(eval(as.symbol(scenario_name))[index_CS, "ROWVOL"]) * 31.536
  }
  
}


##plot to pdf


pdfFile <- file.path(tempdir(), "abimo_comp_years.pdf")
kwb.utils::preparePdf(pdfFile, landscape = FALSE)

M <- matrix(c(1,2), ncol = 1)
layout(M, heights = c(5,5))

kwb.plot::setMargins(left=7, top = 7, bottom = 7)

##validation KW
#by year
plot(x = BWB_data$year, y = BWB_data$rain_runoff_CS/1e6, cex = 1.2, ylim = c(10,45),
     ylab = expression(paste("Runoff from combined sewer area [10"^"6","m"^"3","yr"^"-1","]")),
     xlab = "Year",
     main = "Comparison with runoff data from BWB sewage treatment plants")
points(x = BWB_data$year, y = BWB_data$abimo_calc/1e6, cex = 1.2, pch = 2, col = "blue")

#error bars (2*sigma)
x <- BWB_data$year
y_low <- (BWB_data$rain_runoff_CS - 2 * BWB_data$stdev_BWB)/1e6
y_hi <- (BWB_data$rain_runoff_CS + 2 * BWB_data$stdev_BWB)/1e6
segments(x,y_low,x,y_hi, lwd = 1)

legend(x= 2012.5, y = 45, legend=c("balance at WWTP", "simulation"), 
       pch=c(1,2), col = c("black", "blue"), cex = 0.7, pt.cex=1, 
       xpd = TRUE, y.intersp=1, bg = "white")


#scatter
plot(y = BWB_data$rain_runoff_CS/1e6, x = BWB_data$abimo_calc/1e6, 
     xlim = c(10,45), ylim = c(10,45), cex = 1.2,
     xlab = expression(paste("simulated runoff [10"^"6","m"^"3","yr"^"-1","]")),
     ylab = expression(paste("balance by Berlin water utility [10"^"6","m"^"3","yr"^"-1","]")))

abline(a = 0, b = 1, lty = "dotted")

#error bars
x <- BWB_data$abimo_calc/1e6
y_low <- (BWB_data$rain_runoff_CS - 2 * BWB_data$stdev_BWB)/1e6
y_hi <- (BWB_data$rain_runoff_CS + 2 * BWB_data$stdev_BWB)/1e6
segments(x,y_low,x,y_hi, lwd = 1)



##boxplot by year and component (city only)

M <- matrix(c(1,2,3,4), ncol = 1)
layout(M, heights = c(5,5,5,5))

kwb.plot::setMargins(left=7, top = 4, bottom = 2)

if (include_forest) {
  mytitle <- "Annual water balance by component for Berlin, excluding lakes"
} else {
  mytitle <- "Annual water balance by component for Berlin, excluding forest and lakes"
}


for (myplot in c("evaporation", "infiltration", "interflow", "runoff")) {

  x <- boxplot(eval(as.symbol(myplot)), plot = FALSE)

  for (i in (1:length(x$stats[1,]))) {
  
    x$stats[1,i] <- quantile(eval(as.symbol(myplot))[,i], probs = 0.05)
  
    x$stats[5,i] <- quantile(eval(as.symbol(myplot))[,i], probs = 0.95)
  
  }


  if(myplot == "evaporation") {
    bxp(x, outline = FALSE, boxwex = 0.4, ylab = paste(myplot,"[mm]"), 
        main = mytitle)
  }  else {bxp(x, outline = FALSE, boxwex = 0.4, ylab = paste(myplot,"[mm]"))}

}


##annual averages (city only)

M <- matrix(c(1,2), ncol = 1)
layout(M, heights = c(5,5))

kwb.plot::setMargins(left=7, top = 7, bottom = 3)

if (include_forest) {
  mytitle <- "Annual water balance excluding lakes (volume average)"
} else {
  mytitle <- "Annual water balance excluding forest and lakes (volume average)"
}

plot(x = avg_wat_bal$year, y = avg_wat_bal$rain_yr*1.09,
     ylab = "Annual sum [mm]", col = "blue", 
     main = mytitle)
points(x = avg_wat_bal$year, y = avg_wat_bal$pot_ev_yr,
       col = "red")
abline(v = c(1991:2020), col = "lightgray", lty = "dotted")
legend(x= 2013, y = 1100, legend=c("rainfall", "potential evaporation"), 
       pch=c(1,1), col = c("blue", "red"), cex = 0.7, pt.cex=1.2, 
       xpd = TRUE, y.intersp=1, bg = "white")

plot(x = avg_wat_bal$year, y = avg_wat_bal$evaporation,
     ylab = "annual average [mm]", col = "red", ylim = c(0, 500))
points(x = avg_wat_bal$year, y = avg_wat_bal$infiltration,
     col = "blue")
points(x = avg_wat_bal$year, y = avg_wat_bal$interflow,
       col = "green")
points(x = avg_wat_bal$year, y = avg_wat_bal$runoff,
       col = "black")

abline(v = c(1991:2020), col = "lightgray", lty = "dotted")

legend(x= 2013, y = 650, legend=c("evaporation", "infiltration", "interflow", "runoff"), 
       pch=c(1,1,1,1), col = c("red", "blue", "green", "black"), cex = 0.7, pt.cex=1, 
       xpd = TRUE, y.intersp=1, bg = "white")


dev.off()

kwb.utils::hsShowPdf(pdfFile)


