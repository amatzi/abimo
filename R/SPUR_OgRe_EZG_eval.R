
#directory of clipped EZG dbfs
data.dir <- "Y:/SUW_Department/Projects/SpuR/Data-Work_packages/AP1_Modellierung/ABIMO/maps/"

#directory for output data
write.dir <- "Y:/SUW_Department/Projects/SpuR/Data-Work_packages/AP1_Modellierung/ABIMO/results_OgRe_EZG/"

#path for OgRe concentrations
path_OgRe_Model_files <- 'Y:/SUW_Department/Projects/SpuR/Data-Work_packages/AP1_Modellierung/OgRe-Modell/data_LoadModel/'

#path for help files
path_help_files <- "data"



##format summary tables-----------------------------------------

OgRe_Types <- c("ALT", "NEU", "EFH", "GEW")

sources <- c("Dach", "Strasse", "Hof", "Putzfassade")

#prepare summary table for OgRe EZG
x_summary <- data.frame("OgRe_Type" = OgRe_Types,
                        "Fl_Dach" = NA,
                        "Fl_Hof" = NA,
                        "Fl_Str" = NA,
                        "Fl_PutzFass" = NA,
                        "Runoff_Dach" = NA,
                        "Runoff_Hof" = NA,
                        "Runoff_Str" = NA,
                        "Runoff_PutzFass" = NA,
                        "Load_Zn" = NA,
                        "Load_Benzothiazol" = NA,
                        "Load_Diuron" = NA,
                        "Load_Mecoprop" = NA,
                        "Load_Terbutryn" = NA)

#prepare format of result tables 
x_summary_Konz <- data.frame("Source" = sources,
                             "Konz_Zn" = NA,
                             "Konz_Benzothiazol" = NA,
                             "Konz_Diuron" = NA,
                             "Konz_Mecoprop" = NA,
                             "Konz_Terbutryn" = NA)




#####assemble summary table (by OgRe Type)----------------------------

#load Putzfassadenflaechen

x_Putz <- read.table(file = file.path(path_help_files, "Putzfassaden.csv"), 
                     header = TRUE, sep = ";", dec = ".")

for (OgRe_Type in OgRe_Types) {
  
  #load clipped dbf
  x_type <- foreign::read.dbf(file = file.path(data.dir, paste0("clip_",OgRe_Type,".dbf")), as.is = TRUE)
  
  #row in result table
  index <- which(x_summary$OgRe_Type == OgRe_Type)
  
  #factor for clipping
  x_type$clip_factor <- x_type$clip_area / x_type$FLGES
  
  #impervious surfaces in m2
  x_summary$Fl_Dach[index] <- sum(x_type$FLGES * x_type$clip_factor * #area
                          x_type$PROBAU /100 * x_type$KAN_BEB /100) #impervious and sewered area only
  x_summary$Fl_Hof[index] <- sum(x_type$FLGES * x_type$clip_factor * #area
                                    x_type$PROVGU /100 * x_type$KAN_VGU /100) #impervious and sewered area only
  x_summary$Fl_Str[index] <- sum(x_type$STR_FLGES * x_type$clip_factor * #area
                                    x_type$VGSTRASSE /100 * x_type$KAN_STR /100) #impervious and sewered area only
  x_summary$Fl_PutzFass[index] <- x_Putz$FL_Putz_ha[x_Putz$OgRe_Type == OgRe_Type] * 100 * 100 #calculate m2 from hectares
  
  #runoff in m3/yr
  x_summary$Runoff_Dach[index] <- sum(x_type$ROW_roof / 1000 * x_type$FLGES * x_type$clip_factor) # [mm/yr /1000m/m2 * m2] = [m3/yr]
  x_summary$Runoff_Hof[index] <- sum(x_type$ROW_yards / 1000 * x_type$FLGES * x_type$clip_factor) # [mm/yr /1000m/m2 * m2] = [m3/yr]
  x_summary$Runoff_Str[index] <- sum(x_type$ROW_street / 1000 * x_type$STR_FLGES * x_type$clip_factor) # [mm/yr /1000m/m2 * m2] = [m3/yr]
  
  #loads in kg/yr
  x_summary$Load_Zn[index] <- sum(x_type$Zink_kg_yr * x_type$clip_factor)
  x_summary$Load_Diuron[index] <- sum(x_type$Diuron_kg_ * x_type$clip_factor)
  x_summary$Load_Mecoprop[index] <- sum(x_type$Mecoprop_k * x_type$clip_factor)
  x_summary$Load_Terbutryn[index] <- sum(x_type$Terbutryn_ * x_type$clip_factor)
  x_summary$Load_Benzothiazol[index] <- sum((x_type$Benzothiaz + x_type$Methylthio + x_type$Hydroxyben) * # sum of all benzothiazole metabolites
                                            x_type$clip_factor)
}


#### assemble concentration tables by OgReType-------------------------------------------------

#load OgRe-Model concentrations
OgRe_conc <- read.table(file = file.path(path_OgRe_Model_files, "annual_mean_conc.csv"), 
                        header = TRUE, sep = ";", dec = ".", as.is = TRUE)

for (OgRe_Type in OgRe_Types) {
  
  ##row in summary table
  index <- which(x_summary$OgRe_Type == OgRe_Type)
  
  ##source indices
  index_roof <- which(x_summary_Konz$Source == "Dach")
  index_yard <- which(x_summary_Konz$Source == "Hof")
  index_str <- which(x_summary_Konz$Source == "Strasse")
  index_putz <- which(x_summary_Konz$Source == "Putzfassade")
  
  ##Mecoprop, assumed from roof only in mg/L = g/m3
  x_summary_Konz$Konz_Mecoprop <- 0
  x_summary_Konz$Konz_Mecoprop[index_roof] <- x_summary$Load_Mecoprop[index] / x_summary$Runoff_Dach[index] * 1000 #from [kg/m3] to [g/m3]
  
  ##Zinc, assumed from buildings and streets in mg/L = g/m3
  
    #street share of zinc
    Load_roof <- max(0, 
                     x_summary$Load_Zn[index] - 
                       x_summary$Runoff_Str[index] * 
                       OgRe_conc$STR[which(OgRe_conc$VariableName == "Zink")] /1000 /1000) # conc from ug/L to kg/m3
    Load_str <- x_summary$Load_Zn[index] - Load_roof
  
  
    #assign concentrations
    x_summary_Konz$Konz_Zn <- 0
    x_summary_Konz$Konz_Zn[index_str] <-  Load_str /  # load from streets
                                          x_summary$Runoff_Str[index] * 1000           #from [kg/m3] to [g/m3]
    x_summary_Konz$Konz_Zn[index_roof] <- Load_roof /  # load from streets
                                          x_summary$Runoff_Dach[index] * 1000           #from [kg/m3] to [g/m3]
    
  ##Benzothiazole, assumed from buildings, yards and streets in mg/L = g/m3
    
    #street share of Benzothiazole
    
    Conc_str <- OgRe_conc$STR[which(OgRe_conc$VariableName == "Benzothiazol")] +
      OgRe_conc$STR[which(OgRe_conc$VariableName == "Methylthiobenzothiazol")] +
      OgRe_conc$STR[which(OgRe_conc$VariableName == "Hydroxybenzothiazol")] 
    
    Load_type <- max(0, 
                     x_summary$Load_Benzothiazol[index] - 
                       x_summary$Runoff_Str[index] * 
                       Conc_str /1000 /1000) # conc from ug/L to kg/m3
    Load_str <- x_summary$Load_Benzothiazol[index] - Load_type
    
    #assign concentrations
    x_summary_Konz$Konz_Benzothiazol <- 0
    x_summary_Konz$Konz_Benzothiazol[index_str] <-  Load_str /  # load from streets
                                                    x_summary$Runoff_Str[index] * 1000           #from [kg/m3] to [g/m3]
    x_summary_Konz$Konz_Benzothiazol[index_roof] <- Load_type *  # load from yards and roofs
                                          x_summary$Runoff_Dach[index] / (x_summary$Runoff_Dach[index] + x_summary$Runoff_Hof[index]) / # share of roofs                                      
                                          x_summary$Runoff_Dach[index] * 1000           #from [kg/m3] to [g/m3]
    x_summary_Konz$Konz_Benzothiazol[index_yard] <- Load_type *  # load from yards and roofs
                                          x_summary$Runoff_Hof[index] / (x_summary$Runoff_Dach[index] + x_summary$Runoff_Hof[index]) / # share of roofs                                      
                                          x_summary$Runoff_Hof[index] * 1000           #from [kg/m3] to [g/m3]
  
  ##Terbutryn and Diuron from facades only, assign once volumes are known
  
  ##assign values to type-table
  assign(paste0("Konz_", OgRe_Type), x_summary_Konz)
    
    
}


