library(foreign)


#' adapt ABIMO output dbf-file to Berlin shape file 
#'
#' changes order in dbf-file to match geographical 
#' shape file of Berlin "Stadtstruktur"
#'
#' @param ABIMO_out data.frame of ABIMO output file
#' @param file_georef path of dbf file that matches existing shape (incl. path)
#' @param out_file file path and file name for ordered ABIMO output file (to be linked to shape files)
#' 
#' @return ordered dbf returned and written to out_file
#'
#' @examples
#'
ABIMO_adapt_map <- function (
  ABIMO_out,
  file_georef,
  out_file
)
{
  #read ABIMO output file
  x <- ABIMO_out
  
  #read georeferenced dbf file
  y <- read.dbf(file = file_georef, as.is = TRUE)
  
  
  #match polygons
  index <- match(y$schl5, x$CODE)
  x.out <- x[index,]
  if(length(x$CODE) > length(y$schl5)) {
    print('Warning: some polygons without matching geometry')
  }
  
  #write dbf file
  write.dbf(dataframe = x.out, file = out_file)
  print(paste('ordered ABIMO file written to', out_file))
  
  x.out
  
}


#' join ABIMO in- and output files 
#'
#' joins ABIMO in -and output Files 
#' 
#' @param file_ABIMO_out path of ABIMO output file in dbf format (incl. path)
#' @param file_ABIMO_in path of ABIMO input file in dbf format (incl. path)
#' 
#' @return data.frame with matched ABIMO in- and output data
#'
#' @examples
#'
ABIMO_comb_in_out <- function (
  file_ABIMO_out,
  file_ABIMO_in
)
{
  #read ABIMO output file
  x <- read.dbf(file = file_ABIMO_out, as.is = TRUE)
  
  #read ABIMO input file
  y <- read.dbf(file = file_ABIMO_in, as.is = TRUE)
  
  #same length?
  if(length(x$CODE) != length(y$CODE)) {
    print('In- and output files do not match!')
    break
  }
  
  #match order
  index <- match(y$CODE, x$CODE)
  x <- x[index,]
  
  #combine the two files
  x.out <- cbind(y, x)
  
  
  x.out
  
}

#' writes data.frame into ABIMO-dbf 
#'
#' Saves an existing data.frame into dBase-format   
#' and adds "SUB" field to the end of the file
#' as required by ABIMO
#' 
#' @param df_name name of data.frame
#' @param new_dbf path of new ABIMO-input file to be written (.dbf)
#' 
#' @return dbf file that can be processed by ABIMO
#'
#' @examples
#'
write.dbf.ABIMO <- function (
  df_name,
  new_dbf
)
{
  write.dbf(df_name, new_dbf)
  appendSubToFile(new_dbf)
}





#' Add "SUB" field to dbf-File 
#'
#' Adds "SUB" field to the end of an existing  file, as expected by some 
#' older applications (such as input-dbf-file for ABIMO)
#' function by grandmaster HAUKESON 
#' 
#' @param filename Path of file name of data.frame
#' 
#' @return dbf file with sub field
#'
#' @examples
appendSubToFile <- function (
  filename
)
{
  con <- file(filename, "ab")
  on.exit(close(con))
  writeBin(as.raw(0x1A), con)
}



# read_evaporation_matrix_from_url ---------------------------------------------
read_evaporation_matrix_from_url <- function(url)
{
  stopifnot(is.character(url), length(url) == 1L)
  
  file_name <- basename(url)
  
  file <- file.path(tempdir(), file_name)
  
  download.file(url, file)
  
  con <- gzfile(file)
  
  on.exit(close(con))
  
  text <- readLines(con)
  
  year_month <- kwb.utils::extractSubstring("(\\d{4})(\\d{2})", file_name, 1:2)
  
  extract_date_part <- function(i) as.integer(year_month[[i]])
  
  structure(
    as.matrix(read.table(text = text[-(1:6)])),
    header = text[1:6],
    year = extract_date_part(1L),
    month = extract_date_part(2L)
  )
}

# get geographical "stamp" for Berlin area
Berlin_DWD_mask <- function()
{
  #DWD matrix filled with NA
  DWD_dim <- matrix(NA, nrow = 866, ncol = 654)
  
  #get Berlin coordinates...I did not find out how to link to the file I uploaded!!!
  Berlin_coordinates <- foreign::read.dbf('C:/Aendu_lokal/ABIMO_Paper/Daten/Karten/Hilfsgrid_DWD/Grid_Berlin_DWD.dbf')[,6:7]
  
  #set Berlin cells to 1 
  Berlin_matrix <- DWD_dim
  
  for (i in 1:length(Berlin_coordinates$row)) {
    Berlin_matrix[Berlin_coordinates$row[i], Berlin_coordinates$col[i]] <- 1
  }
  
  Berlin_matrix
  
}

# calculate stats of potential evaporation for geographical subset
evaporation_stats <- function(evaporation_matrices,
                              file_info,
                              geo_mask)
{
  pot_evap_stat <- file_info
  
  
  for (i in 1:length(evaporation_matrices)) {
    
    #keep only Berlin grid cells  
    Berlin_values <- evaporation_matrices[[i]] * geo_mask
    
    #correct unit to mm
    Berlin_values <- Berlin_values / 10
    
    pot_evap_stat$mean[i] <- mean(Berlin_values, na.rm = TRUE)
    pot_evap_stat$sd[i] <- sd(Berlin_values, na.rm = TRUE)
    pot_evap_stat$min[i] <- min(Berlin_values, na.rm = TRUE)
    pot_evap_stat$max[i] <- max(Berlin_values, na.rm = TRUE)
    
  }
  
  pot_evap_stat
  
}

