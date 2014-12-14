pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  ## 1. lépés: id szerinti fájlokból adatok beolvasása
  ## 2. lépés: mean számítása
  
  files_list <- list.files(directory, full.names=TRUE) ## Fájlok nevének beolvasása
  tmp <- vector(mode = "list", length = length(id)) ## Segéd vált
  for(i in seq_along(id)) { 
    tmp[[i]] <- read.csv(files_list[id[i]]) ## Beolvassuk az id-nek megfelelõ fájlok adatait
  }
  dat <- do.call(rbind, tmp) ## És lesz belõle egy fájl
  if(pollutant == "nitrate") {
    mean(dat[, 3], na.rm = TRUE)
  } else if(pollutant == "sulfate") {
    mean(dat[, 2], na.rm = TRUE)
  } else print("Invalid pollutant")
}