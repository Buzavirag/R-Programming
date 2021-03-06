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
  
  ## 1. l�p�s: id szerinti f�jlokb�l adatok beolvas�sa
  ## 2. l�p�s: mean sz�m�t�sa
  
  files_list <- list.files(directory, full.names=TRUE) ## F�jlok nev�nek beolvas�sa
  tmp <- vector(mode = "list", length = length(id)) ## Seg�d v�lt
  for(i in seq_along(id)) { 
    tmp[[i]] <- read.csv(files_list[id[i]]) ## Beolvassuk az id-nek megfelel� f�jlok adatait
  }
  dat <- do.call(rbind, tmp) ## �s lesz bel�le egy f�jl
  if(pollutant == "nitrate") {
    mean(dat[, 3], na.rm = TRUE)
  } else if(pollutant == "sulfate") {
    mean(dat[, 2], na.rm = TRUE)
  } else print("Invalid pollutant")
}

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  files_list <- list.files(directory, full.names=TRUE) ## F�jlok nev�nek beolvas�sa
  tmp <- vector(mode = "list", length = length(id)) ## Seg�d v�lt
  for(i in seq_along(id)) { 
    tmp[[i]] <- read.csv(files_list[id[i]]) ## Beolvassuk az id-nek megfelel� f�jlok adatait
  }
  dat <- do.call(rbind, tmp) ## �s lesz bel�le egy f�jl
  teljes <- !is.na(dat$sulfate) & !is.na(dat$nitrate)
  output <- data.frame(matrix(NA, nrow = length(id), ncol = 2)) # �res data.frame l�trehoz�sa
  names(output) <- c("id", "nobs")
  for(j in 1:length(id)) {
    output[j, 1] <- read.csv(files_list[id[j]])[1, 4]
    output[j, 2] <- sum(teljes[which(dat$ID == id[j])])
  }
  return(output)
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations  
  output <- vector("numeric")
  files_list <- list.files(directory, full.names=TRUE) ## F�jlok nev�nek beolvas�sa
  sorszam <- 1
  for(i in 1:length(files_list)) {
    tmp <- read.csv(files_list[i])
    if(sum(complete.cases(tmp))>threshold) {
      output[sorszam] <- cor(tmp$nitrate, tmp$sulfate, use = "complete.obs")
      sorszam <- sorszam + 1
    }
  }
  return(output)
}