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