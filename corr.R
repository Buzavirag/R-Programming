corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations  
  output <- vector("numeric")
  files_list <- list.files(directory, full.names=TRUE) ## Fájlok nevének beolvasása
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