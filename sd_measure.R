#Gives an idea about the distribution of the magnitude for the different spectra over the frequency range

sd_measure <- function(){
  library(plyr)
  list.filename <- list.files()
  n = length(list.filename)
  
  df_list <- data.frame(matrix(nrow = 1, ncol = 892))
  temper_prof <- data.frame(matrix(ncol = 1, nrow = n))
  
  for (i in 1:n){
    temp_file <- read.csv(file = list.filename[i])
    temp_mag <- temp_file$Magnitude
    mag_row <- data.frame(t(temp_mag))
    temper_prof[i,1] <- mean(temp_file$Inlet.Temp[1], temp_file$Outlet.Temp[1])
    df_list <- rbind.fill(df_list, mag_row)
  }
  
  
  #deletes columns which have complete NA values, possibly coerced by the rbind.fill command
  df_list <- df_list[, ! apply(df_list, 2, function(x) all(is.na(x)))]
  
  colnames(df_list) <- temp_file[,1]
  df_list <- df_list[-1,]
  row.names(df_list) <- NULL
  
  df_list_1 <- data.frame(t(df_list))
  mean_col <- data.frame(rowMeans(df_list_1))
  sd_col <- data.frame(apply(df_list, 2, sd))
  sd_max <- sd_col$apply.df_list..2..sd. + mean_col
  sd_min <- mean_col - sd_col$apply.df_list..2..sd.
  
  sd_1 <- length(which(sd_col <1))
  sd_half <- length(which(sd_col < 0.5))
  total <- nrow(sd_col)
  
  sd_ouput <- data.frame(sd_1,sd_half, total)
  return(sd_ouput)
 
}
