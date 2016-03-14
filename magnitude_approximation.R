#Used to plot only the mean of the spectra
mag_approximation <- function(my_colour){
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
  
  
  for (j in 1:n){
    temp_plot <- df_list[j,]
    temp_plot <- t(temp_plot)
    if (j == 1){
      plot(temp_plot, type = 'l', col = my_colour)
      }
    else{
      par(new = T)
      plot(temp_plot, type = 'l', col = my_colour)
      }
  }
  
  par(new = T)
  plot(mean_col[,1], type = 'l', col = 'black', lwd = 3)
}
