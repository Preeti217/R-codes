#Plots magnitude pulse with respect to temperature for a given soot load
magnitude_pulse <- function(my_colour){
  list.filename <- list.files()
  n = length(list.filename)
  
  #temp_pro <- data.frame(matrix(ncol = 1, nrow = n))
  
  df_list <- data.frame(matrix(ncol = 7, nrow = 0))
  for (i in 1:n) {
    temp_file <- read.csv(file = list.filename[i])
    #temp_file$Avg_Temp <- rowMeans(temp_file[, c(4,5)])
    temp_mat <- temp_file$Magnitude
    #temp_pro[i] <- temp_file$Avg_Temp[1]
    if (i == 1) {
      plot(temp_mat, type = 'l', axes = F)
      }
    else {par(new = T) 
      plot(temp_mat, type = 'l', col = my_colour, axes = F)
    }
  }
}
