Magnitude_profile <- function(){
  list.filename <- list.files()
  n = length(list.filename)
  
  df_list <- data.frame(matrix(ncol = 7, nrow = 0))
  for (i in 1:n) {
    temp_file <- read.csv(file = list.filename[i])
    temp_file$Avg_Temp <- rowMeans(temp_file[, c(4,5)]) 
    temp_file <- subset(temp_file, select = c(1:3,6))
    df_list[i,] <- c(min(temp_file$Frequency), max(temp_file$Frequency), min(temp_file$Magnitude), max(temp_file$Magnitude),min(temp_file$Phase), max(temp_file$Phase), temp_file$Avg_Temp)
  }
  
  return(df_list)
  par(mfrow = c(1,3))
  plot(df_list$X3, type = 'l', ylab = 'Minimum Magnitude', col = 23)
  plot(df_list$X4, type = 'l', ylab = 'Maximum Magnitude', col = 24)
  plot(df_list$X4, type = 'l', ylab = NULL, col = 24)
  par(new=T)
  plot(df_list$X7, type= 'l', col = 66)
  par(new = F)
}
