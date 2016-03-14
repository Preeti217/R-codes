#Extracts max magnitude at a given frequency from the overall soot file arranged in the order of increasing RO numbers

max_magnitude <- function() {
  #Set working directory
  list.filename <- list.files()
  n = length(list.filename)
  
  df_list <- data.frame(matrix(ncol = 7, nrow = 0))
  temp_det <- data.frame(matrix(nrow = n, ncol = 1))
  mag_profile <- data.frame(matrix(nrow = n, ncol = 2))
  for (i in 1:n) {
    temp_file <- read.csv(file = list.filename[i])
    temp_file$Avg_Temp <- rowMeans(temp_file[, c(4,5)]) 
    temp_file <- subset(temp_file, select = c(1:3,6))
    temp_det[i,] <- nrow(temp_file)
    mag_profile[i,] <- c(i, unique(temp_file$Avg_Temp))
    df_list <- rbind(df_list,temp_file)
  }
  
  temp_det$index <- cumsum(temp_det$matrix.nrow...n..ncol...1.)
  temp_det$matrix.nrow...n..ncol...1. <- NULL
  
  occur <- data.frame(table(mag_profile$X2))
  n_occur <- occur[occur$Freq > 1,]
  
  freq_range <- unique(df_list$Frequency)
  len <- length(freq_range)
  max_magni <- data.frame(matrix(nrow = 0, ncol = 4))
  for (j in 1:len) {
    temp_store <- df_list[df_list$Frequency == freq_range[j],]
    temp_store <- temp_store[order(temp_store$Magnitude),]
    temp_magni <- temp_store[1,]
    row.names(temp_magni) <- NULL
    t_magni <- df_list[(df_list$Frequency == temp_magni$Frequency) & (df_list$Magnitude == temp_magni$Magnitude) & (df_list$Phase == temp_magni$Phase) & (df_list$Avg_Temp == temp_magni$Avg_Temp),]
    max_magni <- rbind(max_magni, t_magni)
  }
  
  max_magni$index <- as.numeric(row.names(max_magni))
  max_magni <- max_magni[order(max_magni$index, decreasing = FALSE),]
  # max_magni$index <- NULL
  row.names(max_magni) <- NULL
  
  # ind <- data.frame(matrix(nrow = 0, ncol = 1))
  for (x in 1:nrow(max_magni)){
    rt <- which(max_magni$index[x] <= temp_det$index)
    rt <- rt[1]
    rt <- as.numeric(rt)
    # ind <- rbind(ind,rt)
    max_magni$ro_n[x] <- rt
  }
  
  row.names(max_magni) <- NULL
  
  return(max_magni)
}