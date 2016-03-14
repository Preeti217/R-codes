extract_freq <- function(){
  #Set working directory
  list.filename <- list.files()
  n = length(list.filename)
  
  df_list <- data.frame(matrix(ncol = 7, nrow = 0))
  for (i in 1:n) {
    temp_file <- read.csv(file = list.filename[i])
    temp_file$Avg_Temp <- rowMeans(temp_file[, c(4,5)]) 
    temp_file <- subset(temp_file, select = c(1:3,6))
    df_list <- rbind(df_list,temp_file)
  }
  
  df_tab <- data.frame(matrix(nrow= 0, ncol = 8))
  freq_range <- unique(df_list$Frequency)
  max_mag <- data.frame(matrix(ncol = 4, nrow = 0))
  min_mag <- data.frame(matrix(ncol = 3, nrow = 0))
  for (j in 1:length(freq_range)){
    a <- df_list[df_list$Frequency == freq_range[j],]
    a <- a[order(a$Magnitude),]
    mag_1 <- a[1,]
    mag_2 <- a[length(a),-1]
    max_mag <- rbind(max_mag,mag_1)
    min_mag <- rbind(min_mag,mag_2)
  }
  
  df_tab <- cbind(max_mag, min_mag)
  colnames(df_tab) <- c('Frequency' , 'Max Magnitude','Max_Phase', 'Max_Temp', 'Min Magnitude', 'Min_Phase', 'Min_Temp')
  rownames(df_tab) <- NULL
  return(df_tab)
}