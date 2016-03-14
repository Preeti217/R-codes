#Set working directory
list.filename <- list.files()
n = length(list.filename)

df_list <- data.frame(matrix(ncol = 7, nrow = 0))
for (i in 1:n) {
  temp_file <- read.csv(file = list.filename[i])
  temp_file$Avg_Temp <- rowMeans(temp_file[, c(4,5)]) 
  temp_file <- subset(temp_file, select = c(1:3,6))
#   if (i == 1){plot(temp_file, type= 'l', col = i)}
#   else {par(new=TRUE)
#     plot(temp_file, type='l', col = i)
#   }
  df_list <- rbind(df_list,temp_file)
  # df_list[i,] <- c(min(temp_file$Frequency), max(temp_file$Frequency), min(temp_file$Magnitude), max(temp_file$Magnitude),min(temp_file$Phase), max(temp_file$Phase), temp_file$Avg_Temp)
}

# par(mfrow = c(2,3))
# plot(df_list$X3, type = 'l', ylab = 'Minimum Magnitude', col = 23)
# plot(df_list$X4, type = 'l', ylab = 'Maximum Magnitude', col = 24)
# plot(df_list$X3, type = 'l', ylab = 'Max / Min Magnitude', col = 23)
# par(new = T)
# plot(df_list$X4, type = 'l', ylab = NULL, col = 24)
# par(new=T)
# plot(df_list$X7, type= 'l', col = 66)
# par(new = F)
# plot(df_list$X5, type = 'l', ylab = 'Minimum Phase', col = 19)
# plot(df_list$X6, type = 'l', ylab = 'Maximum Phase', col = 84)
# plot(df_list$X5, type = 'l', ylab = 'Max / Min Phase', col = 19)
# par(new = T)
# plot(df_list$X6, type = 'l',ylab = NULL, col = 84)
# par(new = F)

# library(dplyr)
# df_wide <- reshape(df_list, timevar = 'Frequency', idvar = 'Avg_Temp', direction = 'wide')
max_mag <- 0
min_mag <- 0
max_t_p <- 0
min_t_p <- 0
df_tab <- data.frame(matrix(nrow= 0, ncol = 7))
colnames(df_tab) <- c('Frequency' , 'Max Magnitude', 'Max_Temp', 'Max_Phase', 'Min Magnitude', 'Min_Temp', 'Min_Phase')
freq_range <- unique(df_list$Frequency)
for (j in 1:length(freq_range)){
  max_mag[j] <- max(df_list$Magnitude[df_list$Frequency == freq_range[j]])
  min_mag[j] <- min(df_list$Magnitude[df_list$Frequency == freq_range[j]])
  max_t_p[j] <- df_list[df_list$Magnitude == max_mag[j] & df_list$Frequency == freq_range[j],]
  min_t_p[j] <- df_list[df_list$Magnitude == min_mag[j] & df_list$Frequency == freq_range[j],]
  #df_tab <- cbind(freq_range,max_mag, max_t_p, min_mag, min_t_p) 
}


for (j in 1:length(freq_range)){
  j = 100
  ind <- which(df_list$Frequency == freq_range[j])
  max_mag <- df_list[which.max(df_list$Magnitude[ind]),2:4]
  min_mag <- df_list[which.min(df_list$Magnitude[ind]),2:4]
  #df_tab[j,] <- c(freq_range[j], max_mag, min_mag )
}
