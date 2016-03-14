#Please set the working directory first

file_import <- function(mypattern) {
  #Creates a list of all the LVM files
  list.filename <- list.files(pattern = mypattern)
  
  #Create an empty list to store the files
  list.data <- list()
  
  #Create a loop to store the files into list.data
  for(i in 1:length(list.filename)){
    list.data[[i]] <- read.delim(list.filename[i],header = FALSE)
  }
  
  maindr <- getwd()
  subdr <- tail(strsplit(maindr, split = '/', fixed = TRUE)[[1]],1)
  
  if (dir.exists(file.path(maindr,subdr)) == FALSE){
    dir.create(file.path(maindr,subdr))
    setwd(file.path(maindr,subdr))
  }
  
  for (j in 4:length(list.filename)){
    y <- data.frame(list.data[j])
    y <- subset(y, select = c(1:5))
    colnames(y) <- c('Frequency', 'Magnitude', 'Phase', 'Inlet Temp', 'Outlet Temp')
    LVM_name <- list.filename[j]
    CSV_name <- unlist(strsplit(LVM_name, split = '.', fixed = TRUE))[1]
    write.csv(y, file = paste0(CSV_name,'.csv'), row.names = FALSE)
  }
  
}
