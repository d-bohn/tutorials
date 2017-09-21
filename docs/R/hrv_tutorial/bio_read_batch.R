source("Signal_Utilities.R")


data <- data.table::fread('gunzip -c 1041.txt.gz')

data.table::fwrite(as.data.frame(data), 'test.csv', sep = ',')
filename <- 'test.csv'

system(paste0("gzip -f ", filename))
