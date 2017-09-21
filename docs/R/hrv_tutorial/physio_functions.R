## For de-bugging

# file <- "1021.ibi.txt"
# path <- "processed/"
# path <-
# variable <- "V1"
# df <- read.delim(file, header=FALSE)

#### Extract ecg and trigger ####
extract.ecg <- function(file, path){
  wd <- getwd()
  setwd(path)
  filename <- sub("*.txt.gz", "", file)
  # data <- data.table::fread(file, header = FALSE)
  data <- data.table::fread(paste0("gunzip -c ", file), header = FALSE)
  ecg <- data$V1
  trigger <- data$V15
  
  # write.table(ecg, file = paste0("../processed/",filename,"_ecg.csv"), sep = ",",
  #             col.names = FALSE, row.names = FALSE)
  
  data.table::fwrite(as.data.frame(ecg), file.path('../processed',paste0(filename,'_ecg.csv')))
  data.table::fwrite(as.data.frame(trigger), file.path('../processed',paste0(filename,'_trigger.csv')))
  
  system(paste0('gzip -f ', file.path('../processed',paste0(filename,'_ecg.csv'))))
  system(paste0('gzip -f ', file.path('../processed',paste0(filename,'_trigger.csv'))))
  
  # write.table(trigger, file = paste0("../processed/",filename,"_trigger.csv"), sep = ",",
  #             col.names = FALSE, row.names = FALSE)
  setwd(wd)
  rm(list = ls())
}

#### Setup ecg data file from transposed raw data ####
phys_file <- function(file, path){
  wd <- getwd()
  setwd(path)
  filename <- sub("*_ecg.csv.gz", "", file)
  data <- read.csv(file, header = FALSE)
  data$time <- seq(0, ((length(data$V1)-1)*.001), by = .001)
  data <- data[,c(2,1)]
  colnames(data) <- c("time","ecg")
  write.csv(data, paste("../physio_files/",filename,".phys.csv", sep = ""),
            row.names = FALSE)
  setwd(wd)
  rm(list = ls())
}

#### Setup info file ####
phys_info <- function(file, path, fs, origin = NA){
  wd <- getwd()
  setwd(path)
  filename <- sub("*_ecg.csv.gz", "", file)
  origin = origin
  fs = fs
  data <- cbind.data.frame(origin,fs)
  write.table(data, file = paste("../physio_files/",filename,".info.txt", sep = ""),
              sep = ",", row.names = FALSE)
  setwd(wd)
  rm(list = ls())
}

#### Read and write IBI files ####
write_ibi <- function(file, output, path){
  wd <- getwd()
  setwd(path)
  filename <- sub("*.ibi.gz", "", file)
  data <- read.csv(file)
  data <- data[-1]
  write.table(data, paste("../processed/",filename,".ibi.txt",sep=""), sep="\t",
              col.names=FALSE, row.names = FALSE)
  setwd(wd)
  rm(list = ls())
}

#### Visually inspect IBI series for outliers ####
outliers_ibi <- function(file, path = path, variable= "V1"){
  wd <- getwd()
  setwd(path)
  filename <- sub("*.ibi.txt", "", file)
  df <- read.delim(file, header = FALSE)
  df$x <- seq(1, length(df[,variable]))
  
  y <- seq(2, length(df[,variable]))
  d <- diff(df$V1, lag = 1, differences = 1)
  diff <- cbind.data.frame(y,d)
  data <- merge(df,diff, by.x = "x", by.y = "y", all.x = TRUE)
  
  # Flag potential outliers
  data$V2 <- ifelse(data$d >= 300, NA,
                 ifelse(data$d <= -300, NA, data$V1))
  
  if (sum(is.na(data$V2)) >= 1){
    # Predict values
    data$predict <- forecast::tsclean(data$V2)
    data$V3 <- ifelse(is.na(data$V2)==TRUE, data$predict, data$V2)
  }

  # Plot and compare
  data$colour <- ifelse(is.na(data$V2)==TRUE, "red", "black")
  ## Plot 1
  plot1 <- ggplot2::ggplot(data) +
    ggplot2::geom_point(ggplot2::aes(y = V1, x = x, colour = colour)) +
    ggplot2::scale_colour_manual(values=c("black","red"), guide = FALSE) +
    ggplot2::ggtitle("Raw Data")
  ## Plot 2
  plot2 <- ggplot2::ggplot(data) +
    ggplot2::geom_point(ggplot2::aes(y = V3, x = x, colour = colour)) +
    ggplot2::scale_colour_manual(values=c("black","red"), guide = FALSE) +
    ggplot2::ggtitle("Outliers Interpolated")
  ## multiplot and Save
  png(filename = paste("../plots/",filename,"_plot.png",sep=""), width=1000, height=669,
      units="px")
  gridExtra::grid.arrange(plot1,plot2, ncol=2, top=filename)
  dev.off()
  
  # Write data and save
  write.table(data$V3, file=paste0("../outliers_removed/",filename,"_ecg_clean.txt"), 
              sep="\t", row.names=FALSE, col.names=FALSE)
  
  # Clean up
  setwd(wd)
  rm(list = ls())
}
