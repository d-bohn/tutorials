#### Setup ecg data file from transposed raw data ####
phys_file <- function(data, cols, save){
  df <- data[,cols]
  write.table(df, paste(save,".phys.csv", sep = ""), sep = ',',
            row.names = FALSE)
}

#### Setup info file ####
phys_info <- function(filename, save, fs, origin = NA){
  filename <- filename
  origin = origin
  fs = fs
  data <- cbind.data.frame(origin,fs)
  write.table(data, file = file.path(save,paste0(filename,".info.txt")),
              sep = ",", row.names = FALSE)
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
outliers_ibi <- function(file, path, variable = 'ibi'){
  wd <- getwd()
  setwd(path)
  filename <- sub("*.ibi.gz", "", file)
  df <- read.delim(file, header = TRUE, sep = ',')
  df$x <- seq(1, length(df[,variable]))
  
  y <- seq(2, length(df[,variable]))
  d <- diff(df[,variable], lag = 1, differences = 1)
  diff <- cbind.data.frame(y,d)
  data <- merge(df,diff, by.x = "x", by.y = "y", all.x = TRUE)
  
  # Flag potential outliers
  data$V2 <- ifelse(data$d >= 300, NA,
                 ifelse(data$d <= -300, NA, data$ibi))
  
  if (sum(is.na(data$V2)) >= 1){
    # Predict values
    data$predict <- forecast::tsclean(data$V2)
    data$V3 <- ifelse(is.na(data$V2)==TRUE, data$predict, data$V2)
  }

  # Plot and compare
  data$colour <- ifelse(is.na(data$V2)==TRUE, "red", "black")
  ## Plot 1
  plot1 <- ggplot2::ggplot(data) +
    ggplot2::geom_point(ggplot2::aes(y = ibi, x = x, colour = colour)) +
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
  write.table(data$V3, file=paste0(filename,"_ecg_clean.txt"), 
              sep="\t", row.names=FALSE, col.names=FALSE)
  
  # Clean up
  setwd(wd)
  rm(list = ls())
}
