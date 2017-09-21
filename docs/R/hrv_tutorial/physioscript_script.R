setwd("R:/AdamsLab/Dan/Data/Current_Studies/")
setwd("MGH_PhysioScripts/")

#### Create files and info ####
source("physio_functions.R")
# Get ecg text file names and merge them into a single data frame
files1 <- list.files(path = "R:/AdamsLab/Dan/Data/Current_Studies/MGH_PhysioScripts/Raw_ECG/", pattern = "*.txt")
# create and write each new ecg file and event file
plyr::ldply(files1, phys_file, path="R:/AdamsLab/Dan/Data/Current_Studies/MGH_PhysioScripts/Raw_ECG/", .progress = "text")
plyr::ldply(files1, phys_info, fs = 600, origin = NA, .progress = "text")

#### Analyze some data ####
load("PhysioScript.RData")

files2 <- list.files(path = "R:/AdamsLab/Dan/Data/Current_Studies/MGH_PhysioScripts/physio_files/", pattern = "*.csv")
process.ecg(in.file.list = paste("physio_files/",files2,sep=""), processing.mode = "batch")
# review.ecg()
extract.ibi(in.file.list = paste("physio_files/",files2,sep=""), processing.mode = "batch")
# artifact.ibi(in.file.list = paste("physio_files/",files2,sep=""), processing.mode = "batch", 
#                                   limits = c(300,1500))
artifact.ibi()
review.ibi()
extract.hrv()
# extract.hrv(f.bands = list(hf = c(.15,.4), lf = c(.04,.15)))
# merge.data(pattern = ".hrv.gz", merged.prefix = "Merged.HRV")

files3 <- list.files(path = "R:/AdamsLab/Dan/Data/Current_Studies/MGH_PhysioScripts/physio_files/", pattern = "*.gz")
plyr::ldply(files3, write_ibi, path = "R:/AdamsLab/Dan/Data/Current_Studies/MGH_PhysioScripts/physio_files/",
            .progress = "text")


#### Remove outliers and review the IBI series ####
files4 <- list.files(path = "R:/AdamsLab/Dan/Data/Current_Studies/MGH_PhysioScripts/physio_files/processed/", 
                     pattern = "*.txt")
plyr::ldply(files4, outliers_ibi, path="R:/AdamsLab/Dan/Data/Current_Studies/MGH_PhysioScripts/physio_files/processed/",
            variable = "V1", .progress = "text")


#### Troubleshooting ####
file <- "1045_ecg.csv.gz"
path <- "R:/AdamsLab/Dan/Data/Current_Studies/MGH_PhysioScripts/physio_files/processed/"
variable = "V1"





