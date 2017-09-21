# # For debugging
# file <- "1045_ecg_clean.txt"
# path <- "/Volumes/UNTITLED/ER_Phsyio2_Data/data/outliers_removed/"

freq_analysis_batch <- function(file, path){
  library(RHRV)
  wd <- getwd()
  setwd(path)
  name <- sub("*_ecg_clean.txt", "", file)
  hrv.data = CreateHRVData()
  hrv.data = SetVerbose(hrv.data, FALSE)
  hrv.data = LoadBeatRR(hrv.data, RecordName=file, RecordPath=".", scale = .001)
  
  #we add the info about the episodes
  file_ev <- sub("*_ecg_clean.txt", "", file) 
  # hrv.data = LoadEpisodesAscii(hrv.data, InitTime="0:0:0.00",
  #                              FileName=paste0("../processed/new_",file_ev,"_trigger.txt"),Tag="")
  
  load(paste0("../processed/",name,".RData"))
  InitTime <- episodes$InitTime
  Type <- episodes$Type
  Duration <- episodes$Duration
  Value <- episodes$Value
  
  hrv.data = AddEpisodes(hrv.data, InitTimes = episodes$InitTime, 
                         Tags = episodes$Type,
                         Durations = episodes$Duration,
                         Values = episodes$Value)
  
  hrv.data = BuildNIHR(hrv.data)
  hrv.data = FilterNIHR(hrv.data)
  
  # plot all tags
  png(filename = paste("../plots/",name,"_tagged_plot.png",sep=""), width=1000, height=669,
      units="px")
  PlotNIHR(hrv.data, Tag=episodes$Type)
  dev.off()
  
  hrv.data = InterpolateNIHR(hrv.data, freqhr = 4)
  
  #Perform frequency analysis
  ##Calculating spectrogram and power per band using wavelet analysis:
  hrv.data = CreateFreqAnalysis(hrv.data)
  hrv.data = CalculatePowerBand(hrv.data, indexFreqAnalysis = 1, type="wavelet",
                                wavelet="d4",bandtolerance=0.1)
  
  # plot powerband for all files
  png(filename = paste("../plots/",name,"_powerband.png",sep=""), width=1000, height=669,
      units="px")
  PlotPowerBand(hrv.data, normalized = TRUE, hr = TRUE, Tag = "all")
  dev.off()
  
  # Save the data by stimulus type:
    splitting.data = SplitPowerBandByEpisodes(hrv.data,indexFreqAnalysis = 1, Tag = c("Baseline"))
    Baseline <- log(mean(splitting.data$OutEpisodes$HF))
    
    splitting.data = SplitPowerBandByEpisodes(hrv.data,indexFreqAnalysis = 1, Tag = c("ER_Task"))
    Task <- log(mean(splitting.data$OutEpisodes$HF))
    
    subject_nr <- readr::parse_number(file)
    sub <- cbind.data.frame(subject_nr,Baseline,Task)
    
    write.table(sub, file = "../hrv/data_all_physio.csv", sep = ",", append = TRUE,
                col.names = FALSE, row.names = FALSE)
    # Clean up
    setwd(wd)
    rm(list = ls())
  }
    
    