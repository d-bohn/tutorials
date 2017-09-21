# # For debugging
# file <- "1045_trigger.csv"
# path <- "/Volumes/UNTITLED/ER_Phsyio2_Data/data/processed/"

make.events <- function(file, path){
  wd <- getwd()
  setwd(path)
  name <- sub("*_trigger.csv.gz", "", file)
  events <- read.table(file, header = FALSE)
  events$V2 <- seq(1, length(events$V1))
  colnames(events)[2] <- "Time(ms)"
  
  # Set value variable
  colnames(events)[1] <- "Value"
  
  # Make InitTime variable
  op <- options(digits.secs=3)
  #events$InitTime <- format( (as.POSIXct(Sys.Date())+events$`Time(ms)`/1000)-72000, "%H:%M:%OS")
  events$InitTime <- events$`Time(ms)`/1000
  
  # Make type variable
  values <- c(1.30,1.36)
  condition <- c("Baseline","ER_Task")
  duration <- c(300,600)
  
  match <- cbind.data.frame(values, condition, duration)
  
  events$Type1 <- match(events$Value,match$values)
  events$Type <- match[events$Type1,2]
  events$Duration <- match[events$Type1,3]
  
  # Reorder and save
  events <- events[c(3,5,6,1,2,4)]
  events <- events[,c(-5,-6)]
  events <- events[complete.cases(events$Type),]
  events <- events[!rev(duplicated(rev(events$Type))),]
  
  InitTime <- events$InitTime
  Type <- events$Type
  Duration <- events$Duration
  Value <- events$Value
  
  #rm(events,match,condition,duration,Duration,file,InitTime,op,path,Type,Value,values)
  
 episodes <- list(InitTime=InitTime,Type=Type,Duration=Duration,Value=Value)
  
 save(episodes, file = paste0(path,"/",name,".RData"))
  
  # Clean up
  #rm(match,condition,op,values,duration)
  setwd(wd)
  rm(list = ls())
}




