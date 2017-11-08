# For debugging
# file <- "sub1101.RDS"
# path <- "data/hrv_tutorial/"

make_events <- function(file, path){
  wd <- getwd()
  setwd(path)
  name <- sub("*.RDS", "", file)
  events <- readRDS(file)[c(1,3)]
  
  # Set value variable
  #colnames(events)[1] <- "Value"
  
  # Make InitTime variable
  op <- options(digits.secs=3)
  #events$InitTime <- format( (as.POSIXct(Sys.Date())+events$`Time(ms)`/1000)-72000, "%H:%M:%OS")
  events$InitTime <- events$time
  
  # Make type variable
  values <- unique(events$trigger)
  condition <- NULL
  for (i in seq_along(values)){
    condition[[i]] <- paste0('event_type_', values[[i]])
    }
  duration <- rep(10, length(values))
  
  match <- cbind.data.frame(values, condition, duration)
  
  events$Type1 <- match(events$trigger,match$values)
  events$Type <- match[events$Type1,2]
  events$Duration <- match[events$Type1,3]
  
  # Reorder and save
  # events <- events[c(3,5,6,1,2,4)]
  # events <- events[,c(-5,-6)]
  events <- events[complete.cases(events$Type),]
  events <- events[!rev(duplicated(rev(events$Type))),]
  
  InitTime <- events$InitTime
  Type <- events$Type
  Duration <- events$Duration
  Value <- events$trigger
  
  #rm(events,match,condition,duration,Duration,file,InitTime,op,path,Type,Value,values)
  
 episodes <- list(InitTime=InitTime,Type=Type,Duration=Duration,Value=Value)
  
 save(episodes, file = paste0(name,"_trigger.RData"))
  
  # Clean up
  #rm(match,condition,op,values,duration)
  setwd(wd)
  rm(list = ls())
}




