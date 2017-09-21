readBIOPAC <- function(rawfile) {
  suppressWarnings(require(data.table)) #used for setnames, which can change varnames without re-copying data
  if (!file.exists(rawfile)) stop("Cannot find file: ", rawfile)
  
  #need to read header of file to determine its structure and to separate it from the raw data
  header <- scan(rawfile, what="character", sep="\n", nlines=100)
  
  filename <- header[1]
  sampleRate <- header[2]
  numChannels <- header[3]
  
  #start looking for channels starting at row 4
  line <- 4
  channelNames <- c()
  channelUnits <- c()
  
  #the channels are on one line and the channel units are on the next line
  #the first line containing commas specifies the headers.
  #read channels and units until we arrive at the comma line
  while (length(grep("CH\\d+,",header[line], perl=TRUE)) == 0) {
    channelNames <- c(channelNames, header[line])
    channelUnits <- c(channelUnits, header[(line + 1)])
    line <- line + 2
  }
  
  #increment line number until we find a line specifying the channel mappings
  #note that this shouldn't be necessary given incrementing of line above, but it is a safeguard
  counter <- 1
  while(length(grep("CH\\d+,CH\\d+",header[line],ignore.case=T, perl=T)) == 0) {
    line <- line + 1
    counter <- counter + 1
    if (counter > 10000) stop(paste0("This file is spinning out: ", rawfile, " Check export."))
  } 
  rowheaders <- header[line]
  
  #and skip the next row, which contains number of data points for each channel
  line <- line + 1
  
  convertHrsToMins <- FALSE
  #if the first piece of the row headers is "min", then the file was saved with elapsed time in minutes
  #this occurs when one checks the "horizontal scale values" options when doing a save as from AcqKnowledge
  if (substr(rowheaders, 1,3) == "min") {
    channelNames <- c("Elapsed Time", channelNames)
    channelUnits <- c("Minutes", channelUnits)
  } else if (substr(rowheaders, 1,3) == "hrs") {
    #sometimes, the txt files are exported with hrs as units
    channelNames <- c("Elapsed Time", channelNames)
    channelUnits <- c("Minutes", channelUnits)  
    #need to convert to minutes
    convertHrsToMins <- TRUE
  }
  
  
  #con <- file(rawfile)
  #open(con, "rb")

  #library(R.utils)
  #system.time(x <- gunzip(rawfile))
  #system.time(sc <- scan(file=gz, what="character", sep="\n"))
  
  #system.time(rawdat <- readLines(con=gz))
  
  readfile <- function(fname, chunkBytes=1024^3) { #default to 1GB
    con <- file(rawfile, open = "r")
    
    #readchar is very fast (and memory efficient), but no way of knowing how many chars to read
    morechars <- TRUE
    rawchars <- list()
    while (morechars == TRUE) {
      newchars <- readChar(con=con, nchars=chunkBytes, useBytes=TRUE)
      if (!identical(newchars, character(0))) {
        rawchars <- c(rawchars, newchars)
      } else {
        morechars <- FALSE
      }
    }
    close(con)
    rm(newchars)
    if (length(rawchars) == 1L) {
      return(rawchars[[1L]]) #just one element -- file read in one call
    } else {
      return(do.call(paste0, rawchars)) #paste together file chunks
    }
  }
  
  rawchars <- readfile(rawfile) #default to 1GB chunks (whole file)
  #system.time(rawchars2 <- readfile(rawfile, chunkBytes=1024*1024*100)) #100MB chunk
  
  #system.time(rawchar <- suppressWarnings(readChar(con=gz, nchars=1e9, useBytes=TRUE)))
  
  #seek(gz)
  #seek(gz, origin="current")
  #rawdat <- readBin(con, raw(), n=file.info(rawfile)$size*10)
  
  #can't get memDecompress working
  #x <- memDecompress(rawdat, type = "gzip") 
  #system.time(rawdat <- readLines(gzfile(rawfile)))
  bioData <- fread(input=rawchars, sep=",", colClasses="numeric", header=FALSE, skip=line)
  setnames(bioData, c(make.names(channelNames),"dummy")) #set names by ref
  rm(rawchars) #no longer need raw character data
  
  #works, but much slower
  #system.time(bioData2 <- data.table(read.table(file=rawfile, header=FALSE, sep=",", colClasses="numeric", comment.char="", quote="", skip=line, col.names=c(make.names(channelNames),"dummy"))))
  
  #there is always a trailing comma on the end of the files, which produces a dummy variable
  #bioData[,"dummy"] <- NULL
  bioData[,dummy:=NULL] #delete column without recreating df
  
  if (convertHrsToMins) {
    #convert elapsed time column to minutes from hours
    bioData[,Elapsed.Time := Elapsed.Time * 60] #modify by ref (30x faster!)
    #system.time(bioData$Elapsed.Time <- bioData$Elapsed.Time * 60)
  }
  
  #raw voltages on parallel port pins are +5 (on) or 0 (off)
  #convert these to 1/0 so that they can be converted from binary to decimal
  voltdivisor <- 5
  
  #system.time(pins <- subset(bioData, select=paste("PP.Pin.", 9:16, sep="")))
  if (!any(grepl("PP.Pin", names(bioData)))) {
    #if we just have generic "digital input" labels, assume 9:16
    pins <- bioData[,grep("Digital.input", names(bioData)), with=FALSE]
    setnames(pins, paste("PP.Pin.", 9:16, sep=""))
    #drop pin columns from bioData (since these are now converted to single integer codes)
    bioData <- bioData[, grep("Digital.input", names(bioData)) := NULL] #very rapid since does not recopy
  } else {
    pins <- bioData[,paste("PP.Pin.", 9:16, sep=""), with=FALSE]  
    #drop pin columns from bioData (since these are now converted to single integer codes)
    #bioData <- bioData[,!names(bioData) %in% paste("PP.Pin.", 9:16, sep="")]
    bioData <- bioData[, paste("PP.Pin.", 9:16, sep="") := NULL] #very rapid since does not recopy
  }
  
  pins <- pins/voltdivisor
  
  #Convert binary pins to integer codes using powers of 2
  #note that this basically reverses the info to be computed because the sequence 
  #goes 2^{0,1,2,3,4,5,6,7} so that the first element (PP.Pin.9) is the smallest power
  
  #doesn't seem any faster
  #bin2dec <- function(vec) { if (all(vec==0.0)) 0.0 else sum(vec * 2^(seq_along(vec) - 1)) }
  bin2dec <- function(vec) { sum(vec * 2^(seq_along(vec) - 1)) }
  
  #this would be the code for usual binary data
  #bin2dec <- function(vec) { sum(vec * 2^(rev(seq_along(vec)) - 1)) }
  
  system.time(trialCodes <- apply(pins, 1, bin2dec))
  #not sure how to do this at the moment
  #system.time(pins[, trialCode:=bin2dec(.SD), .SDcols=paste("PP.Pin.", 9:16, sep=""), by=1:nrow(pins)])
  rm(pins)
  gc()
  
  #only want the value of the pins to be non-zero when the stimulus switches.
  #by default in Inquisit, the pins stay "on" for the duration of a stimulus
  #the code below only passes trial codes when they shift, otherwise 0.
  curTrialCode <- -1
  for (i in 1:length(trialCodes)) {
    if (trialCodes[i] != curTrialCode) curTrialCode = trialCodes[i]
    else trialCodes[i] <- 0
  }
  
  bioData[,TrialCodes:=trialCodes]
  
  rm(trialCodes)
  
  if (setequal(names(bioData), c("Elapsed.Time", "Ear.Pulse...PPG100C", "Finger.Pulse...PPG100C", "Eyeblink...EMG100C",
          "Facial.1...EMG100C", "Facial.2...EMG100C", "ECG...ECG100C", "GSR...GSR100C", "Skin.Temperature...SKT100C",
          "Respiration...RSP100C", "ECG.Heart.Rate", "Ear.BPM", "Finger.BPM", "Respiration.Rate", "Eyeblink.Integrated", 
          "ECG.R.R", "Phasic.GSR", "TrialCodes"))) {
    
    setnames(bioData, c("Elapsed.Time", "Ear.Pulse.Raw", "Finger.Pulse.Raw", "Eyeblink.Raw",
        "Facial.EMG.1", "Facial.EMG.2", "ECG.Raw", "GSR.Raw", "Skin.Temperature.Raw", "Respiration.Raw",
        "ECG.Heart.Rate", "Ear.BPM", "Finger.BPM", "Respiration.Rate", "Eyeblink.Integrated",
        "ECG.R.R", "Phasic.GSR", "TrialCodes"))
    
  } else if (setequal(names(bioData), c("P1..ECG...ECG100C", "P1..GSR...GSR100C", "P2..ECG...ECG100C", "P2..GSR...GSR100C", "LED",
          "P1..ECG.Heart.Rate", "P1..ECG.R.R", "C2...Filter", "P2..ECG.Heart.Rate", "P2..ECG.R.R", "C5...Filter", "TrialCodes"))) {
    
    setnames(bioData, c("R.ECG", "R.GSR", "L.ECG", "L.GSR", "LED", "R.ECG.Heart.Rate", "R.ECG.R.R", 
        "R.SCR", "L.ECG.Heart.Rate", "L.ECG.R.R", "L.SCR", "TrialCodes"))
        
  } else if (setequal(names(bioData), c("Elapsed.Time", "P1..ECG...ECG100C", "P1..GSR...GSR100C", "P2..ECG...ECG100C", "P2..GSR...GSR100C", "LED",
          "P1..ECG.Heart.Rate", "P1..ECG.R.R", "C2...Filter", "P2..ECG.Heart.Rate", "P2..ECG.R.R", "C5...Filter", "TrialCodes"))) {
    
    setnames(bioData, c("Elapsed.Time", "R.ECG", "R.GSR", "L.ECG", "L.GSR", "LED", "R.ECG.Heart.Rate", "R.ECG.R.R", 
        "R.SCR", "L.ECG.Heart.Rate", "L.ECG.R.R", "L.SCR", "TrialCodes"))
    
  } else if (setequal(names(bioData), c("Elapsed.Time", "R..ECG...ECG100C", "R..GSR...GSR100C", "L..ECG...ECG100C", "L..GSR...GSR100C", "LED",
                                        "R..ECG.Heart.Rate", "R..ECG.R.R", "R..SCR.phasic", "L..ECG.Heart.Rate", "L..ECG.R.R", "L..SCR.phasic", "TrialCodes"))) {
    
    setnames(bioData, c("Elapsed.Time", "R.ECG", "R.GSR", "L.ECG", "L.GSR", "LED", "R.ECG.Heart.Rate", "R.ECG.R.R", 
                        "R.SCR", "L.ECG.Heart.Rate", "L.ECG.R.R", "L.SCR", "TrialCodes"))
  } else if (setequal(names(bioData), c("Elapsed.Time", "RSP..X..RSPEC.R", "ECG..Y..RSPEC.R", "EDA..Y..PPGED.R", "Heart.Rate", "Phasic.GSR", "ECG.R.R" ,"TrialCodes"))) {
    #SRLD 2016
    setnames(bioData, c("Elapsed.Time", "RSP", "ECG", "GSR", "ECG.Heart.Rate", "Phasic.GSR", "ECG.R.R", "TrialCodes"))
  } else {
    cat(paste(names(bioData), collapse=", "))
    stop("Non-matching names vector")
  }
  
  gc()
  return(bioData)
}

extractSignal <- function(taskName, channelName, startcode, stopcode, includeTrialCodes=FALSE, includeTime=FALSE) {
  curwd <- getwd()
  setwd("~/BPD_Psychophys")
  files <- list.files("~/BPD_Psychophys", ".*\\.Rdata")
  ids <- as.numeric(gsub("(\\d+)processed.Rdata", "\\1", files, perl=TRUE))
  dfNames <- paste("pp", ids, sep="")
  for (f in 1:length(files)) {
    load(file=files[f])
    curDF <- eval(parse(text=dfNames[f])) #reassign for easy reference
    rm(list=dfNames[f]) #remove the original
    
    startTask <- which(curDF$TrialCodes==startcode)
    stopTask <- which(curDF$TrialCodes==stopcode)
    
    if (length(startTask) > 1) {
      #sometimes all pins were on from the start of the experiment (weird)
      #affects baseline task because 255 (all on) is the start code for this task
      warning("multiple matches for id: ", ids[f], ", start code position:", paste(startTask, collapse=", "))
      if (startTask[1] == 1) startTask <- startTask[2]
    }
    if (length(stopTask) > 1) stop("multiple matches for stop code position:", paste(stopTask, collapse=", "))
    
    colsOfInterest <- c(channelName)
    if (includeTime == TRUE) colsOfInterest <- c("Elapsed.Time", colsOfInterest)
    if (includeTrialCodes == TRUE) colsOfInterest <- c(colsOfInterest, "TrialCodes")
    
    SignalDF <- curDF[startTask:stopTask, colsOfInterest]
    
    write.table(SignalDF, file=paste("~/BPD_Psychophys/", ids[f], "_", taskName, "_", channelName, ".dat", sep=""), 
        row.names=FALSE, col.names=FALSE)
    
    rm(curDF)
  }
  
  #reset wd
  setwd(curwd)
}
