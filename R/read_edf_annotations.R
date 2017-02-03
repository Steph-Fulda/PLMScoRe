###Read edf annotation
EDF.getAnnotation.steph4<-function(edf,...){
  ##for den Haag polyman files with only a single annotation chanel
  ## edf = EDF.import(fname)

  EDFfile<-EDF.import(edf)
  dtRcdEnd<-cumsum(EDFfile$nrDataRecord)
  dtRcdStart<-1
  dtRcdSize <- dtRcdEnd[length(dtRcdEnd)]
  u<-EDFfile$numberDataRecords

  # Get the length of the header, reopen the file and seek to the correct position
  to.read <- file(EDFfile$filename, "rb", encoding="UTF-16LE")
  seek(to.read, EDFfile$nByteinHeader) # 2 byte signed integer, skip header

  dtRcd <- readBin(to.read, n=dtRcdSize*u, what="integer", size=2, signed=T) ##needs to be checked with larger annotation files
  close(to.read)

  # Extraction of the relevant part
  recode <- function(v) { return(c(intToUtf8(v %% 256), intToUtf8(v %/% 256))) }

  dtRcdRaw <- c(paste(unlist(lapply(dtRcd,recode)),collapse=''))
  dtRcdRaw1 <- unlist(strsplit(dtRcdRaw,split="\024"))
  dtRcdRaw2 <- unlist(strsplit(dtRcdRaw1,split="\025"))
  dtRcdRaw3 <- unlist(strsplit(dtRcdRaw2,split="@@"))[-1]
  ###is vector with first entry +0, than start - duration - event - channel
  ### for Sleep stage no Loc channel

  k<-grep("+",dtRcdRaw3, fixed=TRUE)
  Onset<-(as.character(dtRcdRaw3[k]))
  Dur<-(as.character(dtRcdRaw3[k+1]))
  Event<-as.character(dtRcdRaw3[k+2])
  Loc<-as.character(dtRcdRaw3[k+3]); k2<-which(diff(k)<4); Loc[k2]<-NA
  t<-cbind(Onset=Onset, Dur=Dur, Event=Event, Loc=Loc)
  t1<-as.data.frame(t,stringsAsFactors=FALSE)

  suppressWarnings(h<-which(is.na(as.numeric(as.character(t1$Dur)))))
  t1$Event[h]<-t1$Dur[h]
  t1$Onset<-as.numeric(as.character(t1$Onset))
  suppressWarnings(t1$Dur<-as.numeric(as.character(t1$Dur)))


  return(t1)
}


EDF.import <- function(infile) {
  # This function permits to import the header part of an EDF file
  #
  # Currently: EDF+ is managed but not verified
  #            Discontinuous signals are not detected
  #            No error check is performed
  #            Annotations are not correctly decomposed
  #

  # Open the file (bad: no error check)
  to.read = file(infile, "rb", encoding="UTF-16LE")
  # Get header
  header <- as.integer(readChar(to.read, 8, T))
  # Get the localPatientId
  localPatientId <- readChar(to.read, 80, T)
  # Get the localRecordId
  localRecordId <- readChar(to.read, 80, T)
  # Get the StartDate and StartTime
  startDate <- strptime(paste(readChar(to.read, 8, T), readChar(to.read, 8, T), sep=" "),
                        format="%d.%m.%y %H.%M.%S")
  # Get the number of byte in header
  nByteinHeader <- as.integer(readChar(to.read, 8, T))
  # Reserved
  reserved <- readChar(to.read, 44, T)
  # Number of data Record
  numberDataRecords <- as.integer(readChar(to.read, 8, T))
  # Duration of data record
  durationDataRecord <- as.integer(readChar(to.read, 8, T))
  # Number of signals
  ns <- as.integer(readChar(to.read, 4, T))
  # Constructing the signals labels
  sLabelsRaw<-readChar(to.read, ns*16, T)
  sLabels <- c()
  for(i in seq(from=1,to=16*ns,by=16)) {
    sLabels <- c(sLabels,substr(sLabelsRaw, start=i, stop=i+15))
  }
  # Construction of the transducers
  transducersRaw <- readChar(to.read, ns*80, T)
  transducers <- c()
  for(i in seq(from=1,to=80*ns,by=80)) {
    transducers <- c(transducers, substr(transducersRaw, start=i, stop=i+79))
  }
  # Physical Dimension
  physicalDimRaw <- readChar(to.read, ns*8, T)
  physicalDimension <- c()
  for(i in seq(from=1, to=8*ns, by=8)) {
    physicalDimension <- c(physicalDimension, substr(physicalDimRaw, start=i, stop=i+7))
  }
  # Physical Minimum
  physicalMinRaw <- readChar(to.read, ns*8, T)
  physicalMinimum <- c()
  for(i in seq(from=1, to=8*ns, by=8)) {
    physicalMinimum <- c(physicalMinimum, substr(physicalMinRaw, start=i, stop=i+7))
  }
  physicalMinimum <- as.integer(physicalMinimum)
  # Physical Maximum
  physicalMaxRaw <- readChar(to.read, ns*8, T)
  physicalMaximum <- c()
  for(i in seq(from=1, to=8*ns, by=8)) {
    physicalMaximum <- c(physicalMaximum, substr(physicalMaxRaw, start=i, stop=i+7))
  }
  physicalMaximum <- as.integer(physicalMaximum)
  # Digital Minimum
  digitalMinRaw <- readChar(to.read, ns*8, T)
  digitalMinimum <- c()
  for(i in seq(from=1, to=8*ns, by=8)) {
    digitalMinimum <- c(digitalMinimum, substr(digitalMinRaw, start=i, stop=i+7))
  }
  digitalMinimum <- as.integer(digitalMinimum)
  # digital Maximum
  digitalMaxRaw <- readChar(to.read, ns*8, T)
  digitalMaximum <- c()
  for(i in seq(from=1,to=8*ns,by=8)) {
    digitalMaximum <- c(digitalMaximum, substr(digitalMaxRaw, start=i, stop=i+7))
  }
  digitalMaximum <- as.integer(digitalMaximum)
  # prefiltering
  prefilteringRaw <- readChar(to.read, ns*80, T)
  prefiltering <- c()
  for(i in seq(from=1, to=80*ns, by=80)) {
    prefiltering <- c(prefiltering,substr(prefilteringRaw, start=i, stop=i+79))
  }
  # nData record
  nrDataRecordRaw <- readChar(to.read, ns*8, T)
  nrDataRecord <- c()
  for(i in seq(from=1, to=8*ns, by=8)) {
    nrDataRecord <- c(nrDataRecord, substr(nrDataRecordRaw, start=i, stop=i+7))
  }
  nrDataRecord <- as.integer(nrDataRecord)
  # reserved
  reserved <- readChar(to.read, 32, T)
  # Data Record are not read here
  # End
  close(to.read)
  # Provide annotation flags
  strmatch <- "EDF Annotation"
  fAnnotation <- function(x) { substr(x, start=1, stop=nchar(strmatch)) == strmatch }
  mtch <- unlist(lapply(sLabels, fAnnotation))

  # Construct an object of class EDF
  obj <- list(
    filename=infile,
    header=header,
    localPatientId=localPatientId,
    localRecordId=localRecordId,
    startDate=startDate,
    nByteinHeader=nByteinHeader,
    numberDataRecords=numberDataRecords,
    durationDataRecord=durationDataRecord,
    ns=ns,
    sLabels=sLabels,
    transducers=transducers,
    physicalDimension=physicalDimension,
    physicalMinimum=physicalMinimum,
    physicalMaximum=physicalMaximum,
    digitalMinimum=digitalMinimum,
    digitalMaximum=digitalMaximum,
    prefiltering=prefiltering,
    nrDataRecord=nrDataRecord,
    annotationFlags=mtch
  )
  class(obj) <- "EDF"
  return(obj)
}
