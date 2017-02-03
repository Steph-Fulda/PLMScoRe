###Prepare edf table file

###################
### Recode events based on RLs specifications
###################
edf_event_recode<-function(d1,...){

  #####Helper function, not foreseen to be called by the user
  #####Based on the specified annotations in RLs, creates numeric codes for all events
  #####		that will ensure that the following scoring routines always have the same imput
  #####
  #####		required input:
  #####			d1 		- annotation from edf plus file
  #####		optional input:
  #####			none
  #####		output:
  #####			an updated data table that will contain two new columns
  #####			T = code domain area
  #####				1 = Sleep
  #####				2 = leg movements
  #####				3 = respiratory events
  #####				4 = arousals
  #####				5 = start/stop events
  #####			T2 = codes within domain area
  #####				0,1,2,3,4 = wake, N1, N2, N3, REM
  #####				10, 11 = left, right leg movement (at a later stage 12 will be added for bilateral LM)
  #####				20 = respiratory events (for now, maybe later differentiate between the different R events)
  #####				30 = arousal (again maybe later more differentiation)
  #####				51, 52 = lights off/start, lights on/stop

  d1$T<-NA; d1$T2<-NA

  #LM
  d1$T[d1$Event=="Limb movement"]<-2
  d1$T2[d1$Event=="Limb movement" & d1$Loc=="EMG RAT"]<-11
  d1$T2[d1$Event=="Limb movement" & d1$Loc=="EMG LAT"]<-10

  #Sleep
  d1$T[d1$Event=="Sleep stage W"]<-1; d1$T2[d1$Event=="Sleep stage W"]<-0
  d1$T[d1$Event=="Sleep stage N1"]<-1; d1$T2[d1$Event=="Sleep stage N1"]<-1
  d1$T[d1$Event=="Sleep stage N2"]<-1; d1$T2[d1$Event=="Sleep stage N2"]<-2
  d1$T[d1$Event=="Sleep stage N3"]<-1; d1$T2[d1$Event=="Sleep stage N3"]<-3
  d1$T[d1$Event=="Sleep stage R"]<-1; d1$T2[d1$Event=="Sleep stage R"]<-4

    #Arousal
  d1$T[d1$Event=="EEG arousal"]<-4; d1$T2[d1$Event=="EEG arousal"]<-30

  #Respiratory events
  h<-which(is.element(d1$Event, c("Central apnea", "Apnea", "Obstructive apnea", "Mixed apnea", "Hypopnea", "RERA", "Obstructive hypopnea",
                                  "Central hypopnea")))
  d1$T[h]<-3
  d1$T2[h]<-20


  #Start/stop
  d1$T[d1$Event=="Lights off"]<-5; d1$T2[d1$Event=="Lights off"]<-51
  d1$T[d1$Event=="Lights on"]<-5; d1$T2[d1$Event=="Lights on"]<-52

  d1<-d1[!is.na(d1$T),]

  return(d1)
}


###################
### Determin start/stop of recording
###################
determine_startstop_edf<-function(d1,...){

  #####Helper function, not foreseen to be called by the user
  #####Based on the specified annotations in RLs, searches for a start and stop signal
  #####
  #####		required input:
  #####			RLs 		- REMLogic specification file (maybe empty)
  #####			d1  		- extracted data table from REMLogic txt file
  #####		optional input:
  #####			none
  #####		output:
  #####			updated RLs with two new entries for start and stop (in time)
  #####			updated d1 data table with:
  #####				- new column "Onset" where start times are expressed as seconds from start of registration
  #####				- with all events that end before the start and start after the stop removed
  #####
  #####		Determination of start, in preferred order
  #####		(1) if a start event is defined and that start event is present
  #####		    this will be taken as the start of the TIB
  #####		    [if more than 1 start event are present, the latest one will be taken]
  #####		(2) if no start event is present but sleep is scored,
  #####		    the first scored wake/sleep epoch is taken as the start
  #####		(3) if no start event is present and no sleep is scored
  #####		    it will be assumed that the registration started 30 s before the
  #####		    first event (any of LM, arousal, respiration)
  #####
  #####		Determination of stop, in preferred order
  #####		(1) if a stop event is defined and that stop event is present
  #####		    this will be taken as the start of the TIB
  #####		    [if more than 1 stop event are present, the latest one will be taken]
  #####		(2) if no stop event is present but sleep is scored,
  #####		    the last scored wake/sleep epoch is taken as the stop
  #####		(3) if no stop event is present and no sleep is scored
  #####		    it will be assumed that the registration stopped 30 s after the
  #####		    last event (any of LM, arousal, respiration)

  d1<-d1[order(d1$Onset),] ###sort by time


  ##Start
  start1<-NA #Lights on
  start2<-NA #Wake/sleep
  start3<-NA #30 s before 1st event
  start<-NA

  s1<-which(d1$T2==51)
  if(length(s1)==1) {
    start1<-d1$Onset[s1]
    starty<-1
  }
  if(length(s1)>1){
    start1<-d1$Onset[s1[length(s1)]]
    message("!!!\t More than one start/lights off signal found,\n\t the latest one will be considered !!!\n")
  }

  s2<-which(d1$T==1)
    if(length(s2)>0){
      start2<-d1$Onset[s2[1]]
    }

  #if not lights off signal and no sleep scoring
  if(is.na(start1) & is.na(start2)){
    start3<-d1$Onset[!is.na(d1$T)][1]-30
    start<-start3
    message("!!!\t No start/lights off signal or sleep scoring found/defined,\n\t it will be assumed that the recording started 30s before\n\t the first found event (LM, arousal, respiratory event) !!!\n")
  }



  #if scoring starts after start, take scoring as start
  if(is.na(start1) & !is.na(start2)){
    message("!!!\tNo start/lights off signal found, it will be assumed that recording started with first scored wake/sleep epoch")
    start<-start2
  }
  if(is.na(start2) & !is.na(start1)) start<-start1

  if(!is.na(start1) & !is.na(start2)){
    if(start1<start2) {
      start<-start2
      message("!!!\t Scoring started after start/lights off signal,\n\t the first wake/sleep epoch present will be considered !!!\n")
    }
    if(start1>=start2) {
      start<-start1
      message("!!!\t Scoring started before start/lights off signal,\n\t the start/lights off signal present will be considered !!!\n")
    }
    }



  ##stop
  stop1<-NA
  stop2<-NA
  stop3<-NA
  stop<-NA

  st1<-which(d1$T2==52)
  if(length(st1)==1) {
    stop1<-d1$Onset[st1]
  }
  if(length(st1)>1){
    stop1<-d1$Onset[st1[length(st1)]]
    message("!!!\t More than one stop/lights on signal found,\n\t the latest one will be considered !!!\n")
  }
  st2<-which(d1$T==1)
    if(length(st2)>0){
      stop2<-d1$Onset[st2[length(st2)]]+d1$Dur[st2[length(st2)]]
      #message("!!!\t No stop/lights on signal found/defined,\n\t the last wake/sleep epoch present will be considered !!!\n")
    }

  if(is.na(stop1) & is.na(stop2)){
    stop<-d1$Onset[!is.na(d1$Onset)][length(d1$Onset[!is.na(d1$Onset)])]+d1$Dur[!is.na(d1$Onset)][length(d1$Onset[!is.na(d1$Onset)])]+30
    message("!!!\t No stop/lights on signal or sleep scoring found/defined,\n\t it will be assumed that the recording stopped 30s after\n\t the last found event (LM, arousal, respiratory event) !!!\n")
  }

  #if scoring stops after stop, take scoring as start
  if(is.na(stop1) & !is.na(stop2)){
    message("!!!\tNo stop/lights on signal found, it will be assumed that recording stopped with last scored wake/sleep epoch")
    stop<-stop2
  }
  if(is.na(stop2) & !is.na(stop1)) stop<-stop1

  if(!is.na(stop1) & !is.na(stop2)){
    if(stop1>stop2) {
      stop<-stop2
      message("!!!\t Scoring stopped before stop/lights on signal,\n\t the last wake/sleep epoch present will be considered !!!\n")
    }
    if(stop1<=stop2) {
      stop<-stop1
      message("!!!\t Scoring stopped after stop/lights on signal,\n\t the stop/lights on signal present will be considered !!!\n")
    }
  }




  ####transform time to sec from start
  d1$Onset<-d1$Onset-start


  #Create new variable offset
  d1$Offset<-d1$Onset+d1$Dur

  ####remove everything before start
  d1<-d1[(is.na(d1$Offset) & d1$Onset>=0) | (!is.na(d1$Offset) & d1$Offset>=0),]

  ####remove everything after stop
  d1<-d1[d1$Onset<=(stop-start),]

  ####if sleep is scored and start/stop is within an epoch of sleep change respective on/offset
  h1<-which(d1$Onset<0 & d1$Offset>0 & d1$T==1)
  if(length(h1)==1) {d1$Onset[h1]<-0; d1$Dur[h1]<-d1$Offset[h1]-d1$Onset[h1]}

  h2<-which(d1$Onset<stop-start & d1$Offset>stop-start & d1$T==1)
  if(length(h2)==1) {d1$Offset[h2]<-stop-start; d1$Dur[h2]<-d1$Offset[h2]-d1$Onset[h2]}

  ####if no sleep scored add wake for complete period
  d1<-d1[order(d1$Onset),]
  h5<-which(d1$T==1)
  if(length(h5)<1){
    d1<-rbind(d1, rep(NA, 24))
    d1$T[dim(d1)[1]]<-1; d1$T2[dim(d1)[1]]<-0;
    d1$Onset[dim(d1)[1]]<-0;
    d1$Offset[dim(d1)[1]]<-stop-start
    d1$Dur[dim(d1)[1]]<-stop-start
    d1<-d1[order(d1$Onset),]
  }

  ##housekeeping

  row.has.na <- rowSums(!is.na(d1))
  if(length(which(row.has.na==0))>0) d1<-d1[-which(row.has.na==0),]


  return(d1)
}


