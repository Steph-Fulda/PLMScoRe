##### Preparation of RL files
#####

RL_event_recode<-function(RLs, d1,...){

  #####Helper function, not foreseen to be called by the user
  #####Based on the specified annotations in RLs, creates numeric codes for all events
  #####		that will ensure that the following scoring routines always have the same imput
  #####
  #####		required input:
  #####			RLs 		- REMLogic specification file (maybe empty)
  #####			d1  		- extracted data table from REMLogic txt file
  #####			filestart 	- Where does the data table start (number)
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
  e_no<-RLs[[1]][[6]][[4]]	#column number of events
  c_no<-RLs[[1]][[6]][[6]]	#column number of channel information
  #Legs
  if(!is.na(RLs[[1]][[1]][[1]])){
    llm<-which(d1[,c_no]==RLs[[1]][[1]][[1]] & is.element(d1[,e_no], RLs[[1]][[1]][[3]]))
    if(length(llm)>0) {d1$T[llm]<-2; d1$T2[llm]<-10}
  }
  if(!is.na(RLs[[1]][[1]][[2]])){
    rlm<-which(d1[,c_no]==RLs[[1]][[1]][[2]] & is.element(d1[,e_no], RLs[[1]][[1]][[4]]))
    if(length(rlm)>0) {d1$T[rlm]<-2; d1$T2[rlm]<-11}
  }
  #Sleep
  if(!is.na(RLs[[1]][[2]][[1]]) & RLs[[1]][[2]][[1]]==1){
    wake<-which(is.element(d1[,e_no], RLs[[1]][[2]][[2]]))
    if(length(wake)>0) {d1$T[wake]<-1; d1$T2[wake]<-0}
    n1<-which(is.element(d1[,e_no], RLs[[1]][[2]][[3]]))
    if(length(n1)>0) {d1$T[n1]<-1; d1$T2[n1]<-1}
    n2<-which(is.element(d1[,e_no], RLs[[1]][[2]][[4]]))
    if(length(n2)>0) {d1$T[n2]<-1; d1$T2[n2]<-2}
    n3<-which(is.element(d1[,e_no], RLs[[1]][[2]][[5]]))
    if(length(n3)>0) {d1$T[n3]<-1; d1$T2[n3]<-3}
    rem<-which(is.element(d1[,e_no], RLs[[1]][[2]][[6]]))
    if(length(rem)>0) {d1$T[rem]<-1; d1$T2[rem]<-4}
  }
  #Arousal
  if(!is.na(RLs[[1]][[3]][[1]])){
    ar<-which(is.element(d1[,e_no], RLs[[1]][[3]][[2]]))
    if(length(ar)>0) {d1$T[ar]<-4; d1$T2[ar]<-30}
  }
  #Respiratory events
  if(!is.na(RLs[[1]][[4]][[1]])){
    re<-which(is.element(d1[,e_no], RLs[[1]][[4]][[2]]))
    if(length(re)>0) {d1$T[re]<-3; d1$T2[re]<-20}
  }
  #Start/stop
  if(!is.na(RLs[[1]][[5]][[1]])){
    if(!is.na(RLs[[1]][[5]][[2]])){
      start<-which(is.element(d1[,e_no], RLs[[1]][[5]][[2]]))
      if(length(start)>0) {d1$T[start]<-5; d1$T2[start]<-51}
    }
    if(!is.na(RLs[[1]][[5]][[3]])){
      stop<-which(is.element(d1[,e_no], RLs[[1]][[5]][[3]]))
      if(length(stop)>0) {d1$T[stop]<-5; d1$T2[stop]<-52}
    }

  }
  d1<-d1[!is.na(d1$T),]
  return(d1)
}


format_time<-function(RLs, d1,...){

  #####Helper function, not foreseen to be called by the user
  #####Based on the specified annotations in RLs, just applies time format to all events (in a new variable time)
  #####
  #####		required input:
  #####			RLs 		- REMLogic specification file (maybe empty)
  #####			d1  		- extracted data table from REMLogic txt file
  #####		optional input:
  #####			none
  #####		output:
  #####			an updated data table that will contain one new column
  #####			Time = formatted date/time

  d1$Time<-NA;
  d1$Time<-strptime(d1[,RLs[[1]][[6]][[3]]], format=RLs[[1]][[6]][[1]])
  return(d1)
}

determine_startstop<-function(RLs, d1,...){

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

  d1$Onset<-NA;
  d1<-d1[order(d1$Time),] ###sort by time
  ##Start
  starty<-0
  s1<-which(d1$T2==51)
  if(length(s1)==1) {
    start<-d1$Time[s1]
    starty<-1
  }
  if(length(s1)>1){
    start<-d1$Time[s1[length(s1)]]
    message("!!!\t More than one start/lights off signal found,\n\t the latest one will be considered !!!\n")
    starty<-1
  }
  if(starty==0){
    s2<-which(d1$T==1)
    if(length(s2)>0){
      start<-d1$Time[s2[1]]
      message("!!!\t No start/lights off signal found/defined,\n\t the first wake/sleep epoch present will be considered !!!\n")
      starty<-1
    }
  }
  if(starty==0) {
    start<-d1$Time[1]-30
    message("!!!\t No start/lights off signal or sleep scoring found/defined,\n\t it will be assumed that the recording started 30s before\n\t the first found event (LM, arousal, respiratory event) !!!\n")
  }
  RLs[[2]][[5]]<-start

  if(length(s1)<1){
    d1<-rbind(d1, rep(NA, 24))
    d1$T[dim(d1)[1]]<-5; d1$T2[dim(d1)[1]]<-51;
    d1$Time[dim(d1)[1]]<-start; d1[dim(d1)[1],RLs[[1]][[6]][[5]]]<-0;
    d1$Onset[dim(d1)[1]]<-0
  }


  ##stop
  stopy<-0
  st1<-which(d1$T2==52)
  if(length(st1)==1) {
    stop<-d1$Time[st1]
    stopy<-1
  }
  if(length(st1)>1){
    stop<-d1$Time[st1[length(st1)]]
    message("!!!\t More than one stop/lights on signal found,\n\t the latest one will be considered !!!\n")
    stopy<-1
  }
  if(stopy==0){
    st2<-which(d1$T==1)
    if(length(st2)>0){
      stop<-d1$Time[st2[length(st2)]]
      message("!!!\t No stop/lights on signal found/defined,\n\t the last wake/sleep epoch present will be considered !!!\n")
      stopy<-1
    }
  }
  if(stopy==0){
    d1<-d1[order(d1$Time),]
    stop<-d1$Time[dim(d1)[1]]+as.numeric(as.character(d1$Dur[dim(d1)[1]]))+30
    message("!!!\t No stop/lights on signal or sleep scoring found/defined,\n\t it will be assumed that the recording stopped 30s after\n\t the last found event (LM, arousal, respiratory event) !!!\n")
  }
  RLs[[2]][[6]]<-stop

  if(length(st1)<1){
    d1<-rbind(d1, rep(NA, 24))
    d1$T[dim(d1)[1]]<-5; d1$T2[dim(d1)[1]]<-52;
    d1$Time[dim(d1)[1]]<-stop; d1[dim(d1)[1],RLs[[1]][[6]][[5]]]<-0;
    d1$Onset[dim(d1)[1]]<-as.numeric(difftime(stop, start, units="sec"))
  }


  ####transform time to sec from start
  d1$Onset<-as.numeric(difftime(d1$Time,start,  units="sec"))

  #Create new variable offset
  names(d1)[RLs[[1]][[6]][[5]]]<-"Dur"; d1$Dur<-as.numeric(as.character(d1$Dur))
  d1$Offset<-d1$Onset+d1$Dur

  ####remove everything before start
  d1<-d1[d1$Offset>=0,]
  ####remove everything after stop
  h<-which(as.numeric(difftime(d1$Time, stop, units="sec"))> 0)
  if(length(h)>0) d1<-d1[-h,]

  ####if sleep is scored and start/stop is within an epoch of sleep change respective on/offset
  h1<-which(d1$Onset<0 & d1$Offset>0 & d1$T==1)
  if(length(h1)==1) {d1$Onset[h1]<-0; d1$Dur[h1]<-d1$Offset[h1]-d1$Onset[h1]}
  stop2<-as.numeric(difftime(stop, start, units="sec"))
  h2<-which(d1$Onset<stop2 & d1$Offset>stop2 & d1$T==1)
  if(length(h2)==1) {d1$Offset[h2]<-stop2; d1$Dur[h2]<-d1$Offset[h2]-d1$Onset[h2]}
  ####if sleep scored and start/stop is outside these epochs add wake before/after
  d1<-d1[order(d1$Onset),]
  h3<-which(d1$T==1)
  if(length(h3)>0 && d1$Onset[h3[1]]>0){
    d1<-rbind(d1, rep(NA, 24))
    d1$T[dim(d1)[1]]<-1; d1$T2[dim(d1)[1]]<-0;
    d1$Onset[dim(d1)[1]]<-0; d1$Offset[dim(d1)[1]]<-d1$Onset[h3[1]]
    d1$Dur[dim(d1)[1]]<-d1$Offset[dim(d1)[1]]-d1$Onset[dim(d1)[1]]
    d1<-d1[order(d1$Onset),]
  }
  if(length(h3)>0 && d1$Offset[h3[length(h3)]]<d1$Onset[d1$T2==52]){
    d1<-rbind(d1, rep(NA, 24))
    d1$T[dim(d1)[1]]<-1; d1$T2[dim(d1)[1]]<-0;
    d1$Onset[dim(d1)[1]]<-d1$Offset[h3[length(h3)]]; d1$Offset[dim(d1)[1]]<-d1$Onset[d1$T2==52]
    d1$Dur[dim(d1)[1]]<-d1$Offset[dim(d1)[1]]-d1$Onset[dim(d1)[1]]
    d1<-d1[order(d1$Onset),]
  }
  ####if no sleep scored add wake for complete period
  d1<-d1[order(d1$Onset),]
  h5<-which(d1$T==1)
  if(length(h5)<1){
    d1<-rbind(d1, rep(NA, 24))
    d1$T[dim(d1)[1]]<-1; d1$T2[dim(d1)[1]]<-0;
    d1$Onset[dim(d1)[1]]<-0;
    d1$Offset[dim(d1)[1]]<-d1$Onset[d1$T2==52 & !is.na(d1$T2)]
    d1$Dur[dim(d1)[1]]<-d1$Offset[dim(d1)[1]]-d1$Onset[dim(d1)[1]]
    d1<-d1[order(d1$Onset),]
  }

  ##housekeeping
  row.has.na <- apply(d1, 1, function(x){all(is.na(x))})
  if(length(which(row.has.na==TRUE))>0) d1<-d1[-which(row.has.na==TRUE),]

  temp<-list(d1, RLs)
  return(temp)
}

