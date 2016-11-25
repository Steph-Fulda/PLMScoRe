### Utilities, get REMLogic specific user input
###

getinfo_txtfile<-function(RLs,filestart,...){

  #####Helper function, not foreseen to be called by the user
  #####Asks user specify the format/specification of the txt file
  #####		required input:
  #####			RLs 		- REMLogic specification file (maybe empty)
  #####			fn  		- file name and path to REMLogic event txt file
  #####			filestart 	- Where does the data table start (number)
  #####		required input, that will be asked:
  #####			- Number of columns (must be > 3)
  #####			- Which column contains the start times (number)
  #####			- Which column contains the event names (number)
  #####			- Which column contains the channel information (number)
  #####			- Which column contains the duration information (number)
  #####			- Time format
  #####		optional input:
  #####			- Is there a sleep stage column? (with sleep stage for each event) (y/n)
  #####			- If yes, which column contains the sleep stage information (number)
  #####		output:
  #####			The asked for information will be written to the RL specification file,
  #####			which will be returned.
  #####			If any of the required information is missing, the function will produce an
  #####			error and stop.

  q1<-c("Put in number:   ")
  m1a<-c("How many data columns do you have?")
  m1b<-c("What is the column with the start times?")
  m1c<-c("What is the column with the duration information?")
  m1d<-c("What is the column with the event names?")
  m1e<-c("What is the column with the channel information?")
  m1f<-c("If you have a column with sleep stages for each event,...")

  m1j<-c("These are your column names.\n")
  w1b<-c("Column with start times is needed.")
  w1c<-c("Column with duration times is needed.")
  w1d<-c("Column with event names is needed.")
  w1e<-c("Column with channel information is needed.")
  w1f<-c("Time format needs to be correctly specified.")
  stop1<-c("Execution stopped...")

  # No of columns
  message(m1a); RLs[[1]][[6]][2]<-as.integer(readline(prompt=q1))
  if(is.na(RLs[[1]][[6]][2])|RLs[[1]][[6]][2]<4) stop("We need at least 4 columns (start, duration, event, location)! Execution stopped...")

  d0<-scan(RLs[[2]][[1]], what="character", sep="\t", quote="", quiet=TRUE, strip.white=FALSE, blank.lines.skip=FALSE)
  col_names<-d0[filestart:(filestart+RLs[[1]][[6]][[2]]-1)]

  #Start times column number
  RLs[[1]][[6]][3]<-ask_info_no(lookup=col_names, display=1, mult=0, req1=1,lumess=m1j, mess1=m1b, warn1=w1b, stop1=stop1, p0=q1)
  #Event names column number
  RLs[[1]][[6]][4]<-ask_info_no(display=0, mult=0, req1=1, mess1=m1d, warn1=w1d, stop1=stop1, p0=q1)
  #Duration column number
  RLs[[1]][[6]][5]<-ask_info_no(display=0, mult=0, req1=1, mess1=m1c, warn1=w1c, stop1=stop1, p0=q1)
  #Channel names column number
  RLs[[1]][[6]][6]<-ask_info_no(display=0, mult=0, req1=1, mess1=m1e, warn1=w1e, stop1=stop1, p0=q1)
  #Sleep stage column number (optional)
  RLs[[1]][[6]][7]<-ask_info_no(display=0, mult=0, req1=0, mess1=m1f, p0=q1)

  #Quick check
  if(length(which(duplicated(RLs[[1]][[6]][3:7])))>0|
     any(RLs[[1]][[6]][3:6]>RLs[[1]][[6]][[2]], na.rm=TRUE)) stop("One or more column number are not correct")
  if(!is.na(RLs[[1]][[6]][7]) & RLs[[1]][[6]][7]>RLs[[1]][[6]][[2]]) RLs[[1]][[6]][7]<-NA


  #Ask for and check time format
  ex_tf<-d0[filestart+RLs[[1]][[6]][[2]]-1+RLs[[1]][[6]][[3]]]
  RLs[[1]][[6]][1]<-check_timeformat(ex_tf=ex_tf)
  if(is.na(RLs[[1]][[6]][1])) {message(w1f); stop(stop1)}

  return(RLs)
}


check_timeformat<-function(RLs_tf=NA, ex_tf,...){

  #####Helper function, not foreseen to be called by the user
  #####Checks and asks for time format input
  #####		required input:
  #####			ex_tf  - Example of the time format, to be tested
  #####		optional input:
  #####			RLs_tf - Time format (string) in REMLogic specification file
  #####		output:
  #####			correct time format (string) or NA

  as_time<-"n"

  if(!is.na(RLs_tf)){
    test1<-try(as.Date(ex_tf, format=RLs_tf))
    if(class(test1)=="try-error" & !is.na(test1)) {return(RLs_tf); stop()}
    if(class(test1)=="try-error"| is.na(test1)) RLs_tf<-NA
  }

  if(is.na(RLs_tf)){
    m1g<-c("This is an example of the time format in this file:")
    m1h<-c("Does this format correspond to the standard format 'YYYY-MM-DDTHH:MM:SS.ssss' ?")
    w1a<-c("****Please see http://stat.ethz.ch/R-manual/R-devel/library/base/html/as.Date.html for help on how to specify time formats")
    w1b<-("That is not the correct format. Please specify the correct one.")

    message(m1g); print(ex_tf)
    message(m1h); as_time<-eval_answer2(readline(prompt="(y/n or 1/0):    "), d=1)
    if(as_time==1){
      test1<-try(as.Date(ex_tf, format="%Y-%m-%dT%H:%M:%OS"))
      if(class(test1)=="try-error"||is.na(test1)) {
        message(w1b)
        as_time<-0
      }else{return("%Y-%m-%dT%H:%M:%OS"); stop()}
    }
    counter<-1
    while(as_time==0 & counter<4){
      message(w1a)
      new_format<-readline(prompt="Please state the correct format:    \n")
      if(!is.na(new_format)){
        test1<-try(as.Date(ex_tf, format=new_format))
        if(class(test1)=="try-error"||is.na(test1)) {
          message(w1b)
          as_time<-0; counter<-counter+1
        } else {
          as_time<-1
          return(new_format)
        }
      }
    }
  }
  if(as_time==0) return(NA)
}

getinfo_legs<-function(RLs, d1,...){

  #####Helper function, not foreseen to be called by the user
  #####Asks user specify the format/specification of the txt file
  #####		required input:
  #####			RLs 		- REMLogic specification file (cannot be empty)
  #####			d1  		- Data table derived from REMLogic event txt file
  #####		required input that will be asked for:
  #####			- At least one leg channel name
  #####			- At least one LM event
  #####		optional input:
  #####			none
  #####		output:
  #####			The asked for information will be written to the RL specification file,
  #####			which will be returned.
  #####			If some minimum required information is missing, the function will produce an
  #####			error and stop.

  q1<-c("Put in number:   ")
  q2<-c("Put in number(s):   ")

  m2a<-c("\nPlease select LEFT leg channel.")
  m2b<-c("\nPlease select RIGHT leg channel.")
  m2c<-c("LEFT Leg: Please select all events that denote single LM:")
  m2d<-c("RIGHT Leg: Please select all events that denote single LM:")
  w2a<-c("*** 'PLM' often or always denote the PLM series and NOT single LM,\n consider carefully if you want choose it!")
  w2b<-c("*** If there is more than one, use R notation eg. 1,2,3 or 1:3, or 1,2,5:7")
  w2c<-c("*** [1] 'all annotations' refers to the complete list of annotations\n         given in the header of the REMLogic event file.\n    [2] 'scored annotations' refer to annotations used in this particular \n         scoring session, and found in the events column of the REMLogic event file.")

  gw1<-c("No selection")
  stop1<-c("Execution stopped...")

  ##Leg channel info
  channels<-names(table(d1[,RLs[[1]][[6]][[6]]]))
  RLs[[1]][[1]][1]<-ask_info(channels, display=1, mult=0, req1=0, p0=q1, warn1=gw1, stop1=stop1, mess1=m2a, lumess="\nThese are the available channels.\n")
  RLs[[1]][[1]][2]<-ask_info(channels, display=0, mult=0, req1=0, p0=q1, warn1=gw1, stop1=stop1, mess1=m2b)
  if(all(is.na(RLs[[1]][[1]][1:2]))) stop("At least one leg channel is needed. Execution stopped...")
  ##LM events info
  if(!is.na(RLs[[1]][[1]][1])){
    leftLM_Names<-names(table(d1[d1[,RLs[[1]][[6]][[6]]]==RLs[[1]][[1]][1],RLs[[1]][[6]][[4]]]))
    if(length(leftLM_Names)>0){
      RLs[[1]][[7]][[3]]<-leftLM_Names
      RLs[[1]][[1]][[3]]<-ask_info(leftLM_Names,
                                   display=1, mult=1, req1=0,p1=q2, warn1=gw1, lumess=w2a, mess1=paste(m2c, "\n", w2b), stop1=stop1)
    } else {message("No scored left LM events found")}
  }
  if(!is.na(RLs[[1]][[1]][2])){
    rightLM_Names<-names(table(d1[d1[,RLs[[1]][[6]][[6]]]==RLs[[1]][[1]][2],RLs[[1]][[6]][[4]]]))
    if(length(rightLM_Names)>0){
      RLs[[1]][[7]][[4]]<-rightLM_Names
      RLs[[1]][[1]][[4]]<-ask_info(rightLM_Names,
                                   display=1, mult=1, req1=0,p1=q2, warn1=gw1, lumess=w2a, mess1=paste(m2d, "\n", w2b), stop1=stop1)
    } else {message("No scored right LM events found")}
  }
  ##Ask for further LM events
  if(length(RLs[[1]][[7]][[1]])>0){
    a1<-eval_answer2(readline(prompt="Do you want to select additional LM events from the list of all annotations?(y/n or 1/0)   "))
    if(a1==1){
      temp_left<-ask_info(RLs[[1]][[7]][[1]],display=1, mult=1, req1=0, p1=q2, mess1=m2c, warn1=gw1)
      if(all(!is.na(temp_left))|length(temp_left)<1) RLs[[1]][[1]][[3]]<-annotation_add(RLs[[1]][[1]][[3]], temp_left)
      temp_right<-ask_info(RLs[[1]][[7]][[1]],display=0, mult=1, req1=0, p1=q2, mess1=m2d, warn1=gw1)
      if(all(!is.na(temp_right))|length(temp_right)<1) RLs[[1]][[1]][[4]]<-annotation_add(RLs[[1]][[1]][[4]], temp_right)
    }
  }

  ##Check that at least one LM event is present
  if(all(is.na(RLs[[1]][[1]][3:4]))) stop("At least one LM event must be selected. Execution stopped...")

  return(RLs)
}

getinfo_sleep<-function(RLs, d1,...){

  #####Helper function, not foreseen to be called by the user
  #####Asks user specify sleep stage annotations
  #####		required input:
  #####			RLs 		- REMLogic specification file (cannot be empty)
  #####			d1  		- Data table derived from REMLogic event txt file
  #####		optional input:
  #####			none
  #####		output:
  #####			The asked for information will be written to the RL specification file,
  #####			which will be returned.


  w2c<-c("*** [1] 'all annotations' refers to the complete list of annotations\n         given in the header of the REMLogic event file.\n    [2] 'scored annotations' refer to annotations used in this particular \n         scoring session, and found in the events column of the REMLogic event file.")
  m3a<-c("These are your sleep annotations:\n")
  m3b<-c("Which event denotes WAKE?")
  m3c<-c("Which event denotes sleep stage N1?")
  m3d<-c("Which event denotes sleep stage N2?")
  m3e<-c("Which event denotes sleep stage N3?")
  m3f<-c("Which event denotes sleep stage REM?")
  m3g<-c("Do you want to select (additional) sleep stage annotations? (y/n or 1/0)   ")
  m3h<-c("Do you want to select annotations from\n [1] all annotations or\n [2] scored anotations?")
  m3i<-c("\nThese are all your annotations.\n")
  q2<-c("Put in number(s):   ")
  gw1<-c("No selection")


  an1<-eval_answer2(readline(prompt="Do you have sleep scorings? (y/n or 1/0)   "), d=1)
  if(an1==0){
    RLs[[1]][[2]][1]<-0
    return(RLs)
    stop()
  }
  if(an1==1){
    RLs[[1]][[2]][1]<-1
    if(!is.na(RLs[[1]][[6]][7])){
      sleep_Names<-names(table(d1[,RLs[[1]][[6]][[7]]]))
      if(length(sleep_Names)<1){
        sleep_flag<-1
      } else{
        RLs[[1]][[2]][[2]]<-ask_info(sleep_Names, display=1, mult=1, req1=0, p1=q2, warn1=gw1, lumess=m3a, mess1=m3b)
        RLs[[1]][[2]][[3]]<-ask_info(sleep_Names, display=0, mult=1, req1=0, p1=q2, war1n=gw1, lumess=m3a, mess1=m3c)
        RLs[[1]][[2]][[4]]<-ask_info(sleep_Names, display=0, mult=1, req1=0, p1=q2, warn1=gw1, lumess=m3a, mess1=m3d)
        RLs[[1]][[2]][[5]]<-ask_info(sleep_Names, display=0, mult=1, req1=0, p1=q2, warn1=gw1, lumess=m3a, mess1=m3e)
        RLs[[1]][[2]][[6]]<-ask_info(sleep_Names, display=0, mult=1, req1=0, p1=q2, warn1=gw1, lumess=m3a, mess1=m3f)
        sleep_flag<-0
        an2<-eval_answer2(readline(prompt=m3g), d=0)
        if(an2==0) {
          if(all(is.na(RLs[[1]][[2]][2:6]))) RLs[[1]][[2]][1]<-0
          return(RLs)
          stop()
        }

      }
    }

    temp1<-ask_info(RLs[[1]][[7]][[1]], display=1, mult=1, req1=0, p1=q2, warn1=gw1, lumess=m3i, mess1=m3b)
    if(!is.na(temp1)|length(temp1)<1) RLs[[1]][[2]][[2]]<-annotation_add(RLs[[1]][[2]][[2]], temp1)
    temp2<-ask_info(RLs[[1]][[7]][[1]], display=0, mult=1, req1=0, p1=q2, warn1=gw1, lumess=m3a, mess1=m3c)
    if(!is.na(temp2)|length(temp2)<1) RLs[[1]][[2]][[3]]<-annotation_add(RLs[[1]][[2]][[3]], temp2)
    temp3<-ask_info(RLs[[1]][[7]][[1]], display=0, mult=1, req1=0, p1=q2, warn1=gw1, lumess=m3a, mess1=m3d)
    if(!is.na(temp3)|length(temp3)<1) RLs[[1]][[2]][[4]]<-annotation_add(RLs[[1]][[2]][[4]], temp3)
    temp4<-ask_info(RLs[[1]][[7]][[1]], display=0, mult=1, req1=0, p1=q2, warn1=gw1, lumess=m3a, mess1=m3e)
    if(!is.na(temp4)|length(temp4)<1) RLs[[1]][[2]][[5]]<-annotation_add(RLs[[1]][[2]][[5]], temp4)
    temp5<-ask_info(RLs[[1]][[7]][[1]], display=0, mult=1, req1=0, p1=q2, warn1=gw1, lumess=m3a, mess1=m3f)
    if(!is.na(temp5)|length(temp5)<1) RLs[[1]][[2]][[6]]<-annotation_add(RLs[[1]][[2]][[6]], temp5)

  }

  #Check that at least one sleep annotation is present
  if(all(is.na(RLs[[1]][[2]][2:6]))) RLs[[1]][[2]][1]<-0

  return(RLs)
}

getinfo_arousal<-function(RLs, d1,...){

  #####Helper function, not foreseen to be called by the user
  #####Asks user specify arousal event annotations
  #####		required input:
  #####			RLs 		- REMLogic specification file (cannot be empty)
  #####			d1  		- Data table derived from REMLogic event txt file
  #####		optional input:
  #####			none
  #####		output:
  #####			The asked for information will be written to the RL specification file,
  #####			which will be returned.

  w2b<-c("*** If there is more than one, use R notation eg. 1,2,3 or 1:3, or 1,2,5:7")
  w2c<-c("*** [1] 'all annotations' refers to the complete list of annotations\n         given in the header of the REMLogic event file.\n    [2] 'scored annotations' refer to annotations used in this particular \n         scoring session, and found in the events column of the REMLogic event file.")
  m3h<-c("Do you want to select annotations from\n [1] all annotations or\n [2] scored anotations?")
  m4a<-c("Which events are arousal events?   ")
  q2<-c("Put in number(s):   ")
  q1<-c("Put in number:   ")
  gw1<-c("No selection")

  go<-1
  an1<-eval_answer2(readline(prompt="Are there arousal events? (y/n or 1/0)   "), d=1)
  if(an1==0){
    RLs[[1]][[3]][1]<-0
    return(RLs)
    stop()
  }
  if(an1==1){
    RLs[[1]][[3]][1]<-1
    message(w2c)
    message(m3h)
    an2<-readline(prompt=q1)
    if(an2==2){
      RLs[[1]][[3]][[2]]<-ask_info(RLs[[1]][[7]][[2]], display=1, mult=1, req1=0, p1=q2,warn1=gw1,lumess=w2b, mess1=m4a)
      an3<-eval_answer2(readline(prompt="Do you want to select further events from all annotations?(y/n or 1/0)    "), d=0)
    }
    if(an2==1||an3==1){
      ar_temp<-ask_info(RLs[[1]][[7]][[1]], display=1, mult=1, req1=0, p1=q2, warn1=gw1, lumess=w2b, mess1=m4a)
      if(all(!is.na(ar_temp))) RLs[[1]][[3]][[2]]<-annotation_add(ar_temp, RLs[[1]][[3]][[2]])
    }
  }

  #check that there is at least one arousal event
  if(all(is.na(RLs[[1]][[3]][2]))) RLs[[1]][[3]][1]<-0
  return(RLs)
}


getinfo_respiration<-function(RLs, d1,...){

  #####Helper function, not foreseen to be called by the user
  #####Asks user specify arousal event annotations
  #####		required input:
  #####			RLs 		- REMLogic specification file (cannot be empty)
  #####			d1  		- Data table derived from REMLogic event txt file
  #####		optional input:
  #####			none
  #####		output:
  #####			The asked for information will be written to the RL specification file,
  #####			which will be returned.

  w2b<-c("*** If there is more than one, use R notation eg. 1,2,3 or 1:3, or 1,2,5:7")
  w2c<-c("*** [1] 'all annotations' refers to the complete list of annotations\n         given in the header of the REMLogic event file.\n    [2] 'scored annotations' refer to annotations used in this particular \n         scoring session, and found in the events column of the REMLogic event file.")
  m3h<-c("Do you want to select annotations from\n [1] all annotations or\n [2] scored anotations?")
  m4a<-c("Which events are respiratory events?   ")
  q2<-c("Put in number(s):   ")
  q1<-c("Put in number:   ")
  gw1<-c("No selection")

  go<-1
  an1<-eval_answer2(readline(prompt="Are there respiratory events? (y/n or 1/0)   "), d=1)
  if(an1==0){
    RLs[[1]][[4]][1]<-0
    return(RLs)
    stop()
  }
  if(an1==1){
    RLs[[1]][[4]][1]<-1
    message(w2c)
    message(m3h)
    an2<-readline(prompt=q1)
    if(an2==2){
      RLs[[1]][[4]][[2]]<-ask_info(RLs[[1]][[7]][[2]], display=1, mult=1, req1=0, p1=q2,warn1=gw1,lumess=w2b, mess1=m4a)
      an3<-eval_answer2(readline(prompt="Do you want to select further events from all annotations?(y/n or 1/0)    "), d=0)
    }
    if(an2==1||an3==1){
      ar_temp<-ask_info(RLs[[1]][[7]][[1]], display=1, mult=1, req1=0, p1=q2, warn1=gw1, lumess=w2b, mess1=m4a)
      if(all(!is.na(ar_temp))) RLs[[1]][[4]][[2]]<-annotation_add(ar_temp, RLs[[1]][[4]][[2]])
    }
  }
  #check that there is at least one respiratory event

  if(all(is.na(RLs[[1]][[4]][2]))) RLs[[1]][[4]][1]<-0
  return(RLs)
}

getinfo_startstop<-function(RLs, d1,...){

  #####Helper function, not foreseen to be called by the user
  #####Asks user specify arousal event annotations
  #####		required input:
  #####			RLs 		- REMLogic specification file (cannot be empty)
  #####			d1  		- Data table derived from REMLogic event txt file
  #####		optional input:
  #####			none
  #####		output:
  #####			The asked for information will be written to the RL specification file,
  #####			which will be returned.

  w2b<-c("*** If there is more than one, use R notation eg. 1,2,3 or 1:3, or 1,2,5:7")
  w2c<-c("*** [1] 'all annotations' refers to the complete list of annotations\n         given in the header of the REMLogic event file.\n    [2] 'scored annotations' refer to annotations used in this particular \n         scoring session, and found in the events column of the REMLogic event file.")
  m3h<-c("Do you want to select annotations from\n [1] all annotations or\n [2] scored anotations?")
  m4a<-c("Which events denote 'start' / 'Lights off'?   ")
  m4b<-c("Which events denote 'stop' / 'Lights on'?   ")
  q2<-c("Put in number(s):   ")
  q1<-c("Put in number:   ")
  gw1<-c("No selection")

  go<-1
  an1<-eval_answer2(readline(prompt="Are there events denoting start/stop or lights off/lights on? (y/n or 1/0)   "), d=1)
  if(an1==0){
    RLs[[1]][[5]][1]<-0
    return(RLs)
    stop()
  }
  if(an1==1){
    RLs[[1]][[5]][1]<-1
    message(w2c)
    message(m3h)
    an2<-readline(prompt=q1)
    if(an2==2|is.na(an2)|an2==""){
      RLs[[1]][[5]][[2]]<-ask_info(RLs[[1]][[7]][[2]], display=1, mult=1, req1=0, p1=q2,warn1=gw1,lumess=w2b, mess1=m4a)
      RLs[[1]][[5]][[3]]<-ask_info(RLs[[1]][[7]][[2]], display=0, mult=1, req1=0, p1=q2,warn1=gw1,lumess=w2b, mess1=m4b)
      an3<-eval_answer2(readline(prompt="Do you want to select further events from all annotations?(y/n or 1/0)    "), d=0)
    }
    if(an2==1||an3==1){
      ar_temp1<-ask_info(RLs[[1]][[7]][[1]], display=1, mult=1, req1=0, p1=q2, warn1=gw1, lumess=w2b, mess1=m4a)
      if(!is.na(ar_temp1)) RLs[[1]][[5]][[2]]<-annotation_add(ar_temp1, RLs[[1]][[5]][[2]])
      ar_temp2<-ask_info(RLs[[1]][[7]][[1]], display=0, mult=1, req1=0, p1=q2, warn1=gw1, lumess=w2b, mess1=m4b)
      if(!is.na(ar_temp2)) RLs[[1]][[5]][[3]]<-annotation_add(ar_temp2, RLs[[1]][[5]][[3]])
    }
  }

  #check that there is at least one start/stop event
  if(all(is.na(RLs[[1]][[5]][2:3]))) RLs[[1]][[5]][1]<-0
  return(RLs)
}


getinfo_scoring<-function(RLs,...){

  #####Helper function, not foreseen to be called by the user
  #####Asks user specify arousal event annotations
  #####		required input:
  #####			RLs 		- REMLogic specification file (cannot be empty)
  #####		optional input:
  #####			none
  #####		output:
  #####			The asked for information will be written to the RL specification file,
  #####			which will be returned.

  m1a<-("Which set of PLMS rules do you want to chose?")
  m1aa<-c("Currently, only the newest WASM (IRLSSG/EURLSSG) 2016 rules are implemented.\n")
  m2a<-c("You indicated that no respiratory events were scored.
         Please be aware, that this may be a limitation in patients with
         obstructive sleep apnea!\n")
  m3a<-c("There are two different criteria for respiratory event related LM:
         [1] - 2s to +10.25 s (recommended)
         [2] - 0.5 s to +0.5 s" )
  m3aa<-c("Please choose one:   ")

  message(m1a)
  message(m1aa)
  if(RLs[[1]][[4]][1]==0|is.na(RLs[[1]][[4]][1])) {message(m2a);RLs[[2]][3]<-NA;return(RLs); stop()}
  if(RLs[[1]][[4]][1]==1){
    message(m3a)
    RLs[[2]][3]<-as.integer(readline(prompt=m3aa))
  }
  if(is.na(RLs[[2]][3]) & RLs[[1]][[4]][1]==1) RLs[[2]][3]<-1 #default -2/10.25 if nothing chosen and respiration scored

  return(RLs)
}

getinfo_output<-function(RLs,...){

  #####Helper function, not foreseen to be called by the user
  #####Asks user specify arousal event annotations
  #####		required input:
  #####			RLs 		- REMLogic specification file (cannot be empty)
  #####		optional input:
  #####			none
  #####		output:
  #####			The asked for information will be written to the RL specification file,
  #####			which will be returned.

  m1a<-("What kind of output would you like to have? (multiple outputs possible)")
  m3a<-c("\t[1] screen\n\t[2] csv file \n\t[3] pdf ")
  m3aa<-c("Please input number(s):   ")
  op<-c("screen", "csv", "pdf")

  message(m1a)
  message(m3a)
  an1<-eval_answer(readline(prompt=m3aa))
  if(all(is.na(an1))){
    RLs[[2]][[4]]<-op[1]
  }else{RLs[[2]][[4]]<-op[an1]}

  return(RLs)
}

