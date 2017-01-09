###################
### extract annotation from unstructured RL file
###################
ann_find<-function(d0,...){
  m0a<-("Wrong file format! Please check manual for file format specifications.")
  mstop<-c("Error. Execution will be stopped...")

  h<-which(d0=="")
  if(length(h)<2){message(m0a); stop(mstop)}
  if(length(h)==2){
    annotation_all<-d0[(h[1]+1):(h[2]-1)]
    return(annotation_all)
    stop()
  }
  if(length(h)>2){
    h1<-d0[h+1]
    a<-which(grepl("Events Included", h1))
    if(length(a)==1){
      annotation_all<-d0[(h[a]+1):(h[a+1]-1)]
      return(annotation_all)
      stop()
    }
    h2<-diff(h)
    b<-which(h2==max(h2))[1]
    annotation_all<-d0[(h[b]+1):(h[b+1]-1)]
    return(annotation_all)
  }
}


###################
### Generates empty RLs object
###################
RLspec<-function(...){

#####Helper function, not foreseen to be called by the user
#####Creates an empty REMLogic specification list with full names
#####
#####		abbrevated names are easier for the assignment of new values
#####		full names are hopefully self-explanatory
#####		required input:
#####			none
#####		optional input:
#####			not foreseen
#####
#####		The list structure that is created:
#####			1 - Global
#####				1.1 - Legs
#####					1.1.1 Left leg channel name
#####					1.1.2 Right leg channel names
#####					1.1.3 Left leg LM event names
#####					1.1.4 Right leg LM event names
#####				1.2 - Sleep
#####					1.2.1 Sleep scored (0/1)
#####					1.2.2 Wake annotation name(s)
#####					1.2.3 N1 annotation name(s)
#####					1.2.4 N2 annotation name(s)
#####					1.2.5 N3 annotation name(s)
#####					1.2.6 REM annotation names(s)
#####				1.3. - Arousal
#####					1.3.1 Arousals scored (0/1)
#####					1.3.2 Arousal event names
#####				1.4. - Respiration
#####					1.4.1 Respiratory events scored (0/1)
#####					1.4.2 Respiratory event names
#####				1.5. - Start/Stop events
#####					1.5.1 Start/Stop events present (0/1)
#####					1.5.2 Start/Lights off event name
#####					1.5.3 Stop/Lights on event name
#####				1.6. - REMLogic text file specifications
#####					1.6.1 - Time format
#####					1.6.2 - Number of columns
#####					1.6.3 - Start time column number
#####					1.6.4 - Event column number
#####					1.6.5 - Duration column number
#####					1.6.6 - Channel location column number
#####					1.6.7 - Sleep stage column number (if present)
#####				1.7. - All annotations (as listed in txt file header)
#####				1.8. - Scored annotations (as found in this file)
#####				1.9. - Scored annotations on leg channels
#####			2 - Local
#####				2.1. - REMLogic event txt file name and path
#####				2.2. - PLMS scoring rules (currently only WASM2016)
#####				2.3. - Criteria for CLMr (-2/10.25 vs. -/+0.5)
#####				2.4. - Output choice (one or more from screen, txt, pdf)
#####       2.5. - Start time
#####       2.6. - Stop time

##### 	RLs = REMLogic specifications
RLs<-list(
	Global=list(
		Legs=list	(
				"Left leg channel name"=NA,
				"Right leg channel name"=NA,
				"Left leg LM event names"=NA,
	 			"Right leg LM event names"=NA
				),
		Sleep=list	(
				"Sleep scored"=NA,
				"Wake annotation name(s)"=NA,
				"N1 annotation name(s)"=NA,
				"N2 annotation name(s)"=NA,
				"N3 annotation name(s)"=NA,
				"REM annotation name(s)"=NA
				),
		Arousal=list(
				"Arousals scored"=NA,
				"Arousal event names"=NA
				),
		Respiration=list("Respiratory events scored"=NA,
				"Respiratory event names"=NA
				),
		"Start/Stop events"=list(
				"Start/Stop events present"=NA,
				"Start/Lights off event name"=NA,
				"Stop/Lights on event name"=NA
				),
		"REMLogic text file specifications"=list(
				"Time format"=NA,
				"Number of columns"=NA,
				"Start time column number"=NA,
				"Event column number"=NA,
				"Duration column number"=NA,
				"Channel location column number"=NA,
				"Sleep stage column number (if present)"=NA
				),
		Annotations=list(
				"All annotations"=c(),
				"Scored annotations"=c(),
				"Scored left leg annotations"=c(),
				"Scored right leg annotations"=c()
				)
			),
	Local=list(
				"REMLogic event txt file"=NA,
				"PLMS scoring rules"="WASM20016",
				"Respiratory event related LM"=1,
				"Output option(s)"=NA,
				Start=NA,
				Stop=NA
			)
			)

return(RLs)
}

###################
### RLs - check I
###################
RLspec_test1<-function(RLs,...){

#####Helper function, not foreseen to be called by the user
#####Tests wether a list of REMLogic specifications (RLs) has a minimum of needed information.
#####
#####		It tests:
#####		- if at least 1 leg channel is given
#####		- if at least 1 LM event is given
#####		- if a time format is specified
#####		- if number of columns is specified
#####		- if start time column number is specified
#####		- if event column number is specified
#####		- if duration column number is specified
#####		- if channel location number is specified

	error1<-0
	if(is.na(RLs[[1]][[1]][1]) & is.na(RLs[[1]][[1]][2])) error1<-1
	if(any(is.na(RLs[[1]][[6]][1:6]))) error1<-1
	return(error1)
}

###################
### RLs - check II
###################

RLspec_test2<-function(RLs,...){

#####Helper function, not foreseen to be called by the user
#####Tests wether a list of REMLogic specifications (RLs) has valid information given the filepath to the txt file.
#####
#####		required input:
#####			RLs = REMLogic specification file
#####			fn = file path to REMLogic txt file
#####		optional input:
#####			not foreseen
#####
#####		It tests:
#####		- for problems when reading the txt file as a table given the specifications
#####		- if at least one of the specified leg channels is present
#####		- if at least one of the specified leg events is present
#####		- if the time format is correct
#####
#####		It also checks:
#####		- in case sleep stages were specified, whether these are present
#####		- in case arousals were specified, whether these are present
#####		- if there were respiratory events specified, whether present
#####		- if start/stop events were specified, whether they are present
  gstop<-c("Error. Execution will be stopped...")

	##Basic file read in
	d0<-scan(RLs[[2]][[1]], what="character", sep="\t", quote="", quiet=TRUE, strip.white=FALSE, blank.lines.skip=FALSE)
	h<-which(d0=="")
	if(length(h)<2){message("File cannot be read. "); stop(gstop)}
	filestart<-h[length(h)]+1
	d1<-utils::read.table(RLs[[2]][[1]], skip=h[length(h)]+1+RLs[[1]][[6]][[2]], header=FALSE, sep="\t", stringsAsFactors=FALSE )
		if(dim(d1)[2]!=RLs[[1]][[6]][[2]]) stop("Something went wrong! Please start again!")
		if(dim(d1)[1]<1) stop("This file contains no data! Please start again!")

	##At least one leg channel and LM event
	if(length(which(duplicated(
		c(unlist(RLs[[1]][[1]][1:2]), names(table(d1[,RLs[[1]][[6]][[6]]]))))))<1) stop("No leg channels found")
	if(length(	which(duplicated(
		c(unlist(unique(RLs[[1]][[1]][3:4])), unlist(unique(RLs[[1]][[7]][3:4]))))))<1) stop("No LM events found")

	###Try time format
	test1<-try(as.Date(d1[1,RLs[[1]][[6]][[3]]], format=RLs[[1]][[6]][[1]]))
	if(class(test1)=="try-error"||is.na(test1)) stop("Time format is not correct")

	if(!is.na(RLs[[1]][[2]][1]) & RLs[[1]][[2]][1]==1){ ##sleep stage
		if(length(which(duplicated(c(unlist(unique(RLs[[1]][[2]][2:6])),
			names(table(d1[,RLs[[1]][[6]][[4]]]))))))<1) RLs[[1]][[2]][1]<-NA
	}
	if(!is.na(RLs[[1]][[3]][1]) & RLs[[1]][[3]][[1]]==1){ ##Arousal
		if(length(which(duplicated(c(unique(RLs[[1]][[3]][[2]]),
			names(table(d1[,RLs[[1]][[6]][[4]]]))))))<1) RLs[[1]][[3]][[1]]<-NA
	}
	if(!is.na(RLs[[1]][[4]][1]) & RLs[[1]][[4]][1]==1){ ##Respiratory events
		if(length(which(duplicated(c(unique(RLs[[1]][[4]][[2]]),
			names(table(d1[,RLs[[1]][[6]][[4]]]))))))<1) RLs[[1]][[4]][1]<-NA
	}
	if(!is.na(RLs[[1]][[5]][1]) & RLs[[1]][[5]][1]==1){ ##Start/stop
		if(length(which(duplicated(c(unique(RLs[[1]][[5]][2:3]),
			names(table(d1[,RLs[[1]][[6]][[4]]]))))))<1) RLs[[1]][[5]][1]<-NA
	}
	return(RLs)
}

###################
### add annotations to existing one without duplicates
###################
annotation_add<-function(RLs_c, v1,...){

#####Helper function, not foreseen to be called by the user
#####Adds further annotations to existing ones, remove duplicated, remove NA if at least 1 valid entry
#####		required input:
#####			RLs_s	- REMLogic specification file cell
#####			v1  		- vector of to be added annotations
#####		optional input:
#####			none
#####		output:
#####			complete/updated list of annotations

	v2<-unique(c(RLs_c, v1))
	v2<-v2[!is.na(v2)]
	if(length(v2)<1) v2<-NA
	return(v2)
}


###################
### print RLs in a nicer format on screen
###################
RLs_prettyprint<-function(RLs,...){
#####Helper function, not foreseen to be called by the user
#####Asks user specify arousal event annotations
#####		required input:
#####			RLs 		- REMLogic specification file (cannot be empty)
#####		optional input:
#####			none
#####		output:
#####			Will print the REMLogic specification file in a nice format
	message("THESE ARE YOUR SPECIFICATIONS:\n")

	cat(names(RLs)[1], ":", sep="")

		for(i in 1:(length(RLs[[1]])-1)){
			cat(names(RLs[[1]])[i], ":", "\n",sep="")
			for(j in 1:length(RLs[[1]][[i]])){
					if(length(unlist(RLs[[1]][[i]][[j]]))<2){
						cat("\t",names(RLs[[1]][[i]])[j], ": ", unlist(RLs[[1]][[i]][[j]]), "\n", sep=" ")
					}else{
						cat("\t",names(RLs[[1]][[i]])[j], ": ", "\n", sep=" ")
						for(k in 1:length(unlist(RLs[[1]][[i]][j]))){
							cat("\t\t",unlist(RLs[[1]][[i]][[j]])[k], "\n", sep="")
						}
					}

			}
		}

	cat("\n", names(RLs)[2], ":", "\n", sep="")
		cat("\t", names(RLs[[2]])[1], ": ", unlist(RLs[[2]][1]), "\n", sep="")
		cat("\t", names(RLs[[2]])[2], ": ", unlist(RLs[[2]][2]), "\n", sep="")
		m1<-c("-2.0 s to + 10.25 s (recommended)", "-0.5 s to +0.5 s")
		cat("\t", names(RLs[[2]])[3], ": ", m1[unlist(RLs[[2]][3])], "\n", sep="")
		cat("\t", names(RLs[[2]])[4], ": ", unlist(RLs[[2]][4]), "\n", sep=" ")
}

###################
### Print only RLs headers
###################
RLs_prettyprint2<-function(RLs,...){
#####Helper function, not foreseen to be called by the user
#####Asks user specify arousal event annotations
#####		required input:
#####			RLs 		- REMLogic specification file (cannot be empty)
#####		optional input:
#####			none
#####		output:
#####			Will print the REMLogic specification file headers only, in a nice format
	cat(names(RLs)[1], ":", sep="")
		for(i in 1:(length(RLs[[1]])-1)){
			cat("\t", i, " - ", names(RLs[[1]])[i], "\n",sep="")
		}
	cat("\n", names(RLs)[2], ":", "\n", sep="")
		for(i in 2:(length(RLs[[2]]))){
			cat("\t", (length(RLs[[1]])-1)+i-1, " - ", names(RLs[[2]])[i], "\n",sep="")
		}
}


###################
### Print very basic stats
###################
prestats_print<-function(RLs,d1,...){
#####Helper function, not foreseen to be called by the user
#####At the end of data read in and before PLMscoring will print an overview over available data
#####
#####		required input:
#####			RLs 		- REMLogic specification file (maybe empty)
#####			d1  		- extracted data table from REMLogic txt file
#####		optional input:
#####			none
#####		output:
#####			prints to screen very basic description of data set including:
#####			- start/stop time
#####			- TIB
#####			- No of left/right LMA events found
#####			- TST (optional on sleep stages scored)
#####			- No of arousals (optional on arousals scored)
#####			- No of respiratory events (optional on respiration scored)
	message("BASIC INFO:\n")
	cat("\tRegistration started: ", "\t",as.character(RLs[[2]][[5]]), "\n", sep="")

	cat("\tRegistration stopped: ", "\t",as.character(RLs[[2]][[6]]), "\n", sep="")

	cat("\tTime in bed:", "\t", "\t",format_hour(x<-as.numeric(difftime(RLs[[2]][[6]], RLs[[2]][[5]], units="hours"))),"\n",sep="")
	if(!is.na(RLs[[1]][[2]][[1]]) & RLs[[1]][[2]][[1]]!=0){
		cat("\tTotal sleep time:\t", format_hour(sum(d1$Dur[which(d1$T==1 & d1$T2>0)])/60/60),"\n\n", sep="")
	}else{cat("\tNo sleep scoring available\n")}

	cat("\tNo. left LMA events:\t", length(which(d1$T2==10)), "\n", sep="")

	cat("\tNo. right LMA events:\t", length(which(d1$T2==11)), "\n\n", sep="")

	if(!is.na(RLs[[1]][[3]][[1]]) & RLs[[1]][[3]][[1]]!=0){
		cat("\tNo. arousal events:\t", length(which(d1$T2==30)), "\n", sep="")
	}else{cat("\tNo arousal scoring available\n")}

	if(!is.na(RLs[[1]][[4]][[1]]) & RLs[[1]][[4]][[1]]!=0){
		cat("\tNo. respiratory events:\t", length(which(d1$T2==20)), "\n", sep="")
	}else{cat("\tNo respiration scoring available\n")}

	cat("\nPLMScoRing started...\n\n")

}

###################
### sub-function to format decimal hour for printing
###################
format_hour<-function(x,...){##input is decimal hour
	x<-as.numeric(x)
	h<-floor(x)
	min<-floor((x-floor(x))*60)
	sec<-(x-h-min/60)*60*60
	t<-paste(sprintf("%02d", h), ":",sprintf("%02d", min), ":", sprintf("%06.3f",sec), sep="")
	return(t)
}
