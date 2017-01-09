###################
<<<<<<< HEAD
### Ask multiple input - general function
=======
### general purpose input function with multiple input
>>>>>>> 7e99900f2cf1f2f67a955f110e3751d765ba7156
###################
ask_info<-function(lookup, display=1, mult=0,req1=0,lumess=NA,mess1=NA, p0=NA,p1=NA,warn1=NA,stop1=NA,...){

  #####Helper function, not foreseen to be called by the user
  #####Let's user choose from a list of entries by entering the respective number(s)
  #####		required input:
  #####			lookup = vector of entries from which to choose from (at least one)
  #####		optional input:
  #####			mult: 0 = only a single choice (default)
  #####				1 = single or comma-separated multiple choices
  #####			req:  0 = no input is required, if no/wrong input will return NA (default)
  #####				1 = input required, if no/wrong imput will stop execution
  #####			display:	1 = display the numbered lookup entries (default)
  #####					0 = does not display lookup entries
  #####			Displayed Messages (all optional):
  #####			lumess:	message displayed under the displayed list of entries (default: NA, no message displayed)
  #####			mess1: 	message displayed at the start of function (default: NA, no message displayed)
  #####			p0:		prompt displayed if mult=0 (default: NA, no message displayed)
  #####			p1:		prompt displayed if mult=1 (default: NA, no message displayed)
  #####			warn1:	Message displayed if no/wrong imput (default: NA, no message displayed)
  #####			stop1:	message displayed if execution is stopped (default: NA, no message displayed)
  #####		Output:
  #####			if valid entries: selected entries in lookup vector
  #####			if required and missing/wrong entries: nothing, excecution will be stopped
  #####			if not required and missing/wrong entries: NA
  #####			if both valid and wrong entries are entered, only the valid entries will be returned
  #####
  #####		Examples:
  #####			out<-ask_info(list1)
  #####			out<-ask_info(list1, mess1="Please choose a number")
  #####			out<-ask_info(list1, mult=1, display=0)
  #####			messTry<-"Put in number"; out<-ask_info(list1, mess1=messTry)
  #####
  #####

  temp1<-NA; temp2<-NA			## create tempory variables
  if(display==1){				## display list of numbered entries
    for(i in 1:length(lookup)) cat("\t", i, " - ", lookup[i], "\n",sep="")
    if(!is.na(lumess)) message(lumess)
  }
  if(!is.na(mess1)) message(mess1)				##display message at start
  if(mult==0){							##ask for, record, and transform user input, expect a single entry
    temp1<-as.integer(readline(prompt=p0))
    if(!is.na(temp1)) temp2<-lookup[temp1]
  }
  if(mult==1){							##ask for, record, and transform user inmput
    temp1<-eval_answer(readline(prompt=p1))
    if(all(!is.na(temp1))) temp2<-lookup[temp1]
  }
  if(all(is.na(temp2)) & req1==1) {message(warn1); stop(stop1)}	##case no/wrong required imput: stop execution
  if(all(is.na(temp2)) & req1==0) {message(warn1); return(NA)}	##case no/wrong optional inmput: print warning, return NA
  if(any(!is.na(temp2))) return(temp2[!is.na(temp2)])			##case at least one valid entry: return entries
  options(warn=0)
}

###################
### sub-function to deal with multiple input
###################
eval_answer<-function(x,...){
  er<-try(x1<-eval(parse(text=paste("c(", x, ")", sep=""))), silent=TRUE)
  if(class(er)=="try-error"|is.null(x1)){
    return(NA)
  }else{return(x1)}
}

###################
### subfunction to deal with binary input
###################
eval_answer2<-function(x,d=0,...){
  if(d==0 & (x==0||x=="n"||is.na(x)||x==""||(x!=1 & x!="y"))) {return(0);stop()}
  if(d==0 & (x==1||x=="y")) {return(1);stop()}
  if(d==1 & (x==1||x=="y"||is.na(x)||x==""||(x!=0 & x!="n"))) {return(1); stop()}
  if(d==1 & (x==0||x=="n")) {return(0);stop()}
}

###################
<<<<<<< HEAD
### Ask single number input - general function
=======
### general purpose function for single input
>>>>>>> 7e99900f2cf1f2f67a955f110e3751d765ba7156
###################
ask_info_no<-function(lookup=NA, display=1, mult=0,req1=0,lumess=NA,mess1=NA, p0=NA,warn1=NA,stop1=NA,...){
  #####Helper function, not foreseen to be called by the user
  #####Let's user choose from a list of entries or ask for free input, similar to ask_info but here output is a number.
  #####		required input:
  #####			none
  #####		optional input:
  #####			lookup = vector of entries from which to choose from (at least one)
  #####			mult: 0 = only a single choice (default)
  #####				1 = single or comma-separated multiple choices
  #####			req:  0 = no input is required, if no/wrong input will return NA (default)
  #####				1 = input required, if no/wrong imput will stop execution
  #####			display:	1 = display the numbered lookup entries (default)
  #####					0 = does not display lookup entries
  #####			Displayed Messages (all optional):
  #####			lumess:	message displayed under the displayed list of entries (default: NA, no message displayed)
  #####			mess1: 	message displayed at the start of function (default: NA, no message displayed)
  #####			p0:		prompt displayed if mult=0 (default: NA, no message displayed)
  #####			warn1:	Message displayed if no/wrong imput (default: NA, no message displayed)
  #####			stop1:	message displayed if execution is stopped (default: NA, no message displayed)
  #####		Output:
  #####			if valid entries: number
  #####			if required and missing/wrong entries: nothing, excecution will be stopped
  #####			if not required and missing/wrong entries: NA
  #####			if both valid and wrong entries are entered, only the valid entries will be returned
  #####
  #####		Examples:
  #####			out<-ask_info_no(list1)
  #####			out<-ask_info(mess1="Please choose a number")
  #####			out<-ask_info(list1, mult=1, display=0)
  #####			messTry<-"Put in number"; out<-ask_info(list1, mess1=messTry)
  #####
  #####
  temp1<-NA								## create tempory variables

  if(display==1 & any(!is.na(lookup))){				## display list of numbered entries
    for(i in 1:length(lookup)) cat("\t", i, " - ", lookup[i],"\n", sep="")
    if(!is.na(lumess)) message(lumess)
  }

  if(!is.na(mess1)) message(mess1)				##display message at start
  ##ask for, record, and transform user input, expect a single entry



  temp1<-suppressWarnings(as.integer(readline(prompt=p0)))
  if(!is.na(temp1)) return(temp1)
  if(is.na(temp1) & req1==0) return(NA)
  if(is.na(temp1) & req1==1) {message(warn1); stop(stop1)}	##case no/wrong required imput: stop execution
}

