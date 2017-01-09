#' Runs PLMScoRe
#'
#' Will read in a REMLogic event txt file and return descriptors of
#' leg movements
#'
#' @param RLs REMLogic specifications
#' @param fn Filename and path of REMLogic txt file
#' @param silent option to run the function interactively (default) or automatic
#' @param ... Additional arguments (currently not specified)
#'
#' @details If you are using \code{StartPLMScore} for the first time you will want to run it
#' in the interactive mode with no input arguments (\code{resultXY<-StartPLMScore()}). During
#' the interactive phase you will be asked to define the input to the actual scoring function.
#' All this information will be saved in an RLs object (short for REMLogic specifications).
#' At the end you will have the chance to save this RLs object, so that next time you will be
#' able to load these specifications without going through the interactive phase again.
#' In fact, after you have selected a REMLogic event txt file you will be asked whether you want
#' to load an existing specification file. Even if you do load an existing file, you will still
#' have the opportunity to change the input (unless you set \code{silent=1}).
#' If you want to use this function for batch processing of a number of event files that all have the
#' same specifications, you can run it in silent mode with all inputs provided (see example below).
#'
#' @return a PLMScoRe object
#' @export
#'
#' @examples
#' #Will run the function interactively
#'
#' \dontrun{plmresultsXY<-StartPLMScoRe()}
#'
#' #Will run the function automatically without further user input
#'
#' \dontrun{plmresultsxy<-StartPLMScore(RLs=RLs, fn="D:\\Test\\XY-Events.txt", silent=1)}
#'
#' #Example of batch processing
#'
#' #You want to make sure that you select only txt files
#' #This will generate a list of all txt files in that directory
#'
#' \dontrun{fnlist<-list.files("D:\\Test\\Scorings\\", pattern=".txt", full.names=TRUE)}
#'
#' #You also need to load the RLs object
#' #Let's say you called it RLsStudyXY you can load it with:
#'
#' \dontrun{load("D:\\Test\\RLsStudyXY.RData")}
#'
#' #or you use the file menu to select and load it
#' #In RStudio you can verify in the "Environment" tab that you actually loaded it
#' #In R type ls() and inspect the outcome
#'
#' \dontrun{for(i in 1:length(fnlist)) StartPLMScore(RLs=RLs, fn=fnlist[i], silent=1)}
StartPLMScoRe<-function(RLs=NULL,fn=NA,silent=0,...){

	op<-options()
	options(digit.secs=3)

	###Messages
	  m0<-("Please select REMLogic event file...")
    m0bA1<-("TESTING THE SPECIFICATION FILE....")
    m0bA2<-("Wrong file format! Please check manual for file format specifications.")
	  m0bA3<-("Input specification file has one or more errors and will not be used. ")
    m0bA4<-("Initial check: ok")
    m0bB1<-("Do you want to load an existing REMLogic specification file? (y/n or 1/0)    ")
	  m0bB2<-("Select REMLogic specification file (.RData)...")
	  mstop<-c("Error. Execution will be stopped...")

##### (0) Read in REMLogic event txt file

	if(is.na(fn)){
		message(m0)
		fn<-utils::choose.files(caption="Please select REMLogic event file...", multi=FALSE)
	}
	d0<-scan(fn, what="character", sep="\t", quote="", quiet=TRUE, strip.white=FALSE, blank.lines.skip=FALSE)

##### (0a) Initial file check (needs to have at least 2 empty rows, data table starts after the third)

	h<-which(d0=="")
	annotation_all<-ann_find(d0)
  filestart<-h[length(h)]+1


##### (0b) RL specification file
	  pre<-0
	###(A) if input contains RLs, do preliminary check
	if(!is.null(RLs)){
		message(m0bA1)
		test1<-RLspec_test1(RLs)
		if(test1>0) {message(m0bA2); RLs<-NULL}
		if(test1==0){
			RLs[[2]][[1]]<-fn
			#askRLs<-1; an1<-1##check if needed
			test2<-try(RLspec_test2(RLs, fn))
			if(class(test2)=="try-error") {RLs<-NULL; message(m0bA3)}
			if(class(test2)!="try-error") {
			  message(m0bA4)
			  RLs[[2]][[1]]<-fn
				d1<-utils::read.table(fn, skip=filestart+RLs[[1]][[6]][[2]], header=FALSE, sep="\t", stringsAsFactors=FALSE )
				if(dim(d1)[2]!=RLs[[1]][[6]][[2]]) stop("Something went wrong! Please start again!")
				if(dim(d1)[1]<1) stop("This file contains no data! Please start again!")
				names(d1)[RLs[[1]][[6]][[5]]]<-"Dur"; d1$Dur<-as.numeric(as.character(d1$Dur))
				RLs[[1]][[7]][[1]]<-annotation_all
				pre<-1
			  }
		}
	}

	###(B) if no RLs input, ask to load, if yes, do preliminary check
	if(is.null(RLs)){
		askRLs<-eval_answer2(readline(prompt=m0bB1), d=1)
		if(askRLs==1){
			fn2<-utils::choose.files(caption=m0bB2, multi=FALSE, filters = utils::Filters[c("RData"),])
			if(length(fn2)>0){
				load(fn2)
				test1<-RLspec_test1(RLs)
				if(test1>0) {message(m0bA2); askRLs<-0}
				if(test1==0){
					message(m0bA4)
					RLs[[2]][[1]]<-fn
					test2<-try(RLs<-RLspec_test2(RLs, fn))
					if(class(test2)!="try-error") {
						message(m0bA4); RLs[[2]][[1]]<-fn
						d1<-utils::read.table(fn, skip=filestart+RLs[[1]][[6]][[2]], header=FALSE, sep="\t", stringsAsFactors=FALSE )
						if(dim(d1)[2]!=RLs[[1]][[6]][[2]]) stop("Something went wrong! Please start again!")
						if(dim(d1)[1]<1) stop("This file contains no data! Please start again!")
						RLs[[1]][[7]][[1]]<-annotation_all
						RLs[[1]][[7]][[2]]<-names(table(d1[,RLs[[1]][[6]][[4]]]))#scored annotations
						names(d1)[RLs[[1]][[6]][[5]]]<-"Dur"; d1$Dur<-as.numeric(as.character(d1$Dur))
					}else{askRLs<-0}
				}
			}else{askRLs<-0}
		}
	###(c) if no valid RLs file by now, ask for complete RLs specifications
		if(askRLs==0){
			RLs<-RLspec()
			RLs[[2]][[1]]<-fn
			t1<-try(RLs<-getinfo_txtfile(RLs,filestart))
				if(class(t1)=="try-error") stop(mstop)

			##read in the file with the new specifications (mandatory)
				d1<-utils::read.table(fn, skip=filestart+RLs[[1]][[6]][[2]], header=FALSE, sep="\t", stringsAsFactors=FALSE )
					if(dim(d1)[2]!=RLs[[1]][[6]][[2]]) stop("Something went wrong! Please start again!")
					if(dim(d1)[1]<1) stop("This file contains no data! Please start again!")
				RLs[[1]][[7]][[1]]<-annotation_all
				RLs[[1]][[7]][[2]]<-names(table(d1[,RLs[[1]][[6]][[4]]]))#scored annotations
				names(d1)[RLs[[1]][[6]][[5]]]<-"Dur"; d1$Dur<-as.numeric(as.character(d1$Dur))

			##get leg info (mandatory)
				t2<-try(RLs<-getinfo_legs(RLs, d1))
					if(class(t2)=="try-error"||all(is.na(t2))) stop(mstop)

			##get sleep info (optional)
				RLs<-getinfo_sleep(RLs,d1)

			##get arousal info (optional)
				RLs<-getinfo_arousal(RLs, d1)

			##get respiration info (optional)
				RLs<-getinfo_respiration(RLs, d1)

			##get info on lights off / lights on (optional)
				RLs<-getinfo_startstop(RLs, d1)

			##get info on scoring preferences
				RLs<-getinfo_scoring(RLs)

			##get output options
				RLs<-getinfo_output(RLs)
		}
	}

	##things we have by now: complete RLs, d1, annotations all scored

		###print RLs
	RLs_prettyprint(RLs)
		##ask for change (only if silent = 0)
	if(silent==0){
			m1a<-("Do you want to change any of the specifications?(y/n or 1/0)   ")
			m1b<-("Which do you want to change?   ")
			w2b<-c("*** If there is more than one, please separate with commas (eg. 1,2,3)")
			q2<-c("Put in number(s):   ")

		an1<-eval_answer2(readline(prompt=m1a), d=0)
		while(an1==1) {
			RLs_prettyprint2(RLs)
			message(m1b); message(w2b)
			an4<-eval_answer(readline(prompt=q2))
			if(all(is.na(an4))) {an1==0; break}
			if(any(!is.na(an4))){
				for(i in 1:length(an4)){
					if(is.element(1, an4)){
						t2<-try(RLs<-getinfo_legs(RLs, d1, annotation_all))
						if(class(t2)=="try-error"|is.na(t2)) stop(mstop)
					}
				  annotation_scored<-RLs[[1]][[7]][[2]]
					if(is.element(2, an4))RLs<-getinfo_sleep(RLs,d1, annotation_all)
					if(is.element(3, an4))RLs<-getinfo_arousal(RLs, d1, annotation_scored, annotation_all)
					if(is.element(4, an4))RLs<-getinfo_respiration(RLs, d1, annotation_scored, annotation_all)
					if(is.element(5, an4))RLs<-getinfo_startstop(RLs, d1,annotation_scored, annotation_all)
					if(is.element(6, an4))RLs<-getinfo_txtfile(RLs, filestart)
					if(any(is.element(c(7,8), an4)))RLs<-getinfo_scoring(RLs)
					if(is.element(9, an4))RLs<-getinfo_output(RLs)
				}
			}
			RLs_prettyprint(RLs)
			message("THESE ARE YOUR UPDATED SPECIFICATIONS")
			an5<-eval_answer2(readline(prompt="Do you want to change anything?(y/n or 1/0)   "), d=0)
			if(an5==0) an1<-0
		}

	#RLs_prettyprint(RLs)

	####Run cursory check again on RLs file
		test1<-RLspec_test1(RLs)
		if(test1>0) {message("Something went wrong!"); stop()}
		if(test1==0){
			test2<-try(RLs<-RLspec_test2(RLs))
			if(class(test2)!="try-error") {
				message("Everything seems to be ok")
				RLs_ok<-1
				}else{message("Something went wrong!"); stop()}
		}
	}

	####Save specification file
	if(silent==0){
		an6<-eval_answer2(readline(prompt="Do you want to save the specification file?(y/n or 1/0)   "), d=0)
		if(an6==1) {
			fn3<-utils::choose.files(caption="Save REMLogic specification file as...", multi=FALSE, filters = utils::Filters[c("RData"),])
			save(RLs, file=fn3)
		}
	message("THANK YOU, that seems to be all.\n")
	message("Running final check...\n")
	}

###Basic transformation, recoding etc.

	d1<-RL_event_recode(RLs, d1) #assigns numeric codes to events and removes all events we are not interested in
	d1<-format_time(RLs, d1)
	dtemp<-determine_startstop(RLs, d1); d1<-dtemp[[1]]; RLs<-dtemp[[2]]

###Very basic statistics output
	prestats_print(RLs, d1)

###Actual scoring
	d1<-lm_unil(d1) 	## join monolateral LMA events with < 0.5 s offset to onset
	d1<-lm_dur1(d1) 	## remove LMA events < 0.5 s
	d1<-lm_dur2(d1) 	## mark LM > 10 s as nonCLM
	d1<-lm_bil(d1)  	## join bilateral LM
	d1<-rlm(RLs,d1) 	## mark rLM
	d1<-plm(d1)	## determine plm status
	d1<-add_sleep(d1)	## add sleep stage and arousal
	o1<-plm_output(d1) ##
	i2<-add_imi(d1) ## extract IMInr and IMI
	if(is.element("screen", RLs[[2]][[4]])) {print_core(o1); imi_plot(d1)}
	if(is.element("csv", RLs[[2]][[4]])) {
	  fout<-gsub(".txt", "_plm_results.csv", RLs[[2]][[1]])
	  utils::write.csv(o1, file=fout, quote=FALSE, na="", row.names=FALSE)
	}
	if(is.element("pdf", RLs[[2]][[4]])) lm_pdf(RLs, o1, d1)

	res<-list(RLs=RLs, Data=d1, Stats=o1, IMI=i2)

	return(invisible(res))

	options(op)
}
