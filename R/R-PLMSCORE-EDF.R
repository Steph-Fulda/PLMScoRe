#' PLMScoRe function for edf plus annotation files
#'
#' Will read in an edf+ annotation file and return descriptors of
#' leg movements
#'
#' @param fn Filename and path of edf+ annotation file
#' @param rrules Chosen definition or respiratory event related leg movements (1 = -2 to 10.25, 2 = -0.5 to 0.5)
#' @param output chosen output options (screen, csv, pdf)
#' @param ... Additional arguments (currently not specified)
#'
#' @details If you are using \code{StartPLMScore_edf} with no input arguments (\code{resultXY<-StartPLMScore_edf()}),
#' you will be asked to chose the definition of respiratory event related leg movements
#' and your output options. Alternatively (see examples below) you can specify both directly.
#'
#' @return a PLMScoRe object
#' @export
#'
#' @examples
#' #Will run the function interactively
#'
#' \dontrun{plmresultsXY<-StartPLMScoRe_edf()}
#'
#' #Will run the function automatically without further user input
#'
#' \dontrun{plmresultsxy<-StartPLMScore_edf(fn="D:\\Test\\XY-Events.edf", rrules=1, output=c("screen", "csv", "pdf"))}
#'
#' #Example of batch processing
#'
#' #You want to make sure that you select only edf files
#' #This will generate a list of all edf files in that directory
#'
#' \dontrun{fnlist<-list.files("D:\\Test\\Scorings\\", pattern=".edf", full.names=TRUE)}
#'
#' \dontrun{for(i in 1:length(fnlist)) StartPLMScore_edf(fn=fnlist[i], rrules=1, output=c("screen", "csv", "pdf"))}
StartPLMScoRe_edf<-function(fn=NA,rrules=NA,output=NA,...){
  op<-options()
  options(digit.secs=3)

  ###Messages
  m0<-("Please select edf+ annotation file...")
  m0a<-("Wrong file format! Please check manual for file format specifications.")
  mstop<-c("Error. Execution will be stopped...")

  ##### (0) Read in edf+ file

  if(is.na(fn)){
    message(m0)
    fn<-utils::choose.files(caption="Please select edf+ annotation file...", multi=FALSE)
  }

  edf_header<-EDF.import(fn)
  edf1<-EDF.getAnnotation.steph4(fn)

  if(is.na(rrules)) rrules<-getinfo_scoring_edf()
  if(all(is.na(output))) output<-getinfo_output_edf()

  edf_header<-list(edf_header, rrules=rrules, output=output)

  ###Basic transformation, recoding etc.

  d1<-edf_event_recode(edf1) #assigns numeric codes to events and removes all events we are not interested in
  d1<-determine_startstop_edf(d1)


  ###Actual scoring
  d1<-lm_unil(d1) 	## join monolateral LMA events with < 0.5 s offset to onset
  d1<-lm_dur1(d1) 	## remove LMA events < 0.5 s
  d1<-lm_dur2(d1) 	## mark LM > 10 s as nonCLM
  d1<-lm_bil(d1)  	## join bilateral LM
  d1<-rlm_edf(rrules,d1) 	## mark rLM
  d1<-plm(d1)	## determine plm status
  d1<-add_sleep(d1)	## add sleep stage and arousal
  o1<-plm_output(d1) ##
  i2<-add_imi(d1) ## extract IMInr and IMI
  if(is.element("screen", output)) {print_core(o1); imi_plot(d1)}
  if(is.element("csv", output)) {
    fout<-gsub(".edf", "_plm_results.csv", fn)
    utils::write.csv(o1, file=fout, quote=FALSE, na="", row.names=FALSE)
  }
  if(is.element("pdf", output)) lm_pdf_edf(o1, d1, fn, rrules)

  res<-list(edf_header=edf_header, Data=d1, Stats=o1, IMI=i2)

  return(invisible(res))

  options(op)
}
