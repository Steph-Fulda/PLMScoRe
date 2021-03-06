% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R-PLMSCORE.R
\name{StartPLMScoRe}
\alias{StartPLMScoRe}
\title{Runs PLMScoRe
PLMScoRe function}
\usage{
StartPLMScoRe(RLs = NULL, fn = NA, silent = 0, ...)
}
\arguments{
\item{RLs}{REMLogic specifications}

\item{fn}{Filename and path of REMLogic txt file}

\item{silent}{option to run the function interactively (default) or automatic}

\item{...}{Additional arguments (currently not specified)}
}
\value{
a PLMScoRe object
}
\description{
Will read in a REMLogic event txt file and return descriptors of
leg movements
}
\details{
If you are using \code{StartPLMScore} for the first time you will want to run it
in the interactive mode with no input arguments (\code{resultXY<-StartPLMScore()}). During
the interactive phase you will be asked to define the input to the actual scoring function.
All this information will be saved in an RLs object (short for REMLogic specifications).
At the end you will have the chance to save this RLs object, so that next time you will be
able to load these specifications without going through the interactive phase again.
In fact, after you have selected a REMLogic event txt file you will be asked whether you want
to load an existing specification file. Even if you do load an existing file, you will still
have the opportunity to change the input (unless you set \code{silent=1}).
If you want to use this function for batch processing of a number of event files that all have the
same specifications, you can run it in silent mode with all inputs provided (see example below).
}
\examples{
#Will run the function interactively

\dontrun{plmresultsXY<-StartPLMScoRe()}

#Will run the function automatically without further user input

\dontrun{plmresultsxy<-StartPLMScore(RLs=RLs, fn="D:\\\\Test\\\\XY-Events.txt", silent=1)}

#Example of batch processing

#You want to make sure that you select only txt files
#This will generate a list of all txt files in that directory

\dontrun{fnlist<-list.files("D:\\\\Test\\\\Scorings\\\\", pattern=".txt", full.names=TRUE)}

#You also need to load the RLs object
#Let's say you called it RLsStudyXY you can load it with:

\dontrun{load("D:\\\\Test\\\\RLsStudyXY.RData")}

#or you use the file menu to select and load it
#In RStudio you can verify in the "Environment" tab that you actually loaded it
#In R type ls() and inspect the outcome

\dontrun{for(i in 1:length(fnlist)) StartPLMScore(RLs=RLs, fn=fnlist[i], silent=1)}
}
