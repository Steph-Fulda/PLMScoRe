###################
### compute complete statistics
###################
plm_output<-function(d,...){


  stage_n<-c("TIB", "TST", "Wake", "N1", "N2", "N3", "REM", "NREM")

  nonCLM<-c(rep(0,7), rep(1,3))
  rLM<-list(c(0,1), c(1), c(0), c(0,1), c(0,1), c(0), c(0), c(0,1), c(1), c(0))
  plm<-list(c(0,1), c(0,1), c(0,1), c(1), c(1), c(0,1), c(0,1), c(0), c(0), c(0))
  plmnr<-list(c(0,1,NA), c(0,1,NA), c(0,1,NA), c(0,1,NA), c(0,1,NA), c(1), c(1), c(0), c(0), c(0))
  ar<-list(c(0,1), c(0,1), c(0,1), c(0,1), c(1), c(0,1), c(1), c(0,1), c(0,1), c(0,1))
  lmt<-list(c(10:12),c(10), c(11), c(12), c(10:11))
  stage<-list(c(0:4), c(1:4), c(0), c(1), c(2), c(3), c(4), c(1:3))

  rn1<-rep(c("","l_","r_", "b_", "m_"), each=10)
  rn2<-rep(c(rep("CLM",3), rep("PLM", 4), rep("nonCLM",3)),length(rn1))
  rn3<-rep(c("","r", "nr", "", "a", "nr", "nr_a", "", "r", "nr"),length(rn1))
  rn<-c(); for(i in 1:length(rn1)) rn[i]<-paste(rn1[i], rn2[i], rn3[i], sep="")

  #LM number
  lm_number<-matrix(NA, 5*10,8)
  for(h in 1:5){
    for(i in 1:10){
      for(j in 1:8){
        lm_number[i+(h-1)*10,j]<-length(which(d$nonCLM==nonCLM[i] & is.element(d$rLM, rLM[[i]]) & is.element(d$PLM, plm[[i]])
                                              & is.element(d$PLMnr, plmnr[[i]]) & is.element(d$AR, ar[[i]])
                                              & is.element(d$Stage, stage[[j]])
                                              & is.element(d$T2, lmt[[h]])))
      }
    }
  }
  lm_number<-as.data.frame(lm_number)
  names(lm_number)<-stage_n;  lm_number<-cbind(LMtype=rn, Statistic="number", lm_number,stringsAsFactors=FALSE)

  #LM indices
  lm_indices<-lm_number[,3:10]
  stage_h<-c();for(i in 1:length(stage))stage_h[i]<-sum(d$Dur[which(is.element(d$T2, stage[[i]]))])/60
  for(i in 1:dim(lm_indices)[2]) lm_indices[,i]<-lm_indices[,i]/(stage_h[i]/60)
  PI<-round(lm_number[which(lm_number$LMtype=="PLM"),3:10]/lm_number[which(lm_number$LMtype=="CLM"),3:10],2);
  PInr<-round(lm_number[which(lm_number$LMtype=="PLMnr"),3:10]/lm_number[which(lm_number$LMtype=="CLMnr"),3:10],2);
  lm_indices<-rbind(lm_indices[1:7,], PI, PInr,lm_indices[8:50,]);
  lm_indices<-cbind(LMtype=c(rn[1:7],c("PI", "PInr"), rn[8:50]), Statistic="no./hour", lm_indices,stringsAsFactors=FALSE)

  #Respiration, arousal
  respar<-matrix(NA, 5,8)
  for(j in 1:8){
    respar[1,j]<-length(which(d$T2==20 & is.element(d$Stage, stage[[j]])))
    respar[2,j]<-respar[1,j]/(stage_h[j]/60)
    respar[3,j]<-length(which(d$T2==20 & d$rLM==1 & is.element(d$Stage, stage[[j]])))*100/(respar[1,j])
    respar[4,j]<-length(which(d$T2==30 & is.element(d$Stage, stage[[j]])))
    respar[5,j]<-respar[4,j]/(stage_h[j]/60)
  }
  respar<-as.data.frame(cbind(LMtype=c("R events", "R events", "R events", "Arousal", "Arousal"),
                              Statistic=c("number", "no./hour", "% with CLM", "number", "no./hour"),
                              respar), stringsAsFactors=FALSE)
  names(respar)[3:10]<-stage_n

  #LM duration
  lm_dur_mean<-matrix(NA, length(lmt)*10,8)
  for(h in 1:length(lmt)){
    for(i in 1:10){
      for(j in 1:8){
        lm_dur_mean[i+(h-1)*10,j]<-mean(d$Dur[which(d$nonCLM==nonCLM[i] & is.element(d$rLM, rLM[[i]]) & is.element(d$PLM, plm[[i]])
                                                    & is.element(d$PLMnr, plmnr[[i]]) & is.element(d$AR, ar[[i]])
                                                    & is.element(d$Stage, stage[[j]])
                                                    & is.element(d$T2, lmt[[h]]))])
      }
    }
  }
  lm_dur_mean<-as.data.frame(lm_dur_mean,stringsAsFactors=FALSE);names(lm_dur_mean)<-stage_n;
  lm_dur_mean<-cbind(LMtype=rn, Statistic="duration; mean", lm_dur_mean,stringsAsFactors=FALSE)

  lm_dur_sd<-matrix(NA, length(lmt)*10,8)
  for(h in 1:length(lmt)){
    for(i in 1:10){
      for(j in 1:8){
        lm_dur_sd[i+(h-1)*10,j]<-stats::sd(d$Dur[which(d$nonCLM==nonCLM[i] & is.element(d$rLM, rLM[[i]]) & is.element(d$PLM, plm[[i]])
                                                & is.element(d$PLMnr, plmnr[[i]]) & is.element(d$AR, ar[[i]])
                                                & is.element(d$Stage, stage[[j]])
                                                & is.element(d$T2, lmt[[h]]))])
      }
    }
  }
  lm_dur_sd<-as.data.frame(lm_dur_sd,stringsAsFactors=FALSE);names(lm_dur_sd)<-stage_n;
  lm_dur_sd<-cbind(LMtype=rn, Statistic="duration; SD", lm_dur_sd,stringsAsFactors=FALSE)

  lm_dur_min<-matrix(NA, length(lmt)*10,8)
  for(h in 1:length(lmt)){
    for(i in 1:10){
      for(j in 1:8){
        x<-suppressWarnings(min(d$Dur[which(d$nonCLM==nonCLM[i] & is.element(d$rLM, rLM[[i]]) & is.element(d$PLM, plm[[i]])
                                            & is.element(d$PLMnr, plmnr[[i]]) & is.element(d$AR, ar[[i]])
                                            & is.element(d$Stage, stage[[j]])
                                            & is.element(d$T2, lmt[[h]]))], na.rm=TRUE))
        ifelse(is.infinite(x), lm_dur_min[i+(h-1)*10,j]<-NA, lm_dur_min[i+(h-1)*10,j]<-x)
      }
    }
  }
  lm_dur_min<-as.data.frame(lm_dur_min,stringsAsFactors=FALSE); names(lm_dur_min)<-stage_n;
  lm_dur_min<-cbind(LMtype=rn, Statistic="duration; min", lm_dur_min,stringsAsFactors=FALSE)

  lm_dur_max<-matrix(NA, length(lmt)*10,8)
  for(h in 1:length(lmt)){
    for(i in 1:10){
      for(j in 1:8){
        x<-suppressWarnings(max(d$Dur[which(d$nonCLM==nonCLM[i] & is.element(d$rLM, rLM[[i]]) & is.element(d$PLM, plm[[i]])
                                            & is.element(d$PLMnr, plmnr[[i]]) & is.element(d$AR, ar[[i]])
                                            & is.element(d$Stage, stage[[j]])
                                            & is.element(d$T2, lmt[[h]]))], na.rm=TRUE))
        ifelse(is.infinite(x), lm_dur_max[i+(h-1)*10,j]<-NA, lm_dur_max[i+(h-1)*10,j]<-x)
      }
    }
  }
  lm_dur_max<-as.data.frame(lm_dur_max,stringsAsFactors=FALSE);names(lm_dur_max)<-stage_n;
  lm_dur_max<-cbind(LMtype=rn, Statistic="duration; max", lm_dur_max,stringsAsFactors=FALSE)

  #IMI
  imi_no<-matrix(NA, 3*2,8)
  for(j in 1:8){
    imi_no[1,j]<-length(which(d$IMI < 10 & is.element(d$Stage, stage[[j]])))
    imi_no[2,j]<-length(which(d$IMI>=10 & d$IMI<= 90 & is.element(d$Stage, stage[[j]])))
    imi_no[3,j]<-length(which(d$IMI>90 & is.element(d$Stage, stage[[j]])))
    imi_no[4,j]<-length(which(d$IMInr < 10 & is.element(d$Stage, stage[[j]])))
    imi_no[5,j]<-length(which(d$IMInr>=10 & d$IMInr<= 90 & is.element(d$Stage, stage[[j]])))
    imi_no[6,j]<-length(which(d$IMInr>90 & is.element(d$Stage, stage[[j]])))
  }
  imi_mean<-matrix(NA,4,8)
  for(j in 1:8){
    imi_mean[1,j]<-exp(mean(log(d$IMI[which(!is.na(d$IMI) & is.element(d$Stage, stage[[j]]))])))
    imi_mean[2,j]<-exp(stats::sd(log(d$IMI[which(!is.na(d$IMI) & is.element(d$Stage, stage[[j]]))])))
    imi_mean[3,j]<-exp(mean(log(d$IMInr[which(!is.na(d$IMInr) & is.element(d$Stage, stage[[j]]))])))
    imi_mean[4,j]<-exp(stats::sd(log(d$IMInr[which(!is.na(d$IMInr) & is.element(d$Stage, stage[[j]]))])))
  }
  imi<-rbind(imi_no, imi_mean); names(imi)<-stage_n
  imi<-as.data.frame(cbind(LMtype=c(rep("IMI",3), rep("IMInr",3), rep("IMI",2), rep("IMInr",2)),
                           Statistic=c(rep(c("<10s; number", "10-90s; number", ">90s; number"),2),
                                       rep(c("log; mean", "log; SD"),2)),imi), stringsAsFactors = FALSE)
  names(imi)[3:10]<-stage_n

  res1<-rbind(rep(NA,10),lm_indices,imi, respar, lm_number, lm_dur_mean, lm_dur_sd, lm_dur_min, lm_dur_max)

  res1[1,]<-c("Sleep/Wake", "duration", stage_h)
  rownames(res1)<-NULL
  for(i in 3:10) res1[,i]<-as.numeric(as.character(res1[,i]))
  return(res1)

}

###################
### select single values
###################

#' Selects and returns a descriptive value from the LM statistic table
#'
#'
#' @param statt \emph{stat}istic \emph{t}able of LM statistics generated by \code{StartPLMScoRe}
#' @param sel list of selection criteria (see details)
#' @param table returns results as a table with (default), \code{table=0} returns only the values
#' @param pretty rounds numeric output to two decimal points (default)
#' @param ... further arguments (currently not implemented)
#'
#' @details The function \code{StartPLMScoRe()} generates a large table with LM statistics,
#' currently 318 different statistics. \code{pprint} can be used to select and return
#' selected values from this table.
#' The input \code{stats} refers to the LM statistic table. If you have run the command
#' \code{outputXY<-StartPLMScoRe()} this table can be found with \code{outputXY$Stats} or
#' \code{outputXY[[3]]}.
#' With the input \code{sel} you specify your selection criteria, for example \code{sel=
#' c("PLMnr", "no./hour", "TST")} will return the PLMnr index for total sleep time. The
#' selection operates on three data types:
#'
#' \describe{
#'   \item{LM type}{\describe{
#'   \item{Main category LM:}{CLM, nonCLM, PLM}
#'  \item{ Modifier 1:}{l (left), r (right), b (bilateral), m (monolateral).
#'   These modifiers are added before the main category with and underscore separating the two,
#'   eg. r_CLM, b_nonCLM}
#'   \item{Modifier 2:}{r (respiratory event associated), nr (non respiratory event associated), a
#'   (arousal associated).
#'   These modifies are added after the main category, if there are more than two these are
#'   separated by and underscore, eg. PLMnr, PLMa, but CLMnr_a.}
#'   \item{Other categories:}{IMI, IMInr, Arousal, R events, Sleep/wake}}}
#'
#'   \item{Statistic}{\describe{
#'   \item{for LM:}{number, no./hour, duration; mean, duration; SD, duration; min,
#'   duration; max}
#'   \item{for IMI:}{log; mean, log;SD, <10s; number, >90s; number, 10-90s; number}
#'   \item{for Sleep/wake:}{duration}
#'   \item{for Arousal:}{number, no./hour}
#'   \item{for R events:}{number, no./hour, \% with CLM }}}
#'
#'   \item{Sleep/wake stage}{TIB, TST, N1, N2, N3, REM, NREM}
#'   }
#'Include in the \code{sel=c()} input all items you want to retrieve from the LM statistic table.
#'If you do not specify anything for any of the three categories (LM type, Statistic, Sleep/wake)
#'all instances will be selected:
#'
#'sel=c("PLMnr", "no./hour") will return PLMnr indices for TIB, TST, Wake, N1, N2, N3, REM, NREM
#'
#'sel=c("IMI") will return all available statistics for all sleep/wake stages
#'
#'You can also specify multiple selections from a single category:
#'
#'sel=c("CLM", "CLMnr", "number", "N1", "REM") will return the number of CLM and CLMnr during N1 and
#'REM sleep.
#'
#'
#'
#' @return the selected descriptive statistics
#' @export
#'
#' @examples
#' # Return PLMS index from LM statistics table \emphasis{lmstat}
#'\dontrun{pprint(lmstat, sel=c("PLM", "no./hour", "TST"))}
#'
#' # Return PLMS index from LM statistics table \emphasis{lmstat} but do not round the result
#' \dontrun{pprint(lmstat, sel=c("PLM", "no./hour", "TST"), pretty=0)}
#'
#' #Return PLMS and PLMSnr index
#' \dontrun{pprint(lmstat, sel=c("PLM", "PLMnr", "no./hour", "TST"))}
#'
pprint<-function(statt,sel=NA,table=1, pretty=1,...){
  out<-statt
  stats<-names(table(statt$Statistic))
  lmtype<-names(table(statt$LMtype))
  stages<-names(statt)[3:10]

  statss<-c(sel,stats)[which(duplicated(c(sel, stats)))]
  lmtypess<-c(sel,lmtype)[which(duplicated(c(sel, lmtype)))]
  stagess<-c(sel,stages)[which(duplicated(c(sel, stages)))]
  ifelse(length(statss)>0, statss<-statss, statss<-stats)
  ifelse(length(lmtypess)>0, lmtypess<-lmtypess, lmtypess<-lmtype)
  ifelse(length(stagess)>0, stagess<-c("LMtype", "Statistic",stagess), stagess<-c("LMtype", "Statistic",stages))

  out1<-out[is.element(out$LMtype, lmtypess) & is.element(out$Statistic, statss), stagess]

  if(pretty==1) {
    h<-which(out1$LMtype!="Sleep/Wake")
    if(length(h)>0){
        for(i in 3:dim(out1)[2]) out1[h,i]<-round(out1[h,i],2)
    }
  }

  ifelse(table==1, out1<-out1, out1<-out1[,-c(1,2)])



  rownames(out1)<-NULL
  return(out1)
}
###################
### format minutes
###################
format_min<-function(x,...){
  a<-as.numeric(x)
  h<-floor(a/60)
  min<-floor(a-h*60)
  sec<-(a-h*60-min)*60
  t<-paste(sprintf("%02d", h), ":",sprintf("%02d", min), ":", sprintf("%06.3f",sec), sep="")
  return(t)
}
###################
### screen print table function
###################

print_tab<-function(tab,co1=c(8,10),...){
  co<-c(co1, rep(9,8))
  nn<-c(" ", " ", names(tab)[3:10])

  if(is.element("Sleep/Wake",tab$LMtype)){
    ns<-which(is.element(tab$LMtype, "Sleep/Wake"))
    s<-tab[ns,]
    tab<-tab[-ns,]
    s1<-substr(format_min(s[3:10]),1,8)
    sn<-c(" ", "", s1)
    cat(rep("_",sum(co)), "\n", sep="")
    for(i in 1:length(nn)) cat(format(nn[i], width=co[i], justify="right"))
    cat("\n")
    for(i in 1:length(sn)) cat(format(sn[i], width=co[i], justify="right"))
  }else{
    cat(rep("_",sum(co)), "\n", sep="")
    for(i in 1:length(nn)) cat(format(nn[i], width=co[i], justify="right"))
  }

  cat("\n",rep("_",sum(co)), "\n", sep="")
  for(j in 1:dim(tab)[1]){
    for(i in 1:dim(tab)[2]){
      cat(format(tab[j,i], width=co[i], justify="right"))
    }
    cat("\n")
  }
  cat(rep("_",sum(co)), "\n", sep="")
}

###################
### screen print basic table information (index and numbers)
###################

print_core<-function(plm_stats,...){
  pc1<-pprint(plm_stats, c("no./hour", "CLM", "PLM", "PLMa", "PI", "CLMnr", "PLMnr", "PLMnr_a", "PInr", "Sleep/Wake",
                           "IMI", "IMInr", "log; mean", "log; SD", "duration", "R events", "Arousal", "% with CLM"))
  pc2<-pprint(plm_stats, c("number", "CLM", "PLM", "PLMa",  "CLMnr", "PLMnr", "PLMnr_a",  "Sleep/Wake", "IMI", "IMInr",
                           "<10s; number", ">90s; number", "10-90s; number", "duration", "R events", "Arousal"))
  pc11<-pc1[c(1,3,6,7,9,12,13,2,4,5,8,10,11,14,15,16),]
  pc21<-pc2[c(1,11,14,15,5,6,7,10,12,13,2,3,4,8,9),]

  pc11[is.na(pc11)]<-c("-"); pc21[is.na(pc21)]<-c("-")

  print_tab(pc21, co1=c(10,14))
  cat("\n\n")
  print_tab(pc11, co1=c(10,14))

}

###################
### add IMI values
###################
add_imi<-function(d,...){
  imi<-d$IMI[!is.na(d$IMI) & d$Stage>0]
  iminr<-d$IMInr[!is.na(d$IMInr) & d$Stage>0]
  i2<-list(IMInr=iminr, IMI=imi)
  return(i2)
}


###################
### screen print imi distributions
###################

imi_plot<-function(d,...){
  imi1<-d$IMI[!is.na(d$IMI)]
  imis1<-d$Stage[!is.na(d$IMI)]
  iminr1<-d$IMInr[!is.na(d$IMInr)]
  iminrs1<-d$Stage[!is.na(d$IMInr)]

  t1<-table(cut(iminr1[iminrs1>0 & iminr1<=90], breaks=seq(0,90,2)))
  t2<-table(cut(log(iminr1[iminrs1>0 & iminr1<=90]),breaks=seq(0,4.5,0.1)))
  t3<-table(cut(imi1[imis1>0 & imi1<=90], breaks=seq(0,90,2)))
  t4<-table(cut(log(imi1[imis1>0 & imi1<=90]),breaks=seq(0,4.5,0.1)))
  y<-max(20, max(t1), max(t2), max(t3), max(t4)); y1<-y+0.2*y

  ##add max of all 4 vectors to add so that all 4 have the same metric
  graphics::par(mfrow=c(2,2))
  imi_subplot(t1, y1=y1)
  imi_subplot(t2, y1=y1,log=1)
  imi_subplot(t3,y1=y1, nr=0)
  imi_subplot(t4,y1=y1, log=1, nr=0)
}

imi_subplot<-function(v,cw=2,log=0,nr=1,y1=0,...){
  xl<-c(2,seq(10,90,10))
  xll<-c(2,10,20,40,60,90)
  graphics::par(mar=c(2,2,1,1))
  if(y1==0) y1<-max(22, max(v)+0.2*max(v))
  ifelse(nr==1, l1<-"CLMSnr intermovement intervals", l1<-"CLMS intermovement intervals")
  ifelse(nr==1, l2<-"CLMSnr intermovement intervals, log scale", l2<-"CLMS intermovement intervals, log scale")
  if(log==0){
    graphics::plot(c(0,0), xlim=c(0,90), ylim=c(0,y1), type="n", ann=FALSE, axes=FALSE)
    graphics::axis(side=2, at=seq(10,y1,10), pos=0, las=1, mgp=c(0,0.6,0));graphics::rect(0,0,90,y1)
    graphics::title(y=list("Number"), line=1)
    graphics::axis(side=1, at=c(2,seq(10,90,10)), labels=paste(c(2,seq(10,90,10)), "s", sep=""),pos=0, cex.axis=0.8, mgp=c(0,0.5,0))
    graphics::text(88,y1-y1/50, l1, adj=c(1,1))
    for(i in 1:length(v)) graphics::rect(i*cw-cw, 0, i*cw, v[i], col=grDevices::gray(0.7))
  }
  if(log==1){
    graphics::plot(c(0,0), xlim=c(0,4.5), ylim=c(0,y1), type="n", ann=FALSE, axes=FALSE)
    graphics::axis(side=2, at=seq(10,y1,10), pos=0, las=1, mgp=c(0,0.6,0));graphics::rect(0,0,4.5,y1)
    graphics::title(y=list("Number"), line=1)
    graphics::axis(side=1, at=log(xll),labels=paste(xll, "s", sep=""), pos=0, mgp=c(0,0.5,0), cex.axis=0.8)
    graphics::text(4.4,y1-y1/50, l2, adj=c(1,1))
    for(i in 1:length(v)) graphics::rect((i/10)-0.1, 0, i/10, v[i], col=grDevices::gray(0.7))
  }
}

###################
### one page pdf output
###################

lm_pdf<-function(RLs, o1,d1,...){
  pc1<-pprint(o1, c("no./hour", "CLM", "PLM", "PLMa", "PI", "CLMnr", "PLMnr", "PLMnr_a", "PInr", "Sleep/Wake",
                           "IMI", "IMInr", "log; mean", "log; SD", "duration", "R events", "Arousal", "% with CLM"))
  pc11<-pc1[c(1,3,6,7,9,12,13,2,4,5,8,10,11,14,15,16),]

  s<-1; a<-1; r<-1; rr<-RLs[[2]][[3]]
  pc11[is.na(pc11)]<-c("-");
  if(is.na(RLs[[1]][[2]][[1]])) {pc11[,c(4, 6:10)]<-c("-"); s<-0}
  if(is.na(RLs[[1]][[3]][[1]])) {pc11[c(4,10,16),c(3:10)]<-c("-"); a<-0}
  if(is.na(RLs[[1]][[4]][[1]])) {pc11[c(2:7,14:15),c(3:10)]<-c("-"); r<-0}

  s1<-substr(format_min(pc11[1,3:10]),1,8)
  #print_tab(pc11, co1=c(10,9))

  fout<-gsub(".txt","_summary.pdf", RLs[[2]][[1]])

  grDevices::pdf(fout,width=8.267 , height=11.692)

  graphics::par(oma=c(2,2,2,2))

    nf<-graphics::layout(rbind(	c(1),
                    c(2,3),
                    c(4,5),
                    c(6)),
             heights=c(0.8,0.5,0.5, 0.2))


  graphics::par(mar=c(0,7,0,7))
  x1<-92; h<-3; y1<-(dim(pc11)[1]+5)*h
  graphics::plot(c(0,0), xlim=c(0,x1), ylim=c(0,y1), ann=FALSE, axes=FALSE, type="n")
  graphics::segments(-1,y1,x1+1,y1, xpd=TRUE, lwd=2)
  for(i in 1:(dim(pc11)[2]-2)) graphics::text(20+i*8, y1-h, names(pc11)[i+2], font=2,adj=c(0,0), xpd=TRUE)
  for(i in 1:(dim(pc11)[2]-2)) graphics::text(20+i*8, y1-h*2, s1[i], cex=0.8,adj=c(0,0), xpd=TRUE)
  graphics::segments(-1,y1-h*3,x1+1,y1-h*3, xpd=TRUE, lwd=2)

  for(i in 2:dim(pc11)[1]) graphics::text(0,y1-h*(2+i), pc11[i,1], adj=c(0,0), xpd=TRUE, font=2)
  for(i in 2:dim(pc11)[1]) graphics::text(10,y1-h*(2+i), pc11[i,2], adj=c(0,0), xpd=TRUE)
  for(j in 1:(dim(pc11)[1]-2)){
    for(i in 2:dim(pc11)[1]) {
      graphics::text(20+j*8,y1-h*(2+i), pc11[i,j+2], adj=c(0,0), xpd=TRUE)
    }
  }
  graphics::segments(-1,y1-h*(dim(pc11)[1]+2.5),x1+1,y1-h*(dim(pc11)[1]+2.5), xpd=TRUE, lwd=2)

  lab1<-c("CLM: candidate LM, LM: leg movements, PLM: periodic LM, IMI: intermovement interval, PI: periodicity index, R events: respiratory events\n")
  lab2<-c("nr: non respiratory eventa associated, a: arousal associated\n")
  lab3<-c("TIB: time in bed, TST: total sleep time\n")
  ifelse(rr==1, lab4a<-c("-2.0 to 10.25 s"), lab4a<-c("-0.5 to 0.5 s"))
  lab4<-paste(c("PLM scoring rules: WASM 2016, CLMr defintion: "), lab4a, "\n",sep="")
  ifelse(s==0, lab5a<-c("**No sleep scorings available\n"), lab5a<-c(""))
  ifelse(r==0, lab5b<-c("**No respiratory event scorings available\n"), lab5b<-c(""))
  ifelse(a==0, lab5c<-c("**No arousal scorings available"), lab5c<-c(""))
  lab5<-paste(lab5a, lab5b, lab5c, sep="")
  lab<-paste(lab1, lab2, lab3, lab4, lab5)
  graphics::text(0,y1-h*(dim(pc11)[1]+2.8), lab, cex=0.75, adj=c(0,1), xpd=TRUE)

  graphics::par(mar=c(0,0,0,0))
  imi1<-d1$IMI[!is.na(d1$IMI)]
  imis1<-d1$Stage[!is.na(d1$IMI)]
  iminr1<-d1$IMInr[!is.na(d1$IMInr)]
  iminrs1<-d1$Stage[!is.na(d1$IMInr)]

  t1<-table(cut(iminr1[iminrs1>0 & iminr1<=90], breaks=seq(0,90,2)))
  t2<-table(cut(log(iminr1[iminrs1>0 & iminr1<=90]),breaks=seq(0,4.5,0.1)))
  t3<-table(cut(imi1[imis1>0 & imi1<=90], breaks=seq(0,90,2)))
  t4<-table(cut(log(imi1[imis1>0 & imi1<=90]),breaks=seq(0,4.5,0.1)))
  y<-max(20, max(t1), max(t2), max(t3), max(t4)); y1<-y+0.2*y

  imi_subplot(t1, y1=y1)
  imi_subplot(t2, y1=y1,log=1)
  imi_subplot(t3, y1=y1, nr=0)
  imi_subplot(t4,log=1, y1=y1, nr=0)



  grDevices::dev.off()
}
