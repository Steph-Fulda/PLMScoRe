######################################
##PLM Scoring Functions
######################################

#####################################
#### 1 LM unilateral - join unilateral LMs with offset-onset < 0.5 s
#####################################

lm_unil<-function(d,...){
  llmj<-0; rlmj<-0
  d$Offset<-NA; d$Offset<-d$Onset+d$Dur
  d<-d[order(d$Onset),]
  for(i in 1:dim(d)[1]){
    if(i<=dim(d)[1]){
      if(d$T2[i]==10 & !is.na(d$T2[i])){
        a<-which(d$T2==10 & d$Onset>=d$Offset[i] & d$Onset< d$Offset[i]+0.5)
        if(length(a)>0) {
          d$Offset[i]<-max(d$Offset[c(a,i)])
          d$Dur[i]<-d$Offset[i]-d$Onset[i]
          d<-d[-a,]
          llmj<-llmj+1
        }
      }
      if(d$T2[i]==11& !is.na(d$T2[i])){
        a<-which(d$T2==11 & d$Onset>=d$Offset[i] & d$Onset< d$Offset[i]+0.5)
        if(length(a)>0){
          d$Offset[i]<-max(d$Offset[c(a,i)])
          d$Dur[i]<-d$Offset[i]-d$Onset[i]
          d<-d[-a,]
          rlmj<-rlmj+1
        }
      }
    }
  }

  return(d)
}

#####################################
#### 2 LM test duration  < 0.5 and remove
#####################################

lm_dur1<-function(d,...){
  llmd<-0; rlmd<-0

  b1<-which(d$Dur< 0.5 & (d$T2==10))
  if(length(b1)>0){
    d<-d[-b1,]
    cat("\tLeft LMA events < 0.5 s (will be removed):  ", length(b1), "\n" ,sep="")
  }
  b2<-which(d$Dur< 0.5 & (d$T2==11))
  if(length(b2)>0){
    d<-d[-b2,]
    cat("\tRight LMA events < 0.5 s (will be removed):  ", length(b2), "\n", sep="")
  }
  return(d)
}

#####################################
#### 2 LM test duration  > 10 monolateral and denote in nonCLM
#####################################

lm_dur2<-function(d,...){
  d$nonCLM<-NA; d$nonCLM[is.element(d$T2, c(10,11))]<-0

  b1<-which(d$Dur > 10 & is.element(d$T2, c(10,11)))
  if(length(b1)>0) {
    d$nonCLM[b1]<-1
    cat("\tMonolateral nonCLM events (> 10 s): ", length(b1), "\n", sep="")
  }

  return(d)
}

#####################################
#### 4 LM join bilateral LM - new rules
#####################################

## change T2 = 12 for bilateral
## new variable NoBil = Number of monolateral LM joined
## change nonCLM = 1 if NoBil>4 or dur > 15 or one or more nonCLM included

lm_bil<-function(d,...){
  d$NoBil<-NA;
  d0<-d[d$T2!=10 & d$T2!=11,]
  d1<-d[d$T2==10|d$T2== 11,]
  d1<-d1[order(d1$Onset),]; d1$NoBil<-0;

  if(dim(d1)[1]>0){

    for(i in 1:dim(d1)[1]){
      if(i<dim(d1)[1]){
        bil<-0
        while(bil==0){
          h<-which(d1$Onset>=d1$Onset[i] & d1$Onset<d1$Offset[i]+0.5)
          if(length(h)>1){
            d1$T2[i]<-12
            d1$Offset[i]<-max(d1$Offset[h])
            d1$NoBil[i]<-d1$NoBil[i]+(length(h)-1)
            d1$nonCLM[i]<-max(d1$nonCLM[h])
            d1$Dur[i]<-d1$Offset[i]-d1$Onset[i]
            d1<-d1[-h[h!=i],]
            bil<-0
          }else{bil<-1}
        }
      }
    }
    d1$NoBil[d1$NoBil>0]<-d1$NoBil[d1$NoBil>0]+1
    d1$nonCLM[d1$NoBil>4|d1$Dur>15]<-1
    cat("\tBilateral LM created: ", length(which(d1$T2==12)), "\t[",
        length(which(d1$T2==12 &d1$nonCLM==0)), " CLM, ", length(which(d1$T2==12 & d1$nonCLM==1)), " nonCLM]\n", sep="")
  }
  d<-rbind(d0, d1); d<-d[order(d$Onset),]
  return(d)
}

#####################################
#### 5 label respiratory LM
#####################################

## new variable rLM (for LM 1 if rLM, for R events 1 if at least 1 CLMr)

rlm<-function(RLs, d,...){
  d$rLM<-NA; d$rLM[is.element(d$T2, c(10:12))]<-0

  if(RLs[[1]][[4]][[1]]==1 & !is.na(RLs[[1]][[4]][[1]]) #R events scored
     & length(which(d$T==3))>0){				#and at least 1 R event
    if(RLs[[2]][[3]]==2 & !is.na(RLs[[2]][[3]])){
      ll<- -0.5
      ul<- 0.5
    }else{
      ll<- -2
      ul<- 10.25
    }
    for(i in 1:dim(d)[1]){
      if(is.element(d$T2[i], c(10:12))){
        h<-which(is.element(d$T2, 20) & ((d$Onset[i]>=d$Offset+ll & d$Onset[i]<=d$Offset+ul)|
                                           (d$Offset[i]>=d$Offset+ll & d$Offset[i]<=d$Offset+ul)|
                                           (d$Onset[i]<d$Offset+ll & d$Offset[i]>d$Offset+ul)))
        if(length(h)>0){
          d$rLM[i]<-1
          if(d$nonCLM[i]==0) d$rLM[h]<-1
        }
      }
    }
  }
  cat("\tNumber of respiratory CLM: ", length(which(d$rLM==1 & d$nonCLM==0)), "\n", sep="")
  return(d)
}

#####################################
#### 5a label respiratory LM edf
#####################################

## new variable rLM (for LM 1 if rLM, for R events 1 if at least 1 CLMr)

rlm_edf<-function(rrules, d,...){
  d$rLM<-NA; d$rLM[is.element(d$T2, c(10:12))]<-0

  if(length(which(d$T==3))>0){ #at least 1 R event

    if(!is.na(rrules) & rrules==2){
      ll<- -0.5
      ul<- 0.5
    }else{
      ll<- -2
      ul<- 10.25
    }

    for(i in 1:dim(d)[1]){
      if(is.element(d$T2[i], c(10:12))){
        h<-which(is.element(d$T2, 20) & ((d$Onset[i]>=d$Offset+ll & d$Onset[i]<=d$Offset+ul)|
                                           (d$Offset[i]>=d$Offset+ll & d$Offset[i]<=d$Offset+ul)|
                                           (d$Onset[i]<d$Offset+ll & d$Offset[i]>d$Offset+ul)))
        if(length(h)>0){
          d$rLM[i]<-1
          if(d$nonCLM[i]==0) d$rLM[h]<-1
        }
      }
    }
  }
  cat("\tNumber of respiratory CLM: ", length(which(d$rLM==1 & d$nonCLM==0)), "\n", sep="")
  return(d)
}

#####################################
#### 6 determine PLM
#####################################

plm<-function(d, RLs,...){
  d$PLM<-NA; d$PLMnr<-NA; 	#1 = without removing rLM, 2 = with rLM removed
  d$PLM_no<-NA; d$PLMnr_no<-NA		#No in PLM series
  d$PLM_Sno<-NA; d$PLMnr_Sno<-NA		#No of PLM series
  d$PLM_l<-NA; d$PLMnr_l<-NA		#Length of PLM series (No PLM)
  d$IMI<-NA; d$IMInr<-NA

  d<-d[order(d$Onset),]
  ####Case 1 without removing rLM

  h1<-which(is.element(d$T2, c(10:12)))

  v<-d$Onset[h1]
  vt<-d$nonCLM[h1]

  Imi<-c(diff(v),NA)
  plmN<-rep(NA, length(Imi))

  n=1
  for(i in 1:(length(Imi)-1)){
    if(vt[i+1]==1) {n<-1; next;next}
    if(vt[i+1]==0){
      if(vt[i]==1){n<-1; next}
      if(Imi[i]>=10 & Imi[i]<=90){plmN[i]<-n; plmN[i+1]<-n+1; n<-n+1}
      if(Imi[i]<10|Imi[i]>90) n<-1
    }
  }
  plm1<-plm_classify(plmN)

  IMI<-c(NA, diff(v)); IMI[which(vt==1)]<-NA; if(is.element(length(vt), which(vt==1))) vt<-vt[-length(vt)];
  IMI[which(vt==1)+1]<-NA; d$IMI[h1]<-IMI



  ####Case 2 with removing rLM

  h2<-which(is.element(d$T2, c(10:12)) & (is.na(d$rLM)|d$rLM==0))

  v2<-d$Onset[h2]
  vt2<-d$nonCLM[h2]

  Imi2<-c(diff(v2),NA)
  plmN2<-rep(NA, length(Imi2))

  n=1
  for(i in 1:(length(Imi2)-1)){
    if(vt2[i+1]==1) {n<-1; next;next}
    if(vt2[i+1]==0){
      if(vt2[i]==1){n<-1; next}
      if(Imi2[i]>=10 & Imi2[i]<=90){plmN2[i]<-n; plmN2[i+1]<-n+1; n<-n+1}
      if(Imi2[i]<10|Imi2[i]>90) n<-1
    }
  }
  plm2<-plm_classify(plmN2)
  IMInr<-c(NA,diff(v2)); IMInr[which(vt2==1)]<-NA; if(is.element(length(vt2), which(vt2==1))) vt2<-vt2[-length(vt2)];
  IMInr[which(vt2==1)+1]<-NA; d$IMInr[h2]<-IMInr

  #####add to d1 data table
  plm1$plm[is.na(plm1$plm)]<-0; plm2$plm[is.na(plm2$plm)]<-0
  d$PLM[h1]<-plm1$plm; d$PLM_no[h1]<-plm1$plmNo; d$PLM_Sno[h1]<-plm1$plmS; d$PLM_l[h1]<-plm1$plmL
  d$PLMnr[h2]<-plm2$plm; d$PLMnr_no[h2]<-plm2$plmNo; d$PLMnr_Sno[h2]<-plm2$plmS; d$PLMnr_l[h2]<-plm2$plmL

  return(d)
}



###################
###6b PLM classification - sub function classify PLM-No vector
###################
##########

plm_classify<-function(v,...){
  # v = vector of plmNo and NA
  # output = vectors of valid plmNo (number in series), plmS (series number),
  # plmL (length of series), plm (1 = plm or NA)

  w1<-which(!is.na(v))
  v1<-v[!is.na(v)]

  v2<-c(diff(v1),NA)
  start<-which(v1==1)
  stop<-c(which(v2<1), length(v1))

  l1<-length(v1)
  plm<-rep(NA,l1); plmNo<-v1; plmS<-rep(NA,l1); plmL<-rep(NA,l1)

  for(i in 1:length(start)) plmS[start[i]:stop[i]]<-i #No of the series
  for(i in 1:length(start)) plmL[start[i]:stop[i]]<-max(v1[start[i]:stop[i]]) #length of series
  for(i in 1:length(start)){
    if(plmL[start[i]]<4){
      plmS[start[i]:stop[i]]<-NA #delete series
      if(i<length(start)) plmS[start[i+1]:length(plmS)]<-plmS[start[i+1]:length(plmS)]-1
      plmNo[start[i]:stop[i]]<-NA
      plmL[start[i]:stop[i]]<-NA
    }
  }
  plm[plmL>=4]<-1

  plm1<-rep(NA, length(v)); plmNo1<-rep(NA, length(v)); plmS1<-rep(NA, length(v)); plmL1<-rep(NA, length(v))
  plm1[w1]<-plm; plmNo1[w1]<-plmNo; plmS1[w1]<-plmS; plmL1[w1]<-plmL

  pr<-list(plm=plm1, plmNo=plmNo1, plmS=plmS1, plmL=plmL1)
  return(pr)
}

###################
###7 add sleep stage and arousal
###################

add_sleep<-function(d,...){
  d$AR<-0; d$Stage<-NA
  d<-d[order(d$Onset),]

  for(i in 1:dim(d)[1]){
    a<-which(!is.na(d$T2) & d$T==1 & d$Onset<=d$Onset[i] & d$Offset>d$Onset[i])
    ifelse(length(a)==1, d$Stage[i]<-d$T2[a], d$Stage[i]<-0)
  }
  h<-which(is.element(d$T2, c(10:12)))
  for(i in 1:length(h)){
    b<-which(d$T2==30 & ((d$Onset>=d$Onset[h[i]]-0.5 & d$Onset<=d$Offset[h[i]]+0.5)|
                           (d$Offset>=d$Onset[h[i]]-0.5 & d$Offset<=d$Offset[h[i]]+0.5)|
                           (d$Onset<d$Onset[h[i]]-0.5 & d$Offset>d$Offset[h[i]]+0.5)))
    if(length(b)>0) d$AR[h[i]]<-1
  }
  return(d)
}
