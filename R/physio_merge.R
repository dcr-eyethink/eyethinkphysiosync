physio_merge <- function(processed_emotibit=NULL,processed_gorilla=NULL,
                                 td=NULL,sinfo=NULL,pd=NULL,sd=NULL,
                                 ed=NULL,condcols=c("category","condition","item","stim"),
                                 normlevel=1,pinfo=NULL,
                                 gd=NULL, filter_prop_min=0.5,
                                 baseline=FALSE,hour_adjust=0,stop_at_plotcheck=F,
                                 eda_check=list(mean=20,range_min=0.01,low=0.03,high=25)){
  #' Label physio data with trial info
  #' labels the data stream with participant and condition info
  #' based on eid and timestamp
  #' @param ed physio data, post compile
  #' @param td trial data, including eid, cond(s), starttime and stoptime
  #' @param condcols the cols in td that pick out unique trial. If each person just saw the same single event, set to NULL
  #' @param normlevel defaults to 1 which is zscoring per person. Set to 2+ for each additional column of condcols to use
  #' @param normvar use the period when normvar=normvalue for normalising
  #' @param normvalue the raw value of normvar that you want to include in normalising
  #' @param baseline do you want to z score using sd (F default) or just baseline shift (T)
  #' @param gd timecourse data that you also want labelled, optional wrapper for timecoursejoin_empatica
  #' @param hour_adjust nunmber of hours to shift physio data by, in case of GMT problem
  #' @return labeled ed data and td with summary stats

  #' @export

  if (!is.null(processed_emotibit)){
    ed <- processed_emotibit$ed
    sinfo <- processed_emotibit$sinfo
  }

  if (!is.null(processed_gorilla)){
    td <- copy(processed_gorilla$td)
    pd <- processed_gorilla$pd
    sd <- processed_gorilla$sd
  }

  # we're now matching based on UTC rather than converted posix times
  ed$t <- ed$utc+hour_adjust*1000

  dinfo <- c("pid",condcols)

  if("start_time" %in% colnames(td)){setnames(td,old ="start_time",new="starttime" )}
  if("stop_time" %in% colnames(td)){setnames(td,old ="stop_time",new="stoptime" )}

  if(!"starttime" %in% colnames(td) | !"stoptime" %in% colnames(td)){
    return(cat("\nYou haven't got starttime and stoptime\n\n"))}


  ## plot the session times

  if (!is.null(sinfo)){
    sinfo[,day:=strftime(time_start,format = "%m-%d"),by=time_start]
    td[,day:=strftime(as.POSIXct(starttime,  origin='1970-01-01', tz = "Europe/London"),format="%m-%d")]
    #td[,day:=strftime(exp_begin,format = "%d"),by=pid]
    pdb <- td[,.(exp_begin=min(starttime),exp_end=max(stoptime)),by=.(pid,eid,day)]


    sessiondata <-   ggplot(pdb,
                            aes(x=exp_begin,
                                xend=exp_end,
                                y=eid,yend=eid,label=pid))+
      geom_segment(linewidth=4,alpha=1)+geom_text(size=2,nudge_y=.5)+
      geom_segment(data=sinfo[filter==1],inherit.aes = F,linewidth=1.5,
                   aes(y=eid,yend=eid,
                       x=utc_start,
                       xend=utc_end,
                       colour=as.factor(trecon)))+
      #scale_color_manual(values = c("green3","red"))+
      facet_wrap(day~.,scales = "free_x")+theme_bw()+theme(legend.position = "none")

    if (stop_at_plotcheck){return(sessiondata)}else{print(sessiondata)}
  }


  # makes tid, wich is a code for every unique person+trial
  setkey(ed,t)
  td <- data.frame(td)
  if(length(dinfo)>1){
    td$tid <- apply( td[ , dinfo ] , 1 , paste , collapse = "-" )
  }else{
    td$tid <- td[ , dinfo ]
  }
  if (normlevel==1){
    td$nid <- td[,dinfo[1]]
  }else{  td$nid <- apply( td[ , dinfo[1:normlevel] ] , 1 , paste , collapse = "-" )}

  td <- data.table(td)
  grpn <- uniqueN(td$tid)
  pb <- txtProgressBar(min = 0, max = grpn, style = 3)

  ## this is the key vectorized function matching eids and timestamps
  emptrial <- td[, {setTxtProgressBar(pb, .GRP);
    ed[eid==.SD$eid & utc>=.SD$starttime & utc<=.SD$stoptime]},
    by=.(tid,nid)]
  close(pb)

  ed <- copy( pid_merge(emptrial,td[!duplicated(tid),.SD,.SDcols = c("tid",dinfo,pinfo)],
                         link = "tid") )

  ## set tt to trial time
  setnames(ed,"V1","raw")
  ed[,tt := as.numeric(t-min(t),units="secs"),by=.(tid)]


  ############  check and ID EDA problems

if(!is.null(eda_check) & "EA" %in% unique(ed$dv)){

    # flag individuals from EDA
    pde <- ed[dv=="EA",.(mean=mean(raw,na.rm=T),range=max(raw,na.rm=T)- min(raw,na.mr=T),filter=1),by=.(pid,eid)]

    pde[mean>eda_check$mean | range < eda_check$range_min,filter:=0]

    if (dim( pde[filter==0])[1]>0){
      cat("\nHere are people excluded because of high mean eda or low range:\n")
      pde[filter==0]}

    # filter out individuals in main data set
    ed[dv=="EA" & pid %in% pde[filter==0]$pid,filter:=0]

    # filter out data points
    ed[dv=="EA" & (raw>eda_check$high | raw < eda_check$low) ,filter:=0]



}


  ############  normalise / zscore

  if (baseline==FALSE){

    # emptrial[filter==1,norm := ((raw - mean(.SD[get(normvar)==normvalue]$raw,na.rm=T) )/
    #                               sd(.SD[get(normvar)==normvalue]$raw,na.rm=T)),by=.(nid,dv)]

    ed[filter==1, norm:=scale(raw),by=.(nid,dv)]


  }else{
   stop("I haven't implemented norming by baselines yet")
     #cat("am using baseline")
   # emptrial[filter==1,norm := (raw - mean(.SD[get(normvar)==normvalue]$raw,na.rm=T) ),by=.(nid,dv)]

  }


  ############  calculate trial means


  ess <-  rbind(ed[filter==1 ,
           .(x="raw",mean=mean(raw,na.rm=T),sd=sd(raw,na.rm=T),
             min=min(raw,na.rm=T),max=max(raw,na.rm=T),
           range=max(raw,na.rm=T)-min(raw,na.rm=T)),by=.(tid,dv)],
        ed[filter==1 ,
                 .(x="norm",mean=mean(norm,na.rm=T),sd=sd(norm,na.rm=T),
                   min=min(norm,na.rm=T),max=max(norm,na.rm=T),
                   range=max(norm,na.rm=T)-min(norm,na.rm=T)),by=.(tid,dv)])

  evars <- c("mean","sd","min","max","range")
  td <- pid_merge(td,dcast(ess,tid ~ x+dv,value.var = evars),link = "tid")

  td <- pid_merge(td, dcast(ed[,.(filterprop=mean(filter)),by=.(tid,dv)],
                            tid~dv,value.var = "filterprop",), link = "tid")


  setnames(td,old = unique(ed$dv) ,new=paste0("filter_prop_",unique(ed$dv)))



  ############################  calc item means


  item_vars <- c(grep(x=colnames(td[ , .SD, .SDcols = is.numeric]),pattern = "raw_",value = T),
                 grep(x=colnames(td[ , .SD, .SDcols = is.numeric]),pattern = "norm_",value = T))

  item_means <- td[,lapply(.SD, mean,na.rm=T),.SDcols=item_vars,
                 by=setdiff(dinfo,c("pid",pinfo))]

  if(is.null(sd)){
    sd <- item_means
    }else{
    sd <- pid_merge(sd,item_means,link=setdiff(dinfo,c("pid",pinfo)))
  }


  ############## make eda plot

  if(!is.null(eda_check) & "EA" %in% unique(ed$dv)){

      edaplot <- mypirate( ed[dv=="EA"],dv="raw",x_condition = "eid",colour_condition = "pid",legend = F,
                 title = "Mean EDAs for each subject, split by sensor",dots=F)+
      geom_point(data=td,inherit.aes = F,aes(y=mean_raw_EA,x=eid,colour=pid))+
      geom_hline(aes(yintercept = eda_check$mean))+
      geom_label(data=td[,.(fpp=sprintf("%1.1f%%", 100*mean(filter_prop_EA))),by=.(pid,eid)],inherit.aes = F,
                 aes(x=eid,colour=pid,label=fpp,y=mean(td$mean_raw_EA,na.rm=T)))
   print(edaplot)
  }

  return(list(ed=ed,td=td,pd=pd,sd=sd))

}
