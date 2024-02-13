physio_merge <- function(processed_emotibit=NULL,processed_gorilla=NULL,
                                 td=NULL,sinfo=NULL,pd=NULL,sd=NULL,
                                 ed=NULL,dinfo=c("pid"),normlevel=1,pinfo=NULL,
                                 gd=NULL, filter_prop_min=0.5, normvar="filter",normvalue=1,
                                 baseline=FALSE,hour_adjust=0,trim_eda=T,stop_at_plotcheck=F,
                                 plot_eda_outliers=F){
  #' Label physio data with trial info
  #' labels the data stream with participant and condition info
  #' based on eid and timestamp
  #' @param ed physio data, post compile
  #' @param td trial data, including eid, cond(s), starttime and stoptime
  #' @param dinfo the cols in td that pick out unique trial. If single rec, just audience ID
  #' @param normlevel how many of those dinfo cols should be used to norm
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
      geom_segment(size=4,alpha=1)+geom_text(size=2,nudge_y=.5)+
      geom_segment(data=sinfo[filter==1],inherit.aes = F,size=1.5,
                   aes(y=eid,yend=eid,
                       x=utc_start,
                       xend=utc_end,
                       colour=as.factor(trecon)))+
      #scale_color_manual(values = c("green3","red"))+
      facet_wrap(day~.,scales = "free_x")+theme_bw()+theme(legend.position = "none")

    # sessiondata <-   (ggplot(sinfo[filter==1],aes(y=eid,yend=eid,x=utc_start,xend=utc_end))+
    #                     geom_segment(data=td[,.(exp_begin=min(starttime),exp_end=max(stoptime),day=day),by=.(pid,eid)],
    #                                  inherit.aes=F,aes(x=exp_begin,xend=exp_end,y=eid,yend=eid),size=4,alpha=.5)+
    #                     geom_segment(colour="red"))+facet_wrap(day~.,scales = "free_x")

    if (stop_at_plotcheck){return(sessiondata)}else{print(sessiondata)}
  }


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

  td_tid <- data.table(data.frame(td)[,c("tid",dinfo,pinfo)])[!duplicated(tid)]
  setkey(td_tid,tid)
  setkey(emptrial,tid)
  emptrial <- td_tid[emptrial]


  ## set tt to trial time
  setnames(emptrial,"V1","raw")
  emptrial[,tt := as.numeric(t-min(t),units="secs"),by=.(tid)]

  ## exclude the filtered data from norm calculation
  #  emptrial[filter==1,norm := ((raw - mean(raw) )/ sd(raw)),by=.(nid,dv)]

  if (baseline==FALSE){

    emptrial[filter==1,norm := ((raw - mean(.SD[get(normvar)==normvalue]$raw,,na.rm=T) )/
                                  sd(.SD[get(normvar)==normvalue]$raw,na.rm=T)),by=.(nid,dv)]
  }else{
    #cat("am using baseline")
    emptrial[filter==1,norm := (raw - mean(.SD[get(normvar)==normvalue]$raw,na.rm=T) ),by=.(nid,dv)]

  }

  emptrial[,filter_prop := (mean(filter)+1)/2,by=.(nid,dv) ]



  ess <- emptrial[filter==1 & filter_prop>filter_prop_min,
                  .(filter_prop=mean(filter_prop,na.rm=T),
                    raw_mean=mean(raw,na.rm=T),raw_sd=sd(raw,na.rm=T),
                    raw_min=min(raw,na.rm=T),raw_max=max(raw,na.rm=T),
                    raw_range=max(raw,na.rm=T)-min(raw,na.rm=T),
                    norm_mean=mean(norm,na.rm=T),norm_sd=sd(norm,na.rm=T),
                    norm_min=min(norm,na.rm=T),norm_max=max(norm,na.rm=T),
                    norm_range=max(norm,na.rm=T)-min(norm,na.rm=T)),
                  ,by=.(tid,dv)]

  ess <- dcast.data.table(ess,tid ~ dv,
                          value.var = colnames(ess)[c(3:13)])
  ess$tid <- factor(ess$tid)
  td$tid<- factor(td$tid)
  setkey(ess,tid)
  setkey(td,tid)
  td <- ess[td]

  setcolorder(td,c(seq(from=dim(ess)[2]+1,to=dim(td)[2]),1:dim(ess)[2]))

  ed <- emptrial


  cat("Trials with missing physio data\n")
  print(td[is.na(raw_mean_EA),.N,by=.(eid,pid)])

  if (trim_eda){

    # flag indivduals from EDA
    pde <- ed[dv=="EA",.(meda=mean(raw,na.rm=T),
                         min=min(raw,na.rm=T),
                         max=max(raw,na.rm=T),
                         range=max(raw,na.rm=T)-min(raw,na.rm=T)),by=.(pid,eid)]
    setkey(pde,meda)
    if (dim( pde[meda>20])[1]>0){
      cat("\nHere are high eda people:\n")
      pde[meda>20]}

    # get rid of individuals higher than 25
    pde[,filter_eda:=ifelse(meda>25,0,1)]
    # get rid of low range
    pde[range<0.01,filter_eda:=0]

    ## merge people filters into data
    pd <- pid_merge(pd,pde,link = c("pid","eid"))
    td <- pid_merge(td,pde,link = c("pid","eid"))
    ed <- pid_merge(ed,pde,link = c("pid","eid"))

    ## exclude individual data points
    ed[dv=="EA" & raw>25, filter:=0]
    ed[dv=="EA" & raw<0.031, filter:=0]
    ed[filter_eda==0,filter:=0]


    if(plot_eda_outliers){
      mypirate( ed[dv=="EA" & filter==1,mean(raw,na.rm=T),
                   by=.(pid,eid)],dv="V1",x_condition = "eid",colour_condition = "pid",legend = F,
                title = "Mean EDAs for each subject, split by sensor, m>20 low range excluded")
      ggsave("eda_sensors_ex.pdf",width = 6,height=2)}


    #################### re-z-score the EDA data now that we've excluded null readings

    #ed <- ed[ filter==1 ]


    ed[filter==1 & dv=="EA",norm:=scale(raw),by=pid]
    ed[filter==0 & dv=="EA",norm:=NA,by=pid]

    #################### fix the EDA norm values in td

    neweda <- ed[dv=="EA",
                 .(norm_mean_EA=mean(norm,na.rm=T),
                   norm_sd_EA=sd(norm,na.rm=T)),by=dinfo]

    td[,grep(grep(colnames(td),pattern = "norm",value=T),pattern = "_EA",value=T):=NULL]

    #td <- pid_merge(td,neweda,link = dinfo)
    td <- mergey(td,neweda,link = dinfo)
  }


  ############################  calc item means

  # item_vars <- setdiff(colnames(td[ , .SD, .SDcols = is.numeric]), c(dinfo ,"eid","exp_begin",
  #                                      "exp_ver","exp_end",  "starttime", "stoptime" ,"start_time","stop_time",
  #                                      "watched", "nid" ,"tid","filter_prop_EA", "filter_prop_HR","meda","min",
  #                                      "max" ,"range","filter_eda"))

  item_vars <- c(grep(x=colnames(td[ , .SD, .SDcols = is.numeric]),pattern = "raw_",value = T),
                 grep(x=colnames(td[ , .SD, .SDcols = is.numeric]),pattern = "norm_",value = T))

  # sd <- pid_merge(sd,td[,lapply(.SD, mean,na.rm=T),.SDcols=item_vars,
  #                       by=setdiff(dinfo,"pid")],link=setdiff(dinfo,"pid"))

  if(!is.null(sd)){
    sd <- mergey(sd,td[,lapply(.SD, mean,na.rm=T),.SDcols=item_vars,
                       by=setdiff(dinfo,c("pid",pinfo))],link=setdiff(dinfo,c("pid",pinfo)))
  }




  return(list(ed=ed,td=td,pd=pd,sd=sd))

}
