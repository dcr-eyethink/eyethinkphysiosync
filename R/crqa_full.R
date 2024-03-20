crqa_full <- function(data, analyses=c("HR","EA","HR.EA","HRwin","EAwin"),
                      condcols = c("category","item","stim","condition"),
                     easample=15,eawsize=30,eawstep=10,
                     writecache=T,pinfo=NULL,...){
  #' Runs through a set of standardized rqa metrics, by running crqa_trials() for each
  #'
  #' @param data  data list containing at least ed, labelled emotibit data after compiling and merging functions
  #' @param analyses the set of CRQA analyses that will be run through
  #' @param condcols column names that identify unique trials that will be analysed together with crqa. Null is only one trial per person
  #' @export


  r <- data.table()

  if (writecache & !file.exists("cache")){dir.create("cache")}

  ############# RQA
  # run crqa unidimensional

  # if (usecache){
  #   r <- fread("cache/romni.csv")
  # }else{

    if ("HR" %in% analyses){
    r_hr_n <-   crqa_trials(data=data,radius=.6,delay=2,embed=4,mindiagline=6,normalize=T,
                            condcols = condcols, edvs= list("HR"),pinfo = pinfo,...)
   if(writecache){fwrite(r_hr_n,"cache/rqa_hr.csv")}

    r <- rbind(r,r_hr_n)
    }

    if ("EA" %in% analyses){
    # for item analyses, we are downsampling to 1hz
    r_ea_n <-   crqa_trials(data=data,radius=.5,delay=2,embed=4,mindiagline=6,normalize=T,samplerate = 1,
                            condcols = condcols, edvs= list("EA"),pinfo = pinfo,...)
    if(writecache){fwrite(r_ea_n,"cache/rqa_ea.csv")}
    r <- rbind(r,r_ea_n)
    }

    if ("HR.EA" %in% analyses){
      r_md <- crqa_trials(data=data,radius=.8,normalize=T,delay=2,embed=4,mindiagline=6,
                          condcols = condcols, edvs= list(c("HR","EA")),announce = F,pinfo = pinfo,...)
      if(writecache){ fwrite(r_md,"cache/rqa_hr.ea.csv")}
      r <- rbind(r,r_md)
      }

  if (is.null(data$r)){
  data$r <- r
  }else{
    data$r <- rbind(data$r,r)
  }

  nodet <- r[,.SD[RR==0,.N]/.N,by=.(edv)]
if (dim(nodet)[1]>0){
  cat( "Proportions where we have no RR data\n")
  print(nodet)
}

  norr <-  r[,.SD[is.na(DET),.N]/.N,by=.(edv)]
  if (dim(norr)[1]>0){
    cat( "Proportions where we have no DET data\n")
    print(norr)
  }



  #################### add to trial data

  rd <- dcast(crqa_means(r,average_over = c("pid1", "edv", condcols),
                        rqa_vars = c("RR","DET")),
              formula = paste(paste(c("pid",condcols),collapse = "+"),"~ edv"),
              value.var = c("RR","DET"))

  rd$pid <- as.factor(rd$pid)

  data$td <- pid_merge(data$td,rd,link = c("pid",condcols))

  if(!is.null(pinfo)){
    rdp <- dcast(crqa_means(r,average_over = c("pid1", "edv", condcols,"sgroup"),
                           rqa_vars = c("RR","DET")),
                 formula = paste(paste(c("pid",condcols),collapse = "+"),"~ edv + sgroup"),
                 value.var = c("RR","DET"))
    rdp$pid <- as.factor(rdp$pid)
    data$td <- pid_merge(data$td,rdp,link = c("pid",condcols))
  }


  if(writecache){fwrite(data$td,"cache/trial_data_ps.csv")}


  #################### add to item data

  rdi <- dcast(crqa_means(r,average_over = c(condcols,"edv"),rqa_vars = c("RR","DET")),
               formula = paste(paste(condcols,collapse = "+"),"~ edv"),
               value.var = c("RR","DET"))

  data$sd <- pid_merge(data$sd,rdi,link=condcols)

  if(writecache){fwrite(file = "cache/item_data.csv",x = data$sd)}


  ############################################
  # get windowed rqa for timecourse

  rw <- data.table()

    if ("HRwin" %in% analyses){

    r_hr_win <-   crqa_trials(data=data,radius=.6,delay=1,embed=2,mindiagline=3,normalize=T,
                              condcols = condcols, edvs= list("HR"),
                              windowsize = 6,windowstep = 3,pinfo = pinfo,...)

    rw <- rbind(rw,r_hr_win)

    if(writecache){write.csv(r_hr_win,"cache/rqa_hr_win.csv")}

        }

    if ("EAwin" %in% analyses){
    ## settings for an EA smaple rate of 15hz

    r_ea_win <-   crqa_trials(data=data,radius=.5,delay=2,embed=3,mindiagline=4,normalize=T,
                              condcols = condcols, edvs= list("EA"),samplerate = easample,
                              windowsize = eawsize,windowstep = eawstep,pinfo = pinfo,...)

    r_ea_win[,t:=t/easample]

    rw <- rbind(rw,r_ea_win,fill=T)

    if(writecache){write.csv(r_ea_win,"cache/rqa_ea_win.csv")}


    }

  if(!is.null(rw)){

    if (dim(rw)[1]>0){

    if (is.null(data$rw)){
      data$rw <- rw
    }else{
      data$rw <- rbind(data$rw,rw,fill=T)
    }

    ############ make dm

    dm <- rw[,.(metric="DET",y=mean(DET,na.rm=T)),by=c(condcols,"edv","t")]

    if (is.null(data$dm)){
      data$dm <- dm
    }else{
      data$dm <- rbind(data$dm,dm,fill=T)
    }
}
  }

  return(data)

}
