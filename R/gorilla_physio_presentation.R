gorilla_physio_presentation <- function(data=NULL,datafolder=NULL,
                                    taskname="AV block multiple stim types",
                                    condcols = c("stim","condition","category","item"),
                                    qlist="demographics",stim_screen="stimuli",
                                    quantkey=NULL,post_task=NULL,post_task_link="item",
                                    ...){

#' Process gorilla data from task 'AV block multiple stim types'
#'
#' @param data a list of imported data from data_collator_gorilla()
#' @param datafolder if no data supplied, looks in the folder with a collection of gorilla downloads. If NULL it will ask for file location
#' @param condcols the names of columns that pick out a unique trial in the data
#' @param stim_screen the critical video stimuli are identified as rows where the Zone.Type is "content_video" and the  Screen.Name in gorilla is in this list
#' @param quantkey a data.table of Response (text that was on a likert button) and r (what number you want to assign)
#' @returns td (trial data) and pd (participant data)
#' @export
#'

  if (is.null(data)){
       data <- data_collator_gorilla(datafolder=datafolder)
  }

  # standardizing some column names
  if ("UTC.Date.and.Time" %in% colnames(data$data_task)){setnames(data$data_task,"UTC.Date.and.Time","UTC.Date")}
  if ("Local.Date.and.Time" %in% colnames(data$data_q)){setnames(data$data_q,"Local.Date.and.Time","Local.Date")}
  if ("Local.Date.and.Time" %in% colnames(data$data_task)){setnames(data$data_task,"Local.Date.and.Time","Local.Date")}

  ## Participant Data - get eids
  pd <- do.call(gorilla_emotibit_eid,resolve.args(data=data,...))


  if ("demographics" %in% unique(data$data_q$Task.Name)){
    pd <- do.call(gorilla_q_parse,resolve.args(data=data,...,qlist = qlist,strip = "quant",pd=pd))
    }

  td <- pd
  tdstartcols <- colnames(td)

  ########################################################
  ## data for the stim presentation

   d <- data$data_task[Task.Name %in% taskname]

  ## play errors

  playerrors <-  d[(Zone.Type=="content_video" | Zone.Type=="content_web_audio") & is.na(Reaction.Time)]

  if (dim(playerrors)[1]>0){
    cat("You have these play errors, which have been removed from data\n")
    playerrors[,missing:=gsub(Response,pattern="No source for the media file ",replacement = "")]
    playerrors[,missing:=gsub(missing,pattern=" could be found! Most likely this is because the file has not been uploaded to your stimuli list or its name has been typed incorrectly in the spreadsheet",replacement = "")]
    print(playerrors[,.(pid,Event.Index,UTC.Date,missing),by=condcols])
    d <- d[!(Zone.Type=="content_video" & is.na(Reaction.Time))]
  }



  # make sure condcols  are factors
  d[,c("pid",condcols):=lapply(.SD, as.factor),.SDcols=c("pid",condcols)]

  d$stimplay <- F
  d[(Zone.Type=="content_video" & Screen.Name %in% stim_screen ),  stimplay:=T]


  ## check that condcols
  ## gets unique stim presentations
  ## so that we can use them to index and collate

  d[stimplay==T,n:=1:.N,by=c("pid",condcols)]

  if (dim(d[n>1])[1]>0){
    cat("Your condcols do not pick out unique stimuli events, check col n in data /n")
    return(d)
  }

  #######################
  ## get start stop times
  #######################

  td <- pid_merge(td, d[(stimplay),
             .(start_time=(Local.Timestamp-rt)/1000,
               stop_time=Local.Timestamp/1000),
             by=c("pid",condcols)])


  td$watched <- td$stop_time-td$start_time

  if ("duration" %in% colnames(td)) {
    td[,exp_stim := watched/duration]
    td[,use := 1]
    td[exp_stim<=0.75,use := 0]
    td[watched>duration, stop_time := (start_time+duration)]
  }

  #######################
  ## get numeric ratings from sliders and likerts
  #######################


  ### the trials that have useful info in them are tagged in qcode column
  d[ Zone.Type=="response_rating_scale_likert" |
          Zone.Type=="response_slider_endValue" ,
        qcode := gsub(paste(c(display,Screen.Name,Zone.Name),collapse = "_"),pattern = " ",replacement = ""),
        by=.(display,Screen.Name,Zone.Name)]

  d[,qcode:=gsub(qcode,pattern = "_pres",replacement = "")]
  d[,qcode:=gsub(qcode,pattern = "_",replacement = ".")]

  if(is.character(data$Response)){
    # we might have some text responses from likert buttons

      quantkey <- rbind(data.table(Response=c("I have seen that trailer/movie before",
                              "I have not seen that trailer/movie before",
                              "I have seen that advert before",
                              "I have not seen that advert before" ,
                              "I have seen that act before",
                              "I have not seen that act before",
                              "I have seen that performance before",
                              "I have not seen that performance before",
                              "I have seen that movie before",
                              "I have not seen that movie before"),
                   r=as.double(c(1,0,1,0,1,0,1,0,1,0))),quantkey)
  setkey(quantkey,Response)
  setkey(d,Response)
  d <- quantkey[d,allow.cartesian=TRUE]
  data[is.na(r) & !is.na(qcode), r := as.numeric(as.character(Response))]

  }else{
      d[,r:=as.numeric(Response)]
  }

  # debugging
  ratingerror <- d[!is.na(qcode),.N,by=c("pid",condcols,"qcode")][N>1]
  if (dim(ratingerror)[1]>0){
    cat("You have non-unique identifieers for your ratings \n")
    return(ratingerror)
  }

  ## get the trial ratings from each trial if we have them, and stitch to td
  dratings <- d[!is.na(r) & !is.na(qcode)]

  if (dim(dratings)[1]>0){
    td <-pid_merge(td, dcast(dratings,
                          formula = formula(paste(paste(c("pid",condcols),collapse = "+"),"~ qcode")),
                          value.var = "r"),
                          link=c("pid",condcols))
    }


  ## get the data for any post presentation tasks, like memory
  md <- data$data_task[Task.Name %in% post_task]
  if(dim(md)[1]>0){

    post_test <- do.call(gorilla_physio_test,resolve.args(data=md,post_task_link,...))

    td <- pid_merge(td,post_test$td,link=c("pid",post_task=post_task,
                                           post_task_link=post_task_link))

    return(list(td=td,pd=pd,ptt=post_test$post_test_trials))

  }else{
    return(list(td=td,pd=pd))
    }

}
