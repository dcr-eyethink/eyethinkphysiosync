gorilla_emotibit_eid <- function(data=NULL,eidtask="Emotibit_ID"){
  #' Get ID from gorilla questionnaire
  #' @param data compiled gorilla data
  #' @return pid, eid, experiment start time and experiment version
  #' @export

  if (is.null(data)){
    data <- data_collator_gorilla()
  }

  if ("UTC.Date.and.Time" %in% colnames(data$data_task)){setnames(data$data_task,"UTC.Date.and.Time","UTC.Date")}
  if ("Local.Date.and.Time" %in% colnames(data$data_q)){setnames(data$data_q,"Local.Date.and.Time","Local.Date")}
  if ("Local.Date.and.Time" %in% colnames(data$data_task)){setnames(data$data_task,"Local.Date.and.Time","Local.Date")}


  if (eidtask %in% unique(data$data_q$Task.Name)){

  eidd <- data$data_q[Task.Name==eidtask & (Question.Key=="eid_colour" | Question.Key=="eid_number"),
                      .(pid,Question.Key,r=as.character(Response))]
  eidd <- dcast(eidd,formula = pid~Question.Key,value.var = "r")

  eidd <- eidd[,.(pid,eid=as.factor(paste0(eid_colour,eid_number)))]

  eidd <- pid_merge(eidd, data$data_q[Task.Name==eidtask & Question.Key=="BEGIN QUESTIONNAIRE",
                                      .(pid,exp_begin=Local.Date,
                                        exp_ver=Experiment.Version)])

  eidd <-pid_merge(eidd, data$data_task[!(is.na(pid)) ,
                                        .(exp_end=max(Local.Date)),by=pid])

  eidd[eid=="I'm not wearing a sensorNo number",eid:=NA]
  eidd$pid <- as.factor(eidd$pid)
  setkey(eidd,pid)

  }
  else{
   cat("\nWe don't have the eid id task named ",eidtask,"\n")
     eidd <- data$data_task[!(is.na(pid)),.(exp_begin=as.POSIXct(min(Local.Date),
                                                                 format='%d/%m/%Y %H:%M:%S',tz = "GMT" ),
                                            exp_end=as.POSIXct(max(Local.Date),
                                                               format='%d/%m/%Y %H:%M:%S',tz = "GMT" )),by=pid]
  }


  return(eidd)
}
