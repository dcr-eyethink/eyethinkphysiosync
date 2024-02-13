emotibit_compile <- function(datafolder=NULL,edv=c("EA","HR","ACC"),hour_adjust=0,
                             minstart="2020-01-01 01:00:01 BST",fix_timestamp_from_file=T,logging=T,
                             ...){
  #' Runs through data folder, collating named emotibit DVs (edv) into one file
  #'
  #' before getting this to work, you need the process_bpm.py in a folder
  #' and you need to create a virtual environment that has python 3
  #' @param datafolder a folder of raw .csv data and .json files from multiple sessions and sensors, if blank will ask for one with system dialog
  #' @param edv All emotibit time series data that you require. These are files listed in the session folders as emotibit_data_XX.csv, where XX is the dv name
  #' @param hour_adjust a correction for time zones / day light savings.
  #' @param logging save emotibit_data_comb.csv to disk and output logs of errors, plots of sessions. If FALSE just returns compiled data
  #' @param minstart if we have data that is time stamped before this point, then we're going to assume that the timestamps are wrong
  #' @param fix_timestamp_from_file in the case that minstart finds a problem, try and reconstruct start times from filenames
  #' @export


  sessionfolders <-  emotibit_getsessionfolders(datafolder)

  import_errors <- data.table()
  combdata <- data.table()

  for (session in sessionfolders){
    cat("\nReading " , basename(session))
    for (dv in edv){
      cat("\t",dv)

      dfile <- paste0(session,"/emotibit_data_",dv,".csv")
      if (!file.exists(dfile)){
        cat(" missing")
        import_errors <- rbind(import_errors,
                               data.table(session=basename(session),
                                          edv=dv,error="no file"))
      }else{
     # we have a data file to read in
         d <-  fread(dfile)

      if (length(colnames(d))==0){
        # we have an error, probably time stamps?
        cat(" missing")
         import_errors <- rbind(import_errors,
                               data.table(session=basename(session),
                                          edv=dv,error="empty file"))
      }else{
       # we have data!
        setnames(d,old=tail(colnames(d),n = 1),new="V1")

        ## we're expecting emotibit data in in col called V1
        ## this is not the case if we've generated the data outside of emotibit
        ## so need to make a new V1 column
        if (dv %in% colnames(d)){d$V1 <- d[[dv]]}
        d$trecon <- 0
        ## the timestamps have different names in different version of data parser...
        ## change all versions to utc

        if("LocalTimestamp" %in% colnames(d)){setnames(d,old="LocalTimestamp",new="utc")}else{
          setnames(d,old="EpochTimestamp",new="utc")
        }

        # do we have a weirdly low timestamp?
        if (d$utc[1]<10000){

          #yes we do, should I fix it?
        if (fix_timestamp_from_file){


            fstart <- substr(basename(session),start=nchar(basename(session))-18,stop=100)
            fs_utc <- as.numeric(as.POSIXct(fstart, format="%Y-%m-%d_%H-%M-%S", tz="GMT"))-3600
            d$trecon <- 1
            d[,utc:=utc+fs_utc]
            cat(" low timestamp, UTC recontructed")
            import_errors <- rbind(import_errors,
                                   data.table(session=basename(session),
                                              edv=dv,error="low ts, reconstruced"))
        }
          cat(" low timestamp, not recontructed")
          import_errors <- rbind(import_errors,
                                 data.table(session=basename(session),
                                            edv=dv,error="low ts"))

        }


        d <- d[,.(eid=substr(basename(session),start = 1,stop = nchar(basename(session))-20),
                  utc,trecon,
                  dv=TypeTag,V1=as.numeric(V1),
                  sid=basename(session))]


        combdata <- rbind(combdata,d)

      }   }
  }  #next dv
  } #next session

  ######### adjust time
  combdata[,utc:=utc+hour_adjust*3600]

  ## make time column
  combdata[,t:=as.POSIXct(utc,  origin='1970-01-01', tz = "Europe/London")]

  combdata[,filter:=1]

  combdata[t<minstart,filter:=0]

  if (dim(import_errors)[1]>0){
  cat("\nImport errors\n")
  print(import_errors)
  }

  if(!logging){
    return(list(ed=combdata))
  }else{
  sinfo <- combdata[,.(utc_start=min(utc),utc_end=max(utc),
                       time_start=min(t),time_end=max(t),
                       n=.N,filter=mean(filter),trecon=mean(trecon) ),by=.(sid,eid)]

  sinfo[,mins:=(utc_end-utc_start)/60]
  sinfo[,duration:=(time_end-time_start)]

  if (dim(sinfo[trecon==1])[1]>0){
    cat("\nThese sessions are logged as pre2020, probably due to time stamp errors. I've reconstructed time from file dates\n")
    print(sinfo[trecon==1])}

  fwrite(combdata,file = paste0(datafolder,"/emotibit_data_comb.csv"))


  p_start <- ggplot(sinfo[filter>0],aes(x=time_start))+geom_histogram()
  p_duration <- ggplot(sinfo[filter>0],aes(x=duration))+geom_histogram(binwidth = 30)

  return(list(ed=combdata,sinfo=sinfo,p_start=p_start,p_duration=p_duration,import_errors=import_errors))
  }


}
