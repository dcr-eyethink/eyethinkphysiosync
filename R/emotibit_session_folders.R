emotibit_session_folders <- function(datafolder=NULL,
                                     sensor_id="eyethink",duplicate_to="emotibit_data"){
  #' Organize emotibit raw data files and .json files, into labelled session folders.
  #' Will default to using the eyethink lab key to translate the emotibit serial IDs into soemthing more friendly
  #' @param datafolder a folder of raw .csv data and .json files from multiple sessions and sensors, if blank will ask for one with system dialog
  #' @param sensor_id key to use: "eyethink", "none" or name of file with headings eid and serialid. If blank or it can't find the file, it will ask for location
  #' @param duplicate_first before processing, copy all the files to this new folder in working. Set to NULL if overwriting
  #' @returns the name of the (new) datafolder
  #' @export

  if (is.null(datafolder)){
    rstudioapi::showDialog(message = "Select the data containing all your emotibit files",title = "datafolder location?")
    datafolder <- rstudioapi::selectDirectory()
    }

 if  (!is.null(duplicate_to)){
 dir.create(duplicate_to,showWarnings = F)
     file.copy(list.files(datafolder,full.names = T),
             to = duplicate_to,recursive = TRUE)
     datafolder <- duplicate_to
 }

  dfs <- list.files(datafolder,pattern=".csv",include.dirs = F)

  # we have a folder of .csv and .json files
  # make folders for each session, named by key

  if (sensor_id=="none"){eidkey <- data.table(serialid="",eid="")}else{
  if (sensor_id=="eyethink"){eidkey <- eyethinkphysiosync::eyethink_emotibits}else{
   if (file.exists(sensor_id)){eidkey <- data.table(read.csv(sensor_id))}else{
      eidkey <- data.table(read.csv(file.choose()))
    }  }  }


  # ignore any past data compiles
  dfs <- dfs[!grepl(dfs,pattern = "_comb.csv")]

  # make folders
  for (df in dfs){

    ## is the file big enough?
    if (file.info(paste0(datafolder,"/",df))$size>40000){


      # session name
      sess <- gsub(df,pattern = ".csv",replacement = "")
      newf <- paste0(datafolder,"/",sess)
      dir.create(path = newf,showWarnings = F)
      for (sdfs in list.files(datafolder,pattern = sess)){
        if(file.copy(paste0(datafolder,"/",sdfs), newf)){file.remove(paste0(datafolder,"/",sdfs))}
      }

      jfile <-  list.files(newf,pattern = ".json",full.names = T)

      # does newf have a .json file in it?

      if (length(jfile)>0){
        # rename folder and files if we have .json and key
        jd <- data.table(jsonlite::fromJSON(jfile, flatten=TRUE))

        if("info.device_id" %in% colnames(jd)){
          serial <-  subset(jd,info.name=="EmotiBitData")$info.device_id
          }else{
          serial <-  subset(jd,info.name=="Temperature0")$info.sensor_serial_number_a
          if (serial==0) {
            serial <-  subset(jd,info.name=="ElectrodermalActivity")$info.setup.eda_transform_intercept
          }  }


        ## look up in eidkey, if we have it, otherwise use serial
        eid <- as.character(subset(eidkey,serialid==as.character(serial))$eid)
        if (length(eid)==0){eid <- serial}

        ## rename the folder and files inside
        file.rename(paste0(newf,"/",df),paste0(newf,"/emotibit_data.csv"))
        file.rename(jfile,paste0(newf,"/emotibit_meta.json"))
        file.rename(newf,paste0(datafolder,"/",eid,"_",substr(sess,1,19)))

      }
    }else{
      # file is too small
      probd <- paste0(dirname(dirname(df)),"/problem data" )
      if (!dir.exists( probd  )){dir.create(probd )  }
      file.copy(from = df, to = probd)
      file.copy(from = gsub(x=df,pattern = ".csv",replacement = ".json"), to = probd)

    }
    # get next .csv file
  }

return(datafolder)
}


emotibit_getsessionfolders <- function(datafolder){
  #' given a datafolder filepath, or named directory in working directory, or nothing
  #' it returns a list of emotibit session folders

  if (is.null(datafolder)){
  rstudioapi::showDialog(message = "Select the data containing all your emotibit files",title = "datafolder location?")
  datafolder <- rstudioapi::selectDirectory()
}else{
  if (!file.exists(datafolder)){stop(paste0("I can't find datafolder ",datafolder))}

  # convert datafolder to full path of working directory
  if ( dirname(datafolder)=="."){ datafolder <- paste0(getwd(),"/",datafolder)}
}

if (!file.exists(datafolder)){stop(paste0("I can't find datafolder ",datafolder))}

if (length(list.dirs(datafolder,recursive = F))==0){
  # there are no folders, so this is a single session of parsed data
  sessionfolders <- datafolder
}else{
  sessionfolders <- list.dirs(datafolder,full.names = T,recursive=F)
}
  return(sessionfolders)
}
