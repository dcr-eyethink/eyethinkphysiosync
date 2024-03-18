emotibit_session_folders <- function(datafolder=NULL,overwrite=F,
                                     sensor_id="eyethink",destination="emotibit_data"){
  #' Organize emotibit raw data files and .json files, into labelled session folders.
  #' Will default to using the eyethink lab key to translate the emotibit serial IDs into soemthing more friendly
  #' @param datafolder a folder of raw .csv data and .json files from multiple sessions and sensors, if blank will ask for one with system dialog
  #' @param sensor_id key to use: "eyethink", "none" or name of file with headings eid and serialid. If blank or it can't find the file, it will ask for location
  #' @param destination name of folder where sorted files end up. Set to NULL if overwriting
  #' @param overwrite will skip folders that already exist by default, unless set to T
  #' @returns the name of the (new) datafolder
  #' @export

  if (is.null(datafolder)){
    rstudioapi::showDialog(message = "Select the data containing all your emotibit files",title = "datafolder location?")
    datafolder <- rstudioapi::selectDirectory()
    }

 # if  (!is.null(duplicate_to)){
 # dir.create(duplicate_to,showWarnings = F)
 #     file.copy(list.files(datafolder,full.names = T),
 #             to = duplicate_to,recursive = TRUE)
 #     datafolder <- duplicate_to
 # }

  if(is.null(destination)){destination <- datafolder}else{
    dir.create(destination,showWarnings = F)
  }

  ## get the eidkey
  if (sensor_id=="none"){eidkey <- data.table(data.table(serialid="",eid=""))}else{
  if (sensor_id=="eyethink"){eidkey <- eyethinkphysiosync::eyethink_emotibits}else{
   if (file.exists(sensor_id)){eidkey <- data.table(read.csv(sensor_id))}else{
      eidkey <- data.table(read.csv(file.choose()))
    }  }  }

  dfs <- list.files(datafolder,pattern=".csv",include.dirs = F)

  # we have a folder of .csv and .json files
  # make folders for each session, named by key

  # ignore any past data compiles
  dfs <- dfs[!grepl(dfs,pattern = "_comb.csv")]

  # make folders
  for (df in dfs){

    ## is the file big enough?
    if (file.info(paste0(datafolder,"/",df))$size>40000){

      sess <- gsub(df,pattern = ".csv",replacement = "")

      jfile <-  paste0(datafolder,"/",sess,"_info.json")
      csvfile <- paste0(datafolder,"/",sess,".csv")

      if (!file.exists(csvfile)){
        stop(paste("\nI can't find csv file",csvfile,"\n"))
      }

      if (!file.exists(jfile)){
        stop(paste("\nI can't find json file",jfile,"\n"))
      }

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


         newf <- paste0(destination,"/",eid,"_",substr(sess,1,19))

    if(!dir.exists(newf) | overwrite==T){

      dir.create(path = newf,showWarnings = F)

      # move stuff into new folder


         if (destination==datafolder){
           file.rename(from = csvfile,to=paste0(newf,"/emotibit_data.csv"))
           file.rename(from = jfile,to=paste0(newf,"/emotibit_meta.json"))
         }else{
           file.copy(from = csvfile, to=newf)
           file.copy(from = jfile, to=newf)
           file.rename(from = paste0(newf,"/",basename(csvfile)),
                       to=paste0(newf,"/emotibit_data.csv"))
           file.rename(from = paste0(newf,"/",basename(jfile)),
                       to=paste0(newf,"/emotibit_meta.json"))
           }


    }else{cat("\n Folder ",newf," already exists, so I skipped it (set overwrite=T if you want otherwise)\n")}


      # session name
      # sess <- gsub(df,pattern = ".csv",replacement = "")
      # newf <- paste0(destination,"/",sess)
      #
      #
      #
      #
      #
      # dir.create(path = newf,showWarnings = F)
      # for (sdfs in list.files(datafolder,pattern = sess)){
      #   if(file.copy(from = paste0(datafolder,"/",sdfs), to=newf)){
      #     #if we're overwriting get rid of previous file
      #     if (destination==datafolder){file.remove(paste0(datafolder,"/",sdfs))}
      #     }
      # }
      #
      # jfile <-  list.files(newf,pattern = ".json",full.names = T)
      #
      # # does newf have a .json file in it?
      #
      # if (length(jfile)>0){
      #   # rename folder and files if we have .json and key
      #   jd <- data.table(jsonlite::fromJSON(jfile, flatten=TRUE))
      #
      #   if("info.device_id" %in% colnames(jd)){
      #     serial <-  subset(jd,info.name=="EmotiBitData")$info.device_id
      #     }else{
      #     serial <-  subset(jd,info.name=="Temperature0")$info.sensor_serial_number_a
      #     if (serial==0) {
      #       serial <-  subset(jd,info.name=="ElectrodermalActivity")$info.setup.eda_transform_intercept
      #     }  }
      #
      #
      #   ## look up in eidkey, if we have it, otherwise use serial
      #   eid <- as.character(subset(eidkey,serialid==as.character(serial))$eid)
      #   if (length(eid)==0){eid <- serial}
      #
      #   ## rename the folder and files inside
      #   file.rename(paste0(newf,"/",df),paste0(newf,"/emotibit_data.csv"))
      #   file.rename(jfile,paste0(newf,"/emotibit_meta.json"))
      #   file.rename(newf,paste0(destination,"/",eid,"_",substr(sess,1,19)))
      #
      # }



    }else{
      # file is too small
      probd <- paste0(dirname(datafolder),"/problem data" )

      if (!dir.exists( probd  )){dir.create(probd )  }
      file.copy(from = paste0(datafolder,"/",df), to = probd)
      file.copy(from = gsub(x=paste0(datafolder,"/",df),pattern = ".csv",replacement = "_info.json"), to = probd)

    }
    # get next .csv file
  }

return(destination)
}


