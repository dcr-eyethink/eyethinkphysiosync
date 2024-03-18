emotibit_getsessionfolders <- function(datafolder){
  #' given a datafolder filepath, or named directory in working directory, or nothing
  #' it returns a list of emotibit session folders

  if (is.null(datafolder)){
    if(file.exists("emotibit_data")){datafolder <- "emotibit_data"}else{
      rstudioapi::showDialog(message = "Select the data containing all your emotibit files",title = "datafolder location?")
      datafolder <- rstudioapi::selectDirectory()
    }
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
