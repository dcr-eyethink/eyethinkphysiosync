emotibit_heartpy <- function(datafolder=NULL,heartpy_reparse=F,winsize = 8,
                              heartpy_location="/Users/dcr/Dropbox/Rwork/python/hp"){
  #' Run heartpy through data to regenerate heart rate
  #'
  #' before getting this to work, you need the process_bpm.py in a folder
  #' and you need to create a virtual environment that has python 3
  #' in that folder, and install heartpy.
  #' This is what I ran:
  #'   ###  python3 -m venv /Users/dcr/Dropbox/Rwork/python/hp
  #'   ###  source venv/bin/activate
  #'   ###  pip install heartpy
  #' @param datafolder a folder of raw .csv data and .json files from multiple sessions and sensors, if blank will ask for one with system dialog
  #' @param heartpy_reparse force a re-parsing of data
  #' @param location of the emotibit parser file on your computer
  #' @param winsize the window for calculating heart rate
  #' @export

  if (!file.exists(heartpy_location)){
  rstudioapi::showDialog(message = "Give me the location of your heartpy folder (see help)",
                         title = "heartpy folder?")
    heartpy_location <- rstudioapi::selectDirectory(caption = "heartpy folder")
}

  sessionfolders <-  emotibit_getsessionfolders(datafolder)

## loop through sessions

  for (session in sessionfolders){

    if (!file.exists(paste0(session,"/","emotibit_data_HR.csv")) | heartpy_reparse==T){
    #  we don't have a heartpy outout, or we are forcing a reparse

      if (Sys.info()["sysname"]=="Windows"){
      cat("You still don't have Mac?\n")

        ############# PC system call

        command_pc <-  paste0("cd ",heartpy_location,
                              " && source venv/bin/activate && python process_bpm.py -i "
                              ,session,"/","emotibit_data_PG.csv",
                              " -w ",winsize)
        cat("This is the system command that I am trying\n", command_pc)

        system2(command =command_pc)

      }else{
        ############# MAC system call

          system(command = paste0("cd ",heartpy_location,
                            " && source venv/bin/activate && python process_bpm.py -i '"
                              ,session,"/","emotibit_data_PG.csv",
                            "' -w ",winsize))
        }

    cat ("\n",basename(session)," heartpy processed")
    }
    else
    { cat ("\n",basename(session)," skipped")}

}

}
