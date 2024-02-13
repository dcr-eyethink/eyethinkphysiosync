emotibit_process <- function(datafolder=NULL,...){
  #' Organize, process and compiles emotibit raw data files and .json files
  #'
  #' It starts with a folder that contains all the .csv and .json data files from emotibit SD cards,
  #' and ends with the data parsed out and organised into folders, and compiled into a single data.table
  #' You can pass
  #' @param datafolder a folder of raw .csv data and .json files from multiple sessions and sensors, if blank will ask for one with system dialog
  #' @param ... Arguments for emotibit_session_folders, emotibit_parser, emotibit_heartpy, emotibit_compile
  #' @export


   datafolder <- do.call( emotibit_session_folders, resolve.args(datafolder = datafolder,...))

   do.call( emotibit_parser, resolve.args(datafolder = datafolder,...))

   do.call( emotibit_heartpy, resolve.args(datafolder = datafolder,...))

   compiled_emotibit <- do.call( emotibit_compile, resolve.args(datafolder = datafolder,...))

   return(compiled_emotibit)
}
