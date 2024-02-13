emotibit_parser <- function(datafolder=NULL,reparse=F,
                            parser_location="/Applications/EmotiBitSoftware-macOS/EmotiBitDataParser.app/Contents/MacOS/EmotiBitDataParser"){
  #' Run emotibit data through parser to make files for each sensor
  #'
  #' @param datafolder a folder of raw .csv data and .json files from multiple sessions and sensors, if blank will ask for one with system dialog
  #' @param reparse force a re-parsing of data
  #' @param location of the emotibit parser file on your computure
  #' @export

  if (!file.exists(parser_location)){
    rstudioapi::showDialog(message = "Find the EmotiBitDataParser application",title = "parser location?")
    parser_location <- paste0(rstudioapi::selectFile(caption = "Find the EmotiBitDataParser application"),
                              "/Contents/MacOS/EmotiBitDataParser")
  }

  sessionfolders <-  emotibit_getsessionfolders(datafolder)

for (session in sessionfolders){


  if (all(grepl(list.files(session,pattern=".csv",include.dirs = F),
                pattern = "AX")==FALSE) | reparse==T){
    # we are running the parser cos either hasn't been run before
    # or we want to reparse

    emdata <- paste0(session,"/emotibit_data.csv")

    system(command = paste0(parser_location," '",emdata,"'"))

    # calculate ACC

    d <- emotibit_compile(edv =c("AX","AY","AZ"),logging=F,
                          datafolder  = session)$ed

    d <- dcast(d,formula = utc~dv,value.var = "V1",fun.aggregate = mean)

    d[,ACC:=(AX^2+AY^2+AZ^2)^.5]

    d <- d[, .(EpochTimestamp=utc,TypeTag="ACC",AX,AY,AZ,ACC)]

    fwrite(d,file = paste0(session,"/","emotibit_data_ACC.csv"))

    cat("\t ACC calculated")

    # rename HR

   file.rename(from =  paste0(session,"/emotibit_data_HR.csv"), to =paste0(session,"/emotibit_data_HRem.csv"))

      cat("\n",basename( session)," parsed" )

    }else{ cat("\n", basename( session)," skipped" )}
}
}
