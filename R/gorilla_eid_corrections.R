gorilla_eid_corrections <-  function(processed_gorilla,corrections){
  #' corrects eid from list of pid throughout data packet
  #' @export
  corrections[,pid:=as.factor(pid)]
  corrections[,eid:=as.factor(eid)]
  for (i in 1:length(processed_gorilla)){
    if("pid" %in% colnames(processed_gorilla[[i]]) & "eid" %in% colnames(processed_gorilla[[i]])){
      processed_gorilla[[i]]$pid <- as.factor(processed_gorilla[[i]]$pid)
      processed_gorilla[[i]]$eid <- as.factor(processed_gorilla[[i]]$eid)
      processed_gorilla[[i]][pid %in% corrections$pid]$eid <- pid_merge(processed_gorilla[[i]][pid %in% corrections$pid],
                                                                        corrections)$eid
    }}
  return(processed_gorilla)
}
