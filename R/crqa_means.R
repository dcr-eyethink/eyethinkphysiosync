crqa_means <- function(data,average_over="pid1",
                      rqa_vars=c("cor" ,   "RR" ,    "DET",    "NRLINE", "maxL",
                                 "L",  "ENTR",   "rENTR",  "LAM",    "TT"  )){
  #' Averages a table of pairwise comparisons over people (default), trials or conditions
  #'
  #' Doubles the table first, ie looks at full square rather than upper triangle, then gets means of rqa_vars for average_over
  #'
  #' @export


  if (inherits(data, "list")){
    r <- data$r
  }else{
    r <- data
  }

  rn <- copy(r)
  try( {setnames(rn,old=c("pinfo1","pinfo2"), new=c("pinfo2","pinfo1"))},silent = T)

  setnames(rn,old=c("pid1","pid2"), new=c("pid2","pid1"))

  frqad <- rbind(rn,r)

  frqad <- frqad[,lapply(.SD,mean,na.rm=TRUE),by=c(average_over),
                 .SDcols=rqa_vars]


  if  ("pid1" %in% colnames(frqad)){setnames(frqad,old = "pid1",new="pid")}

  return(frqad)
}

