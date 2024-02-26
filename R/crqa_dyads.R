crqa_dyads <- function(gdata,opt,...){
  #' Vectorized rqa over a set of timeseries grouped by gid
  #' single or multidimensional
  #' figures out all possible dyads in set
  #' and runs rqa on them

  res <- data.table(t(combn(unique(gdata$pid), 2)))
  res[,dyad:=1:.N]

  ## this passes dyads one at a time to run rqa analysis

  pb <- txtProgressBar(min = 0, max = dim(res)[1], style = 3)
  res <- res[,{
    setTxtProgressBar(pb, .GRP);
    do.call(crqa_run,resolve.args(pd=c(V1,V2),gdata=gdata,opt,...))
    },by=.(dyad,V1,V2)]
  close(pb)

  setnames(res,old=c("V1","V2"),new=c("pid1","pid2"))

  return(res)

}
