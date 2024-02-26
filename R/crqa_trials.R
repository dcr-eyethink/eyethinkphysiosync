crqa_trials <- function(data,condcols=c("category","condition","item","stim"),
                        pinfo=NULL,pid="pid",
                        edvs=NULL,opt=F,dyads=T,pseudo_dyads=F,
                        announce=F,samplerate=1,...){
  #' Runs crqa, mdcrqa or mdrqa on pairs or groups
  #'
  #' Will group together items using condcols, or people using pinfo
  #' @param data ed or a data list containing ed, labelled emotibit data after compiling and merging functions
  #' @param condcols column names that identify unique trials that will be analysed together with crqa. Null is only one trial per person
  #' @param pinfo column for a person designator, for in out group differences
  #' @param pid column name for unique subject ID
  #' @param edvs list of DVs to crq process, can be multidimensional. if null does all
  #' @param dyads if cross recurrence between dyads T, or F for multidimensional analysis

  #' @export

  if (inherits(data, "list")){
    ed <- data$ed[filter==1]
  }else{
   ed <- data[filter==1]
 }

  ptm <- proc.time()

  ed$pid <- as.factor(ed[[pid]])
  if(is.null(edvs)){edvs <- as.list(unique(ed$dv))}
  # trim to those DVs we are using
  ed <- ed[dv %in% unlist(edvs)]

  if (file.exists("temp_rqa.csv")){file.remove("temp_rqa.csv")}

  # ## create gid variable for grouping
  # should be able to do this without c overting to dataframe and back...
  if(is.null(condcols)){ed$gid <- "single_item"}else{
    ed <- data.frame(ed)
    if(length(condcols)>1){
      ed$gid <- apply( ed[ , condcols ] , 1 , paste , collapse = "§" )
    }else{
      ed$gid <- ed[ , condcols ]
     }
     ed <- data.table(ed)
  }
   ed$gid <- as.factor(ed$gid)


  # NOT IMPLEMENETD YET
  # if (pseudo_dyads){
  #   glist <-  ed[,.(orig_gid=unique(gid),tid=unique(tid),i=1:length(unique(gid))),by=.(pid)]
  #   glist[,gind:=sample(i),by=pid]
  #   glist[,gid:=paste0("pseudo-",gind)]
  #   setnames(ed,"gid","orig_gid")
  #   ed <- pid_merge(ed,glist[,.(tid,gid)],link = "tid")
  #   ed$gid <- as.factor(ed$gid)
  #
  # }

  if(!is.null(pinfo)){
    pinfo <- ed[,.(p=unique(get(pinfo))),by=.(pid,gid)]
  }

  # ## resample so that data every second
  # # discarding  <8 samples

  ed[,tt:= as.numeric(tt)]
  ed[!is.na(norm),samples:=.N,by=c("pid","tid","dv","gid")]

  ed <- ed[ samples >=8,
            approx(x=tt,y=norm,
                   xout =seq(from=floor(min(tt)),
                             to=round(max(tt)),by=1/samplerate)),by=c("pid","tid","dv","gid")]


  ed[,in_gid:=length(unique(pid)),by=gid]

  rqatable <- data.table()
  profiles <- data.table()

  for (edv in edvs){

    grpn <- uniqueN(ed[dv %in% edv]$gid)


    items=levels(ed[dv %in% edv]$gid)

    # this chunks the data into lumps by gid
    # which contain all the people/trials that should be compared to each other

    if (dyads){
      # passes them to cross_recurrence_dyads for processing all individual dyads in gid set
      r <- ed[dv %in% edv & in_gid>1,{cat(edv," item: ",items[gid],"  progress",round(.GRP/grpn*100),"%\n");
        crqa_dyads(gdata =.SD,opt,...)},by=gid]

    }else{
      # we're doing multidimensional rqa over groups
      r <- ed[dv %in% edv & in_gid>1,{cat(edv," item: ",items[gid],"  progress",round(.GRP/grpn*100),"%\n");
        crqa_run(gdata =.SD,pd=NULL,opt,...)},by=gid]
    }

    rqatable <- rbind(rqatable,data.frame(dv=paste(edv,collapse = "."),r))
  }

  fwrite(rqatable,"temp_rqa.csv")

  # if (pseudo_dyads){
  #   # renter gid info FIX THIS
  #   rqatable <- doubleup_rqatable(rqatable)
  #   rqatable[,pid:=pid1]
  #   rqatable <- pid_merge( rqatable,glist[,.(pid,gid,orig_gid,tid)],link=c("pid","gid"))
  #   setnames(rqatable,old=c("gid","orig_gid"),new=c("pseudo_id","gid"))
  #   #  setcolorder(rqatable,c("pid", "dyad","dv",condcols))
  # }



  # relabels the data nicely

  if (!opt ){
    if (!is.null(pinfo)){
      ## label the rqa table with pinfo of par1 and par2
      ## calculate sgroup (in/out) and pgroup(pinfo/out)

      setkey(pinfo,pid,gid)
      setkey(rqatable,pid1,gid)
      #pinfo <- pinfo[!duplicated(pid)]
      rqatable <- pinfo[rqatable]

      setnames(rqatable,old=c("p","pid"),new=c("pinfo1","pid1"))
      setkey(rqatable,pid2,gid)
      rqatable <- pinfo[rqatable]
      setnames(rqatable,old=c("p","pid"),new=c("pinfo2","pid2"))
      rqatable[,sgroup:="out"]
      rqatable[pinfo1==pinfo2,sgroup:="in"]
      rqatable[,pgroup:="out"]
      rqatable[pinfo1==pinfo2,pgroup:=pinfo1]

      setcolorder(rqatable,c("dyad","gid","dv","pid1","pid2",
                             "pinfo1","pinfo2","sgroup","pgroup"))

    }

    #label the rqatable with gid stuff
    if(!is.null(condcols)){
      if(length(condcols)>1){
        for (ci in 1:length(condcols)){
          #  2  before
          rqatable[[condcols[ci]]] <- tstrsplit(rqatable$gid,"§")[[ci]]

        }
      }
      setcolorder(rqatable,condcols)
    }
  }
  cat("That took me\n")
  print(  (proc.time() - ptm)/60 )

  if(announce){system("say Your RQA analysis has completed!")}

  if ("dv" %in% colnames(rqatable)){setnames(rqatable,"dv","edv")}

  return(rqatable)
}
