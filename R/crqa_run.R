crqa_run <- function(pd=NULL,gdata,opt=F,drawplot=F,reverse_one_ts=F,plot_ts=F,scramble=F,
                    metric="euclidean",
                    windowsize=0, windowstep=0,profile=0,
                    delay=6, embed=5, rescale=1, radius=0.4,
                    maxlag =  20,radiusspan = 100, radiussample = 40, normalize=0,
                    mindiagline=10, minvertline=2, tw=0, whiteline=F,
                    recpt = FALSE, side = "both",
                    datatype = "continuous", fnnpercent  = 10,
                    typeami = "mindip", nbins  = 50, criterion = "firstBelow",
                    threshold = 1, maxEmb = 20, numSamples = 500, tau=1,
                    Rtol = 10, Atol = 2,silent=T,printpd=F){



  ## if we weren't told dyads, get them all
  if (is.null(pd)){
    dyads <- F
    pd <- unique(gdata$pid)
  }else{dyads <- T}

  ts <- na.omit(dcast(data = gdata[pid %in% pd],formula = x~pid+dv,value.var = "y"))
  ts$x <- NULL

  use <- T
  if (dim(ts)[1]<(embed*delay)){use <- F}

  n <- dim(ts)[2]

item_name <- gsub(x=gsub(x=gdata[1,tid],pattern = gdata[1,pid],replacement = ""),pattern = "-",replacement = " ")

  if (dyads){


    ts1 <- ts[,.SD,.SDcols=grep(colnames(ts),pattern = pd[1],value = T)]
    ts2 <- ts[,.SD,.SDcols=grep(colnames(ts),pattern = pd[2],value = T)]

    tid1 <- unique(gdata[pid==pd[1]]$tid)
    tid2 <- unique(gdata[pid==pd[2]]$tid)

    if(printpd){print(paste(tid1,tid2))}

    # check the same number of dimensions
    if(!length(ts1)==length(ts2)){use <- F}

    # set the method for single or multi dimensions
    if (length(ts1)==1){method="crqa"}else{method="mdcrqa"}


    if (opt){
      # run optimisation for mdcrqa or crqa
      if (use){

        rqa_stats <- try(crqa::optimizeParam(ts1=ts1, ts2=ts2,
                                             par = list(method=method,metric=metric,
                                                        maxlag=maxlag,radiusspan=radiusspan,
                                                        radiussample=radiussample,
                                                        normalize=normalize,rescale=rescale,
                                                        mindiagline=mindiagline,minvertline=minvertline,
                                                        tw=tw,whiteline=whiteline,recpt=recpt,
                                                        side=side,datatype=datatype,fnnpercent=fnnpercent,
                                                        typeami=typeami,nbins=nbins,criterion=criterion,
                                                        threshold=threshold,maxEmb=maxEmb,
                                                        numSamples=numSamples,Rtol=Rtol,Atol=Atol)),silent = silent)

        # print(rqa_stats)
        if (class(rqa_stats)=="try-error" | class(rqa_stats)=="NULL"){
          rqa_stats <- list(radius=0, emddim=0, delay=0, use=0) }else{
            rqa_stats$use <- 1
          }

      }else{
        #  return empty data
        rqa_stats=list(radius=0, emddim=0, delay=0, use=0)
      }

    }else{
      # we're doing CRQA not optimisation

      if (windowstep==0){
        #############################################
        # try a regular crqa
        if (use){

          # reverse row order
          if(reverse_one_ts){
            ts1 <- ts1[nrow(ts1):1]
          }

          # scramble order
          if(scramble){
            ts1 <- ts1[sample(1:nrow(ts1))]
          }

          if(plot_ts){

            print(  ggplot(data=rbind(data.table(x=1:nrow(ts1),y=ts1[[1]],pid=colnames(ts)[1]),
                                      data.table(x=1:nrow(ts2),y=ts2[[1]],pid=colnames(ts)[2])),
                           aes(x=x,y=y,colour=pid,group=pid))+geom_line()+ggtitle(item_name))
          }


          ## run main rqa
          res <- crqa::crqa(ts1, ts2, delay=delay, embed=embed, rescale=rescale, radius=radius,
                            normalize=normalize,mindiagline=mindiagline, minvertline=minvertline,
                            tw=tw, whiteline=whiteline, recpt=recpt, side=side, method=method,
                            metric=metric,datatype=datatype)

          rqa_stats <- as.data.table(res[1:10])
          rqa_stats[,names(rqa_stats) := lapply(.SD, as.numeric)]
          rqa_stats$tid1 <- tid1
          rqa_stats$tid2 <- tid2

          if(drawplot){print(crqa::plotRP(res$RP,par=list(unit = 1, labelx = colnames(ts)[1], labely = colnames(ts)[2],
                                                          cols = "blue", pcex = 1, pch = 19,
                                                          labax = round(seq(0, nrow(res$RP), by=nrow(res$RP)/4)),
                                                          labay = round(seq(0, nrow(res$RP),by=nrow(res$RP)/4)),
                                                          las = 1))+title(item_name))}
          # profile ?

          if (profile>0){
            pres <- crqa::drpfromts(ts1, ts2,
                                    windowsize = profile,
                                    delay=delay, embed=embed, rescale=rescale, radius=radius,
                                    normalize=normalize,mindiagline=mindiagline, minvertline=minvertline,
                                    tw=tw, whiteline=whiteline, recpt=recpt, side=side, method=method,
                                    metric=metric,datatype=datatype)
            #print(res)

            rp <- as.list(pres$profile)
            names(rp) <- paste0("lag",names(rp))
            rp$maxlag <- as.numeric(names(pres$maxlag))[1]
            rp$maxrec <- pres$maxrec

            setDT(rp)
            rqa_stats <- cbind(rqa_stats,rp)

          }

        }else{
          # send back an empty result
          rqa_stats <- data.table(RR=as.numeric(NA),DET=as.numeric(NA),
                                  NRLINE=as.numeric(NA),maxL=as.numeric(NA),
                                  L=as.numeric(NA),ENTR=as.numeric(NA),
                                  rENTR=as.numeric(NA),LAM=as.numeric(NA),
                                  TT=as.numeric(NA),catH=as.numeric(NA),tid1=tid1,tid2=tid2)
          #rqa_stats <- data.table()
        }
      }else{
        #############################################
        # try a windowed crqa

        # check lengths
        #length(ts1)

        if (use){

          res <- tryCatch({
            crqa::wincrqa( ts1, ts2, windowstep = windowstep, windowsize = windowsize,
                           delay=delay, embed=embed, rescale=rescale, radius=radius,
                           normalize=normalize,mindiagline=mindiagline, minvertline=minvertline,
                           tw=tw, whiteline=whiteline, recpt=recpt, side=side, method=method,
                           metric=metric,datatype=datatype,trend=FALSE)
          },
          error=function(cond){
            return(    data.table(RR=as.numeric(NA),DET=as.numeric(NA),
                                  NRLINE=as.numeric(NA),maxL=as.numeric(NA),
                                  L=as.numeric(NA),ENTR=as.numeric(NA),
                                  rENTR=as.numeric(NA),LAM=as.numeric(NA),
                                  TT=as.numeric(NA),
                                  catH=as.numeric(NA), win=as.numeric(NA),
                                  TREND=as.numeric(NA),  t=as.numeric(NA),
                                  use=0))
          })


          #print(res)
          setDT(res)

          res[,t := win * windowstep]
          # print(res)

          rqa_stats <- data.table(RR=as.numeric(res$RR),DET=as.numeric(res$DET),
                                  NRLINE=as.numeric(res$NRLINE),maxL=as.numeric(res$L),
                                  L=as.numeric(res$L),ENTR=as.numeric(res$ENTR),
                                  rENTR=as.numeric(res$rENTR),LAM=as.numeric(res$LAM),
                                  TT=as.numeric(res$TT),
                                  catH=as.numeric(res$catH), win=as.numeric(res$win),
                                  TREND=as.numeric(res$TREND),  t=as.numeric(res$t),
                                  use=1)

        }else{
          # send back an empty window result
          rqa_stats <- data.table(RR=as.numeric(NA),DET=as.numeric(NA),
                                  NRLINE=as.numeric(NA),maxL=as.numeric(NA),
                                  L=as.numeric(NA),ENTR=as.numeric(NA),
                                  rENTR=as.numeric(NA),LAM=as.numeric(NA),
                                  TT=as.numeric(NA),
                                  catH=as.numeric(NA), win=as.numeric(NA),
                                  TREND=as.numeric(NA),  t=as.numeric(NA),
                                  use=0)
        }

      }
    }
  } else {
    #############################################
    # no dyads, so multidemensional
    if(opt){
      if (use){
        ## run mdrqa opt

        delay <-try( mdDelay(ts, nbins, maxlag, criterion, threshold),silent = T)
        fnn <-try(  mdFnn(as.matrix(ts), tau, maxEmb, numSamples, Rtol, Atol),silent = T)

        if (class(delay)=="try-error" | class(delay)=="NULL"){
          use <- F }else{
            use <- T
          }
      }

      if(use){
        rqa_stats <- as.data.table(as.list(c(delay,fnn$fnnPerc,1))) ## must be a smarter way to do this...
        names(rqa_stats) <- c("delay",fnn$embTimes,"use")
      }else{
        # send back blank opt data
        rqa_stats <- as.data.table(as.list(rep(0,maxEmb+2)))
        names(rqa_stats) <- c("delay",1:maxEmb,"use")
      }

    }else{
      # run a multidemensional rqa
      if (use){


        res <- crqa::crqa(ts1 = ts, ts2=ts, delay=delay, embed=embed, rescale=rescale, radius=radius,
                          normalize=normalize,mindiagline=mindiagline, minvertline=minvertline,
                          tw=tw, whiteline=whiteline, recpt=recpt, side=side, method="mdcrqa",
                          metric=metric,datatype=datatype)
        rqa_stats <- data.table(RR=as.numeric(res$RR),DET=as.numeric(res$DET),
                                NRLINE=as.numeric(res$NRLINE),maxL=as.numeric(res$L),
                                L=as.numeric(res$L),ENTR=as.numeric(res$ENTR),
                                rENTR=as.numeric(res$rENTR),LAM=as.numeric(res$LAM),
                                TT=as.numeric(res$TT),use=1,n=n)


        if(drawplot){(myRRplot(RP = res$RP))}
      }else{
        # send back blank  data

        rqa_stats <- data.table(RR=as.numeric(NA),DET=as.numeric(NA),
                                NRLINE=as.numeric(NA),maxL=as.numeric(NA),
                                L=as.numeric(NA),ENTR=as.numeric(NA),
                                rENTR=as.numeric(NA),LAM=as.numeric(NA),
                                TT=as.numeric(NA),use=0)
      }
    }

  }
  return(rqa_stats)

}

