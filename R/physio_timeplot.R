physio_timeplot <- function(data,plotmetric,plotdv,
                            selection=NULL,
                            contrast=NULL,
                            smoother=0.3,
                            plot_error=F,
                            title=NULL,
                            outp="analysis",h=4,w=6){
  #' plots data from dm
  #' @param data data list from physio_merge
  #' @param plotmetric The metric in data$dm to plot, eg DET or norm
  #' @param plotdv The dv, eg HR or EA
  #' @param selection A datatable with the columns and values that you want to use to select the data to plot
  #' @param smoother parameter for loess smoothing. set to 0 for none
  #' @param plot_error give error from smoothing
  #' @importFrom ggplot2 ggplot aes
  #' @export

  d <- data$dm[metric %in% plotmetric & edv %in% plotdv]

  if(!is.null(selection)){
    matchvars <- colnames(selection)
    selection$include <- TRUE
    d <- pid_merge(d,selection,link = matchvars)[(include)]
  }else{d$include <- T}

  if (is.null(contrast)){contrast <- "include"}


  lp <- function(data,smoother,give_error){
    if(dim(data)[1]<3){return(NULL)}else{
      dv <- last(colnames(data))
      model <- loess(y~t, data=data,span = smoother)
      xseq=seq(from=0, to=max(data$t,na.rm=T), by=.5)
      pred <- predict(model, newdata = data.frame(t = xseq), se=give_error)

      if(give_error){
        dl=data.table(t=xseq,
                      ymin=pred$fit-pred$se.fit,
                      ymax=pred$fit+pred$se.fit,
                      y=pred$fit)
        colnames(dl) <- c("t","ymin","ymax","y")
      }else{
        dl=data.table(t=xseq,y=pred)
        colnames(dl) <- c("t","y")
      }
      return(dl)
    }}
  if (smoother>0){
  dts <- d[,lp(data=.SD,smoother =smoother,give_error = plot_error),
       by=c(contrast)]
  }else{
    dts <- d
  }


  p <- ggplot2::ggplot(data = dts,
           aes(x =t,
             y = y,
             colour=.data[[contrast]] ))
  if (plot_error){
    p <- p+ ggplot2::geom_ribbon(alpha=.1 ,linewidth=0,aes(ymin=ymin,ymax=ymax,
                                                  fill=.data[[contrast]]))
  }

    p <- p + ggplot2::geom_line(linewidth = 2)


  p <-  p + ggplot2::xlab("seconds") + ggplot2::theme_bw() + ggplot2::theme(
    axis.ticks.y = element_blank(),
    plot.title = element_text(face = "bold",hjust = 0)
  )


if(contrast=="include"){
  p <- p + ggplot2::theme(legend.position = "none")
}else{
  p <- p +  ggplot2::theme(legend.position = "bottom",
        legend.text = element_text(size = 8))
}

  if(!is.null(title)){
    p <- p+ggtitle(title)
  }

  if (is.character(outp)){
    dir.create(file.path(outp),showWarnings = FALSE)
  ggplot2::ggsave(paste0(outp,"/",title,".pdf"),
                  width = w, height = h, limitsize = FALSE)
  }

  return(p)

}
