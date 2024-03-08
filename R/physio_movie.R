physio_movie <- function(timeplot=NULL,stimfile,outputfile=NULL,
                         fps=30,spwidth=800,spheight=400,...){

  #' Animates a physio_timeplot, scales and adds to stimulus video into a composite movie
#' @param timeplot a plot from function physio_timeplot, or pass variables for that function here and it will make a new one
#' @param stimfile the stimulus video - leave blank and you can select it with the file OS
#' @param outputfile name for composite movie. If blank, we'll just use the stim file name and add _spark
#' @param fps frame per second of movie
#' @param spwidth width of the sparkline movie - the sitmulus movie will be scaled to the same width before being composited
#' @param spheight height of the sparkline movie
#' @inheritDotParams physio_timeplot
  #' @export

  if (is.null(timeplot)){
    timeplot <- do.call(physio_timeplot,...)
  }

  if (!file.exists(stimfile)){
    rstudioapi::showDialog(message = "I can't find your stim file!","problem")
    stimfile <- NULL
  }

  if(is.null(stimfile)){
    rstudioapi::showDialog(message = "Select the stimulus video file that you want to compsite with the plot",title = "stim location?")
    stimfile <- file.choose()
  }

  if(is.null(outputfile)){
    outputfile <- gsub(x=basename(stimfile),pattern = ".mp4",replacement = "")
  }

  # Get working directory and assign to variable 'wd'
  wd <- getwd()

  contrast <- setdiff(colnames(timeplot$data),c("t","y"))

  playd <- timeplot$data[,
                  approx(x = t,y=y,xout=seq(from=0, to=max(t),by=1/fps) ),by=contrast]

#setnames(playd,"x","t")

  timeplot <- timeplot+ ggplot2::ggtitle("")
  timeplot <- timeplot+ ggplot2::theme( axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.line.x.bottom = element_line(color='black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank())

  #ggplot2::ggsave(p,filename=paste0(wd,"/movies/",itemfocus ,"_sparkline.pdf"),units = "px",
  #       height = spheight,width = spwidth,dpi = 120)

  timeplot <- timeplot+ ggplot2::geom_point(data=playd,aes(x=x,y=y),inherit.aes = F,size=5,alpha=.5)

  #timeplot <- timeplot+ ggplot2::geom_point(data=playd,size=5,alpha=.5)


  timeplot <- timeplot+ gganimate::transition_time(x)


  sparkfile <- paste0(outputfile,"_sparkline.mp4")

  gganimate::anim_save(animation=timeplot,filename = sparkfile,
                       height = spheight,width = spwidth, res=120,
                       fps=fps,nframes=fps*max(playd$x),
                       renderer=gganimate::av_renderer())


  minifile <- gsub(x=sparkfile,pattern = "_sparkline.",replacement = "_mini.")
  combfile <- gsub(x=sparkfile,pattern = "_sparkline.",replacement = "_spark.")



  if(file.exists(combfile)){file.remove(combfile)}
  if(file.exists(minifile)){file.remove(minifile)}

  # ffmpeg -i input.avi -filter:v scale=720:-1 -c:a copy output.mkv

  heightorig<- av::av_media_info(stimfile)$video$height

  system(command = paste0("cd '",wd,
                          "' && ffmpeg -i '", stimfile, "' -filter:v scale=800:-1,setsar=1:1 -c:a copy '",minifile,"'"))

  heightmini<- av::av_media_info(minifile)$video$height

  system(command = paste0("cd '",wd,
                          "' && ffmpeg -i '", minifile, "' -i '",sparkfile,"' -filter_complex '[0:v]pad=",800,":",
                          heightmini+400,":0:[a]; [a][1:v]overlay=0:",
                          heightmini,"[b]' -map 0:a -map '[b]' -pix_fmt yuv420p ",
                          combfile))

   if(file.exists(minifile)){file.remove(minifile)}
   if(file.exists(sparkfile)){file.remove(sparkfile)}
}
