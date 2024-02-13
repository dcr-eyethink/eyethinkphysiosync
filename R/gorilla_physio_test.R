gorilla_physio_test <- function(data,test_task="post_memory",
                                post_task_link=c("item"),memory_id=c("item"),
                           image_id="ad_image",frag_id="ad_fragment",set_id="ad_set"){

  #' Process gorilla data from test phase of physiological sync experiment
  #'
  #' @param data either task data that has already been filtered to the post task, or an imported data list, that will be filtered with test_task
  #' @param test_task names of post tasks to analyse here
  #' @returns td (trial data) and full post test data, with one line per trial
  #' @export



  if (inherits(data, "list")){
    md <- data$data_task[Task.Name %in% test_task]
   }else{  md <- data}



  ## add in column names so that it's all consistent
  if (!"ad_image" %in% colnames(md)){md$ad_image <- md[[image_id]] }
  if (!"ad_fragment" %in% colnames(md)){md$ad_fragment <- md[[frag_id]] }
  if (!"ad_set" %in% colnames(md)){md$ad_set <- md[[set_id]] }

  for (c in 1:length(post_task_link)){
    #if (!condcols[c] %in% colnames(md)){
    md[[ post_task_link[c] ]] <- md[[memory_id[c]]]
    # }
  }


  ### process memory test and ratings
  memd <- md[Zone.Type=="response_button_text",.SD,.SDcols=c("pid",post_task_link,
                                                             "ad_image","ad_fragment","ad_set",
                                                             "rt","Correct")  ]

  memd[,ad_set:=gsub(pattern = " ",replacement = "",x = ad_set)]

  setnames(memd,"Correct","acc")

  condcount <- memd[,.N,by=c("pid",post_task_link,"ad_set","ad_fragment")][N>1]
  if(dim(condcount)[1]>0){
    cat("In the memory stage, you have more than one trial for each item/condition, so I'm going to average rts and accuracy/n")
    print(condcount)
  }

  # one row for each unique item
  d <- dcast(memd,formula = paste(paste(c("pid",post_task_link),collapse = "+"),"~ad_fragment+ad_set"),
             value.var = c("rt","acc"),fun.aggregate = mean)
  # calc the accuracy for critical items, and rts of correct responses
  d <- pid_merge(d,memd[ad_set=="crit",.(acc_crit=mean(acc)),by=c("pid",post_task_link)],
              link = c("pid",post_task_link))

  d <- pid_merge(d,memd[ad_set=="crit" & acc==1,.(rt_crit_correct=mean(rt)),by=c("pid",post_task_link)],link = c("pid",post_task_link))

  ## drpime

  memd[ad_set=="crit" & acc==1,s:="hit"]
  memd[ad_set=="crit" & acc==0,s:="miss"]
  memd[ad_set=="foil" & acc==1,s:="cr"]
  memd[ad_set=="foil" & acc==0,s:="fa"]

  mdw <- dcast(memd[,.(n=.N),by=c("pid",post_task_link,"s")], formula = paste(paste(c("pid",post_task_link),collapse = "+"),"~s"),
               value.var = "n",fill = 0)
  mdw <- mdw[,psycho::dprime(n_hit=hit,n_miss=miss,n_fa = fa,n_cr = cr),by=c("pid",post_task_link)]

  d <- pid_merge(d,mdw,link = c("pid",post_task_link))


  return(list(td=d,post_test_trials=memd))

}
