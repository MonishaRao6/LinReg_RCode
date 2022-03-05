view_col_summary <- function(x,y){
  outlier_summary=data.frame()
  categorical_summary=data.frame()

  for (i in colnames(x))
  {
  
    if((class(x[[i]])=='integer')|(class(x[[i]])=='numeric'))
    {
      UL<- mean(x[[i]],na.rm=TRUE)+(sd(x[[i]],na.rm=TRUE)*y)
      LL<- mean(x[[i]],na.rm=TRUE)-(sd(x[[i]],na.rm=TRUE)*y)
      outlier_ind <- length(which(x[,i] < LL | x[,i]  > UL))
      outlier_summary<- rbind(outlier_summary,data.frame(
                                              col_name=i,
                                              col_class=class(x[[i]]),
                                              col_na_count=sum(is.na(x[[i]])),
                                              col_mean= mean(x[[i]],na.rm=TRUE),
                                              col_median= median(x[[i]],na.rm=TRUE),
                                              col_sd  = sd(x[[i]],na.rm=TRUE),
                                              col_upp_lim= UL,
                                              col_low_lim= LL,
                                              col_max= max(x[[i]],na.rm=TRUE),
                                              col_min= min(x[[i]],na.rm=TRUE),
                                              no_of_outliers=outlier_ind,
                                              outliers_percent=(outlier_ind/nrow(x))*100
                                              ))
      

    }
    else
    {
        
      
        categorical_summary<- rbind(categorical_summary,data.frame(
        col_name=i,
        col_class=class(x[[i]]),
        col_na_count=sum(is.na(x[[i]])),
        col_mode= names(sort(-table(x[,i])))[1]
      ))
      
      
      
    }
   
  }
  
  View(outlier_summary)
  View(categorical_summary)
  
}



