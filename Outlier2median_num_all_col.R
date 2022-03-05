#median for numerical outlier


Outlier2median_num_all_col <- function(x,y){
  for (i in colnames(x))
  {
    if((class(x[[i]])=='integer')|(class(x[[i]])=='numeric'))
    {
      UL<- mean(x[[i]],na.rm=TRUE)+(sd(x[[i]],na.rm=TRUE)*y)
      LL<- mean(x[[i]],na.rm=TRUE)-(sd(x[[i]],na.rm=TRUE)*y)
      outlier_ind <- length(which(x[,i] < LL | x[,i]  > UL))
         if(outlier_ind>nrow(x)*0.01)
            {
           x<- subset(x, select=-c(x[[i]]))
            }
         else
            {
              p=median(x[,i],na.rm=T)
              x[[i]]=ifelse((x[[i]]>UL|x[[i]]<LL),p,x[[i]])
            }
    }
  else
    {
      
    }
   
  }
  return(x)

}