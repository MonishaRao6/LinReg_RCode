#median for numerical outlier


SD_num_all_col <- function(x){
  for (i in colnames(x))
  {
    if((class(x[[i]])=='integer')|(class(x[[i]])=='numeric'))
    {
      sd_1=sd(x[[i]])
      if(sd_1<=0.01)
      {
        x<- subset(x, select=-c(x[[i]]))
      }
      else
      {
       print(sd_1)
      }
    }
    else
    {
      
    }
    
  }
  return(x)
  
}