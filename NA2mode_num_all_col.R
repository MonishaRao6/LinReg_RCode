#mode for categorical missing val

NA2mode_num_all_col <- function(p,y)
{
  for(z in 1:ncol(p))
    {
    if ((sum(is.na(p[,z]))) < 0.1*nrow(p))
      {
         p[is.na(p[,z]),z]<- names(sort(-table(p[,z])))[1]
      }
    else
      {
         p <- na.omit(p)
      }
    }
  return(p)
}