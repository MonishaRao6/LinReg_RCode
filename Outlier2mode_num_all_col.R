#median for numerical more quantity missing val
library(dplyr)
NA2median_num_all_col <- function(p)
{
  for(z in 1:ncol(p))
 {
   if ((sum(is.na(p[,z]))) < 0.1*nrow(p))
   {
    p[is.na(p[,z]), z] <- median(p[,z], na.rm = TRUE)
   }
   else
   {
    p <- na.omit(p)
   }
 }
 return(p)
}