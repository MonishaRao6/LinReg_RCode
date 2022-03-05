# Function gives summary of numerical and categorical columns 
#separately. Ex: mean, median, mode, sd, missing val, outliers
#Function 1:
library(dplyr)
view_col_summary <- function(x,y)
  {
    numerical_summary=data.frame()
    categorical_summary=data.frame()
      for (i in colnames(x))
        {
          if((class(x[[i]])=='integer')|(class(x[[i]])=='numeric'))
          {
           UL<- mean(x[[i]],na.rm=TRUE)+(sd(x[[i]],na.rm=TRUE)*y)
           LL<- mean(x[[i]],na.rm=TRUE)-(sd(x[[i]],na.rm=TRUE)*y)
           outlier_ind <- length(which(x[,i] < LL | x[,i]  > UL))
           numerical_summary<- rbind(numerical_summary,data.frame(
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
  
    View(numerical_summary)
    View(categorical_summary)
   }

#Function 2:
#mean for numerical less quantity of missing val
NA2mean_num_all_col <- function(p)
{
  for(z in 1:ncol(p))
  {
    if ((sum(is.na(p[,z]))) < 0.1*nrow(p))
    {
      p[is.na(p[,z]), z] <- mean(p[,z], na.rm = TRUE)
    }
    else
    {
      p <- na.omit(p)
    }
  }
  return(p)
}

#Function 3:
#median for numerical more quantity missing val
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

#Function 4:
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

#Function 5:
#mean for less quantity of numerical outlier
Outlier2mean_num_all_col <- function(x,y){
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
        p=mean(x[,i],na.rm=T)
        x[[i]]=ifelse((x[[i]]>UL|x[[i]]<LL),p,x[[i]])
      }
    }
    else
    {
      
    }
    
  }
  return(x)
  
}

#Function 6:
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

#Function 7: Dimensionality Reduction
#sd check, sd<=0.1, drop col
SD_num_all_col <- function(x)
{
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

# Function 8. Dimensionality Reduction : Linearity Check
# if variable transformability can't fix, drop the col

plot_all_col= function (n,p,k)
  for(f in colnames(n)) 
  { 
    if(class(n[[f]])=='character')
    {
      print(f)
      print(ggplot(n, aes(x=n[,f])) + geom_bar())
      Sys.sleep(2)
    }
    else
    {
      print(f)
      print(ggplot(p, aes(y = k, x = n[,f])) + geom_point())
      Sys.sleep(2)
    }
  }



# Function 9: Corelation function
cor_all_col= function (p,q,k)
{ 
  for(f in colnames(q)) 
  { 
    if((class(q[[f]])=='numeric')|(class(q[[f]])=='integer'))
    {
      print(f)
      print(cor(k,q[,f]))
    }
  }
}

#convert factor to character
fact_to_char <- function (s)
{
  for (i in colnames(s))
      {
       if(class(s[,i])=='factor')
        {
          print(i)
          print(class(s[,i]))
          s[,i]<- as.character(s[,i])
          print(class(s[,i]))
         }
      }
  str(s)
  return(s)
}

#convert factor to character
char_to_fact <- function (s)
{
  for (i in colnames(s))
  {
    if(class(s[,i])=='character')
    {
      print(i)
      print(class(s[,i]))
      s[,i]<- as.factor(s[,i])
      print(class(s[,i]))
    }
  }
  str(s)
  return(s)
}