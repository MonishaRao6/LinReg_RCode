plot_all_col= function (x,p){
  
  for(f in names(x)) 
{ # Printing ggplot within for-loop
  print(f)
  print(ggplot(data=my_data, aes(x = p, y = my_data[ , f])) + geom_point())
  Sys.sleep(2)
  }
}