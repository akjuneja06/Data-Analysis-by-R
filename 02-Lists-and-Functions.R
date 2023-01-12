conv_df_to_list<-function(x){

l2<-list()

for (i in 1:length(x[[1]])) 
{
  
    l1<-list()
    for (j in 1:length(x[i,]))
      {
      l1<-append(l1,list(x[[j]][i]))
      }
    names(l1)=names(x)
    l2<-append(l2,list(l1))
 
}
names(l2)=setNames(paste0("R-",1:length(x[[1]])),l2)
str(l2)
}

mtcars1<-head(mtcars)
conv_df_to_list(Formaldehyde)
conv_df_to_list(mtcars1)



