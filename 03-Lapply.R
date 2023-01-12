library(repurrrsive)
sw_films
View(sw_films)

#Answer 1

#creating a function to obtain directors names from sw_films
get_directors <- function(x){
  
  #apply function lapply to obtain all values of director from sw_films
  a <- lapply(sw_films,function(x)x$director)
  #remove repeated values from list by using duplicate function
  a<-a[!duplicated(a)]
  a<-unlist(a)
}
#function created, storing values of function in variable.
b<-get_directors()
b



#Answer 2

#creating a function, to obtain movie infromation by their director name.
get_film_info <- function(x){
  #list that contain movie information from their director name and print NA if director name not found. 
  x_list <- lapply(sw_films,
                   function(x1)
                     if(x1$director==x) x1 else NA)
  
  #Removing NA from list to store only information related to thier director name.
  x_list <- x_list[!is.na(x_list)]
  
  #Insert a list of requirement data into lapply function to obtain same data from every movie.
  movies2<-lapply(x_list, 
                  function(x2) 
                    list(Director=x2$director,
                         Title = x2$title,
                         Episode = x2$episode_id,
                         Opening=x2$opening_crawl ,
                         Date = x2$release_date))
}
#function created, obtain a data by passing a director name in a function.
movies<-get_film_info("George Lucas")
str(movies)