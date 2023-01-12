library(aimsir17)
library(ggplot2)
library(dplyr)
library(tidyr)


#Answer 1:

#firtly, apply filter for requested stations
obs<-filter(observations,station %in% c("DUBLIN AIRPORT", 
                                        "SherkinIsland",
                                        "MACE HEAD"))
#Added new column and give conditions to season from months
obs<-obs|>mutate(Season = case_when(month=="1"|month=="11"|month=="12"~"Winter", 
                                 month=="2" | month=="3"|month=="4"~"Spring", 
                                 month=="5" | month=="6"|month=="7"~"Summer",
                                 month=="8" | month=="9"|month=="10"~"Autumn"))

glimpse(obs)

#Answer 2:
#Extract information from eirgrid17
ener<-group_by(eirgrid17,year,month,day,hour)|>
  #Summarise that columns with new columns of IE and NI means.
  summarise(IE=mean(IEDemand,na.rm=T),
  NI=mean(NIDemand,na.rm=T),CheckObs=n()) |>
  ungroup()

#Answer 3:

#perform left join as per question
x<-left_join(ener,obs)
set.seed(100)
#Select random 10% data by using sample function.
ds <- x[sample(nrow(x), size=(round(nrow(x)*.1))),]
glimpse(ds)

#Answer 4:
#Instead of reducing only selected required columns.
ds<-select(ds,station,month,temp,Season,IE,NI)

#Answer 5:
#Created new dataset by using pivot_longer
ds1<-pivot_longer(ds,cols=c('IE','NI'),
                         names_to='Area',
                         values_to='Demand')

#Answer 6:
#Plot a graphy using 4 columns(temp,Area,staion and Season).
ggplot(data=ds1,aes(x=temp,y=Demand,color=Area)) +
               geom_smooth(method='lm')+
  facet_grid(station~Season)+geom_point()

#Answer 7:
#Plot a graphy using 4 columns(temp,Area,staion and month).
ggplot(data=ds1,aes(x=temp,y=Demand,color=Area)) +
  facet_grid(station~month)+ geom_smooth(method='lm')+geom_point()

#Answer 8:
#Group for Season and station column
cor_season <-group_by(ds,station, Season) |>
#Calculated the correlations of IE and NI respect to temp and difference of those. 
  summarise(Corr_IE=cor(IE,temp), Corr_NI=cor(NI,temp),diff=Corr_IE-Corr_NI) |>
  ungroup()

slice(cor_season,1:nrow(cor_season))

#Answer 9:

#Group for month and station column
cor_month <-group_by(ds,station,month) |>
#Calculated the correlations of IE and NI respect to temp and difference of those. 
  summarise(Corr_IE=cor(IE,temp), Corr_NI=cor(NI,temp),diff=Corr_IE-Corr_NI) |>
  ungroup()

slice(cor_month,1:nrow(cor_month))


