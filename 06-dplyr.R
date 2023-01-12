library(aimsir17)
library(ggplot2)
library(dplyr)

#Answer1:
observations
#Use group_by function to take columns from existing tibble
m<-group_by(observations,station,day,month)
#Prepared new tibble by adding two new columns
s_data<-summarize(m,TotalRain=sum(rain,na.rm = T),AvrTemp=mean(temp,na.rm=TRUE))
s_data
glimpse(s_data)


#Answwer2: 
#Creating new tibble with mutate function and extract the data from last tibble
s_data_diff<- mutate(tibble(arrange(s_data,station,month)),
                     #Use lag function to get prevoius values
  RainDiff=(TotalRain-lag(TotalRain)),
  AbsRainDiff=abs(RainDiff),
  MeanTempDiff=(AvrTemp-lag(AvrTemp)),
  AbsMeanTempDiff=abs(MeanTempDiff))

s_data_diff
glimpse(s_data_diff)

#Answer 3:
arrange(s_data_diff,desc(AbsRainDiff)) |> slice(1:5)
#Answer 4:
arrange(s_data_diff,desc(AbsMeanTempDiff)) |> slice(1:5)

#Answer 5:
m1<-group_by(s_data_diff,station,month)
out<-summarise(m1,AvrDiffTemp=mean(MeanTempDiff,na.rm=TRUE),
               SDDiffTemp=sd(MeanTempDiff,na.rm=TRUE),
               MinDiffTemp=min(MeanTempDiff,na.rm=TRUE),
               MaxDiffTemp=max(MeanTempDiff,na.rm=TRUE),
               AvrDiffRain=mean(RainDiff,na.rm=TRUE),
               SDDiffRain=sd(RainDiff,na.rm=TRUE),
               MinDiffRain=min(RainDiff,na.rm=TRUE),
               MaxDiffRain=max(RainDiff,na.rm=TRUE))
out
glimpse(out)

#Answer 6:
#Created new graph with the help of below functions under ggplot2.
plot <- ggplot(data=out,aes(x=month,y=AvrDiffTemp))+
  geom_line(aes(y=MinDiffTemp),color="blue")+
  geom_line(aes(y=MaxDiffTemp),color="blue")+
  geom_ribbon(aes(ymin=AvrDiffTemp-SDDiffTemp,ymax=SDDiffTemp),fill="pink")+
  geom_point(data=out,aes(x = month, y = AvrDiffTemp))+
  geom_line(aes(y=AvrDiffTemp))+
  facet_wrap(~station)+coord_cartesian(ylim=c(-5,5))+
  #Make scale of x axis from 1 to 12
  scale_x_continuous(n.breaks = 12)+
  #Rotate the scale of x axis by 90°and adjust vertical for 0.075
  theme(axis.text.x = element_text(angle = 90, vjust=-0.075))
plot

#Answer 7:
#Created new graph with the help of below functions under ggplot2.
plot1 <- ggplot(data=out,aes(x=month,y=AvrDiffRain))+
  geom_line(aes(y=MinDiffRain),color="red")+
  geom_line(aes(y=MaxDiffRain),color="red")+
  geom_ribbon(aes(ymin=AvrDiffRain-SDDiffRain,ymax=SDDiffRain),fill="#0000FF",alpha=0.3)+
  geom_point(data=out,aes(x = month, y = AvrDiffRain))+
  geom_line(aes( y = AvrDiffRain))+
  facet_wrap(~station)+coord_cartesian(ylim=c(-20,20))+
  scale_x_continuous(n.breaks = 12)+
  theme(axis.text.x = element_text(angle = 90, vjust=-0.075))

plot1







