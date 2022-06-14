library(tidyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(dplyr)

setwd("C:/Users/kreinl1/OneDrive/OneDrive - UW-Madison/GitHub/SWMP/Data/Nutreints")

Nutrients <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.))

setwd("C:/Users/kreinl1/OneDrive/OneDrive - UW-Madison/GitHub/SWMP/Data/WQ")

WQ <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.))

setwd("C:/Users/kreinl1/OneDrive/OneDrive - UW-Madison/GitHub/SWMP")

Nut_long <- melt(setDT(Nutrients), id.vars = c("StationCode","REP","DateTimeStamp"), 
                    measure.vars = c("PO4F","NH4F","NO3F", "CHLA_N"))

nut_df<-separate(Nut_long, DateTimeStamp, into = c("Date", "Time"), sep = " ")
nut_df_2<-separate(nut_df, Date, into = c("Month", "Day","Year"), sep = "/")

nut_all<-nut_df_2[(nut_df_2$Month!=04|nut_df_2$Month!=11),]

ggplot(data=nut_all, aes(x=StationCode,y=value))+
  geom_boxplot()+
  geom_point(aes(color=Year))+facet_wrap(~variable,scales="free")+theme_bw()

nut_mean<-na.omit(nut_all) %>%
  group_by(StationCode,Year,variable) %>%
  summarise(mean = mean(value), n = n())

ggplot(data=nut_mean, aes(x=StationCode,y=mean))+
  geom_boxplot()+
  geom_point(aes(color=Year))+facet_wrap(~variable,scales="free")+theme_bw()

nut_rm_21<-nut_mean[(nut_df_2$Year!=2021),]

ggplot(data=nut_rm_21, aes(x=StationCode,y=value))+
  geom_boxplot()+
  geom_point(aes(color=Year))+facet_wrap(~variable,scales="free")+theme_bw()

####################

WQ_long <- melt(setDT(WQ), id.vars = c("StationCode","DateTimeStamp"), 
                 measure.vars = c("Temp","SpCond", "Sal","DO_Pct","pH","Turb"))

WQ_long_na<-na.omit(WQ_long)

wq_df<-separate(WQ_long_na, DateTimeStamp, into = c("Date", "Time"), sep = " ")

wq_df_2<-separate(wq_df, Date, into = c("Month", "Day","Year"), sep = "/")

wq_df_2$MonthYear<- paste(wq_df_2$Month, "-", wq_df_2$Year)

wq_all<-wq_df_2[(wq_df_2$Month!=04|wq_df_2$Month!=11),]

wq_mean<-wq_all %>%
  group_by(StationCode,Year,variable) %>%
  summarise(mean = mean(value), n = n())


ggplot(data=wq_mean, aes(x=StationCode,y=mean))+
  geom_boxplot()+
  geom_point(aes(color=Year))+facet_wrap(~variable,scales="free")+theme_bw()

wq_outlier_rm<-wq_mean[wq_mean$Year!=2021,]

ggplot(data=wq_outlier_rm, aes(x=StationCode,y=mean))+
  geom_boxplot()+
  geom_point(aes(color=Year))+facet_wrap(~variable,scales="free")+theme_bw()
