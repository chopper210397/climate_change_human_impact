library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

co2<-read.csv("co2emissions.csv",sep=",",skip = 4)
str(co2)
unique(co2$Country.Code)
unique(co2$Country.Name)
aruba<-co2 %>% filter(Country.Code== c("IDN","PER"))
str(aruba)
# creating a tidy data
indicadoresaruba<-aruba %>% select(!c("Country.Code","Country.Name","Indicator.Code"))%>%
  pivot_longer(!Indicator.Name,names_to = "fecha",values_to = "valor")
# formatting dates
indicadoresaruba$fecha<-substr(indicadoresaruba$fecha,2,5)
indicadoresaruba$fecha<-ymd(indicadoresaruba$fecha, truncated = 2L)
# plotting or at least trying
unique(indicadoresaruba$Indicator.Name)
# crecimiento de la población para aruba desde 1960
indicadoresaruba %>% filter(Indicator.Name=="Population, total") %>%
  ggplot(aes(x=fecha,y=valor/1000))+geom_line()+labs(x="",y="Población en miles")+theme_light()

# graph line for all interesting variables for aruba
indicadoresaruba %>%
  filter(Indicator.Name %in% c("Population, total",
                               "Nitrous oxide emissions (thousand metric tons of CO2 equivalent)",
                               "CO2 emissions from liquid fuel consumption (kt)",
                               "Forest area (sq. km)",
                               "Agricultural land (sq. km)")) %>%
  ggplot(aes(x=fecha,y=valor/1000,color=Indicator.Name))+geom_line()+labs(x="",y="Población en miles")+theme_light()

# EMISION DE CO2 POR CONSUMO DE COMBUSTIBLES LIQUIDOS
indicadoresaruba %>%
  filter(Indicator.Name %in% c(
                               
                               "CO2 emissions from liquid fuel consumption (kt)")) %>%
  ggplot(aes(x=fecha,y=valor/1000,color=Indicator.Name))+geom_line()+labs(x="",y="miles")+theme_light()


indicadoresaruba %>%
  filter(Indicator.Name %in% c(
    "Nitrous oxide emissions (thousand metric tons of CO2 equivalent)",
    "Forest area (sq. km)",
    "Agricultural land (sq. km)")) %>%
  ggplot(aes(x=fecha,y=valor,color=Indicator.Name))+geom_line()+labs(x="",y="")+theme_light()

unique(co2$Country.Name)


# WORKING WITH ALL THE DATA

co2 %>% select(!c("Country.Code","Indicator.Code"))%>%
  pivot_longer(!c(Indicator.Name, Country.Name),names_to = c("fecha"),values_to = "valor")
