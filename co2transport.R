library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(ggplot2)
library(lubridate)

co2<-read_xls("co2transport.xls")
co2 %>% select(`Indicator Name`)
formateddata<-co2 %>% select(!c("Country Code","Indicator Code")) %>%
  pivot_longer(!c( `Country Name`,`Indicator Name`),names_to = c("fecha"),values_to = "valor")

# formateddata<-formateddata %>% pivot_wider(names_from = `Indicator Name`,values_from = valor)
# looking the data values
unique(formateddata$`Country Name`)
unique(formateddata$fecha)
unique(formateddata$`Indicator Name`)
# formatting dates
formateddata$fecha<-ymd(formateddata$fecha, truncated = 2L)

formateddata %>% filter(fecha>"1995-01-01") %>% filter(`Country Name`=="Perú")%>%  ggplot(aes(x=fecha,y=valor))+geom_line(color="red")+
  labs(x="",y="% del total de la quema de combustible",
       title="% Emisiones de CO2 en el Perú de 1995 a 2015",caption = "Fuente: Elaboración propia - Banco Mundial")+
  theme_light()
ggsave("co2transportemissionperu.jpg",dpi = 300)



na.omit(formateddata) %>% filter(fecha>"1995-01-01") %>% group_by(fecha) %>%
  summarise(soles=mean(valor)) %>% ggplot(aes(x=fecha,y=soles))+geom_line(color="blue",size=1.5)+
  labs(x="",y="promedio del % del total de la quema del combustible",
       title = "% Emisiones de CO2 en el mundo de 1995 a 2015",caption = "Fuente: Elaboración propia - Banco Mundial")+
  theme_light()

ggsave("co2transportemissionworld.jpg",dpi = 300)

# parque vehicular estimado
cars<-read_xlsx("vehicularpark.xlsx")
cars$numero<-as.numeric(cars$numero)
cars$Periodo<-ymd(cars$Periodo, truncated = 2L)

cars %>% ggplot(aes(x=Periodo,y=numero/1000000))+geom_line(color="green",size=2)+
  labs(x="",y="millones de vehículos",
       title = "Parque vehicular estimado en el Perú del 2007 al 2018",caption = "Fuente: Elaboración propia - SINIA")+theme_light()

ggsave("vehicularnumberperu.jpg",dpi = 300)
