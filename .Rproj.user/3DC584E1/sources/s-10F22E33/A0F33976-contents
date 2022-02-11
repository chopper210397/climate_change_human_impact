library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

co2<-read.csv("co2emissions.csv",sep=",",skip = 4)
str(co2)
unique(co2$Country.Code)
unique(co2$Country.Name)



#------------------------------------  DATA FORMAT -----------------------------------#
formateddata<-co2 %>% select(!c("Country.Code","Indicator.Code"))%>%
  pivot_longer(!c( Country.Name,Indicator.Name),names_to = c("fecha"),values_to = "valor",names_pattern = )
formateddata<-formateddata %>% pivot_wider(names_from = Indicator.Name,values_from = valor)
# looking the data values
unique(formateddata$Country.Name)
# formatting dates
formateddata$fecha<-substr(formateddata$fecha,2,5)
formateddata$fecha<-ymd(formateddata$fecha, truncated = 2L)

#----------------- GGPLOT -----------------#
# PLOTS TOTAL POPULATION
formateddata %>% filter(Country.Name %in% c("Peru","Chile"))%>% 
  ggplot(aes(x=fecha,y=`Population, total`/1000000,color=Country.Name))+geom_line()+
  labs(x="",y="Población en millones",color="País",title = "Crecimiento poblacional en Sudamérica")+theme_minimal()

# ggsave("poblacionsudamerica.jpg",dpi = 400)

formateddata %>% filter(Country.Name %in% c("Canada","Mexico"))%>% 
  ggplot(aes(x=fecha,y=`Population, total`/1000000,color=Country.Name))+geom_line()+
  labs(x="",y="Población en millones",color="País",title = "Crecimiento poblacional en Norteamerica")+theme_minimal()

# ggsave("poblacionnorteamerica.jpg",dpi = 400)

formateddata %>% filter(Country.Name %in% c("India","China"))%>% 
  ggplot(aes(x=fecha,y=`Population, total`/1000000,color=Country.Name))+geom_line()+
  labs(x="",y="Población en millones",color="País",title = "Crecimiento poblacional en resto del mundo")+theme_minimal()

# ggsave("poblacionrestomundo.jpg",dpi = 400)
# PLOTS C02 EMISSION
formateddata %>% filter(Country.Name %in% c("Peru","Chile"))%>% 
  ggplot(aes(x=fecha,y=`CO2 emissions (kt)`/1000000,color=Country.Name))+geom_line()+
  labs(x="",y="Emisiones de CO2 (kt), millones",color="País",title = "Emisiones de CO2 en Sudamérica")+theme_minimal()

formateddata %>% filter(Country.Name %in% c("Canada","Mexico"))%>% 
  ggplot(aes(x=fecha,y=`CO2 emissions (kt)`/1000000,color=Country.Name))+geom_line()+
  labs(x="",y="Emisiones de CO2 (kt), millones",color="País",title = "Emisiones de CO2 en Norteamérica")+theme_minimal()

formateddata %>% filter(Country.Name %in% c("India","China"))%>% 
  ggplot(aes(x=fecha,y=`CO2 emissions (kt)`/1000000,color=Country.Name))+geom_line()+
  labs(x="",y="Emisiones de CO2 (kt), millones",color="País",title = "Emisiones de CO2 en resto del mundo")+theme_minimal()

# PLOTS ELECTRI POWER
formateddata %>% filter(Country.Name %in% c("Peru","Chile"))%>% 
  ggplot(aes(x=fecha,y=`Electric power consumption (kWh per capita)`/1000,color=Country.Name))+geom_line()+
  labs(x="",y="KWh en miles",color="País",title = "Consumo eléctrico per cápita en Sudamérica")+theme_minimal()

formateddata %>% filter(Country.Name %in% c("Canada","Mexico"))%>% 
  ggplot(aes(x=fecha,y=`Electric power consumption (kWh per capita)`/1000,color=Country.Name))+geom_line()+
  labs(x="",y="KWh en miles",color="País",title = "Consumo eléctrico per cápita en Norteamérica")+theme_minimal()

formateddata %>% filter(Country.Name %in% c("India","China"))%>% 
  ggplot(aes(x=fecha,y=`Electric power consumption (kWh per capita)`/1000,color=Country.Name))+geom_line()+
  labs(x="",y="KWh en miles",color="País",title = "Consumo eléctrico per cápita en resto del mundo")+theme_minimal()

# correlation between variables 
# log(electricconsumption vs co2 emission)
formateddata %>% filter(Country.Name %in% c("Canada","Mexico","Peru","Chile","China","India"))%>% 
  ggplot(aes(x=log(`Electric power consumption (kWh per capita)`),y=log(`CO2 emissions (kt)`),color=Country.Name))+
  geom_point()+theme_minimal()+labs(x="Consumo eléctrico (KWh per cápita)",y="CO2 total (kt)")

# log(total population vs co2 emission)
formateddata %>% filter(Country.Name %in% c("Canada","Mexico","Peru","Chile","China","India"))%>% 
  ggplot(aes(x=log(`Population, total`),y=log(`CO2 emissions (kt)`),color=Country.Name))+
  geom_point()+theme_minimal()+labs(x="Población total",y="CO2 total (kt)")

# log(total population vs co2 emission)
formateddata %>% filter(Country.Name %in% c("Canada","Mexico","Peru","Chile","China","India"))%>% 
  ggplot(aes(x=log(`CO2 emissions from liquid fuel consumption (kt)`),y=log(`CO2 emissions (kt)`),color=Country.Name))+
  geom_point()+theme_minimal()+labs(x="CO2 de consumo liquido de combustibles (kt)",y="CO2 total (kt)")


cor.test(formateddata$`Population, total`,y=formateddata$`CO2 emissions (kt)`)
unique(co2$Indicator.Name)
#---------------------------------- another code ----------------------------------#
# aruba<-co2 %>% filter(Country.Code== c("IDN","PER"))
# str(aruba)
# # creating a tidy data
# indicadoresaruba<-aruba %>% select(!c("Country.Code","Country.Name","Indicator.Code"))%>%
#   pivot_longer(!Indicator.Name,names_to = "fecha",values_to = "valor")
# # formatting dates
# indicadoresaruba$fecha<-substr(indicadoresaruba$fecha,2,5)
# indicadoresaruba$fecha<-ymd(indicadoresaruba$fecha, truncated = 2L)
# # plotting or at least trying
# unique(indicadoresaruba$Indicator.Name)
# # crecimiento de la población para aruba desde 1960
# indicadoresaruba %>% filter(Indicator.Name=="Population, total") %>%
#   ggplot(aes(x=fecha,y=valor/1000))+geom_line()+labs(x="",y="Población en miles")+theme_light()
# 
# # graph line for all interesting variables for aruba
# indicadoresaruba %>%
#   filter(Indicator.Name %in% c("Population, total",
#                                "Nitrous oxide emissions (thousand metric tons of CO2 equivalent)",
#                                "CO2 emissions from liquid fuel consumption (kt)",
#                                "Forest area (sq. km)",
#                                "Agricultural land (sq. km)")) %>%
#   ggplot(aes(x=fecha,y=valor/1000,color=Indicator.Name))+geom_line()+labs(x="",y="Población en miles")+theme_light()
# 
# # EMISION DE CO2 POR CONSUMO DE COMBUSTIBLES LIQUIDOS
# indicadoresaruba %>%
#   filter(Indicator.Name %in% c(
#                                
#                                "CO2 emissions from liquid fuel consumption (kt)")) %>%
#   ggplot(aes(x=fecha,y=valor/1000,color=Indicator.Name))+geom_line()+labs(x="",y="miles")+theme_light()
# 
# 
# indicadoresaruba %>%
#   filter(Indicator.Name %in% c(
#     "Nitrous oxide emissions (thousand metric tons of CO2 equivalent)",
#     "Forest area (sq. km)",
#     "Agricultural land (sq. km)")) %>%
#   ggplot(aes(x=fecha,y=valor,color=Indicator.Name))+geom_line()+labs(x="",y="")+theme_light()
# 
# unique(co2$Country.Name)


# # WORKING WITH ALL THE DATA
# totaldata<-co2 %>% select(!c("Country.Code","Indicator.Code"))%>%
#   pivot_longer(!c(Indicator.Name, Country.Name),names_to = c("fecha"),values_to = "valor")
# # formatting dates
# totaldata$fecha<-substr(totaldata$fecha,2,5)
# totaldata$fecha<-ymd(totaldata$fecha, truncated = 2L)
# # looking our data
# unique(totaldata$Country.Name)
# unique(totaldata$Indicator.Name)
