########################################################################
########################################################################
##################### BIG DATA y ANALISIS DE DATOS #####################
###################### Tema: Exploración de Datos ######################
########################################################################
########################################################################
# Ejemplo Covid

# Installar Paquetes
install.packages("readxl") 
install.packages("tidyverse")  
install.packages("dplyr") 
install.packages("lubridate") 
install.packages("reactable") 
install.packages("hrbrthemes")
install.packages("reshape2") 
install.packages("reshape")  
install.packages("survival") 
install.packages("ggplot2") 
install.packages("ggsurvfit") 
install.packages("downloader")

# Cargar Paquetes
library(readxl)         #Leer documentos de Excel
library(tidyverse)      #Analizar datos
library(dplyr)          #Analizar datos
library(lubridate)      #Trabajar con fechas
library(reactable)      #Cuadros interactivos
library(hrbrthemes)     #Temas de ggplot
library(reshape2)       #Trasponer1
library(reshape)        #Trasponer2
library(survival)       #Análisis de supervivencia1
library(ggsurvfit)      #Análisis de supervivencia2
library(downloader)     #Descargar archivos

# Definir directorio de trabajo
setwd("C:/Users/Lenovo/OneDrive - Departamento Nacional de Planeacion/Desktop/Exploración de Datos")

# Declarar el nombre de los datos
destfile <- "covid_example_data.xlsx"

# Importar la base de datos
data <- read_excel(destfile)

# Filtrar datos
# Seleccionar variables de interés
data2 <- select(data,                   #base de datos
                PID,                    #id
                died,                   #Murió
                pos_sampledt_FALSE ,    #Fecha de prueba positiva de COVID
                died_dt_FALSE,          #Fecha de muerte
                case_age,               #Edad
                case_gender)            #Sexo

# Limpiar variables
# Declarar variables como fechas
data2$pos_sampledt_FALSE<-ymd(data2$pos_sampledt_FALSE)
data2$died_dt_FALSE<-ymd(data2$died_dt_FALSE)

#Convertir variables de character a factor
data2[, c("case_gender")] <- lapply(data2[, c("case_gender")], factor)

#Limpiar variable sexo (NA's -> Unknown)
data2$case_gender <- replace(data2$case_gender, 
                             is.na(data2$case_gender) , "Unknown")


#Agrupar la variable de edad en categorías
data2$case_age[data2$case_age==-20]<-0
data2<-mutate(data2,case_age2=case_when(case_age<=14 ~ "a0_14",
                                        case_age>=15 & case_age<=24 ~"a15_24",
                                        case_age>=25 & case_age<=59~ "a25_59",
                                        case_age>=60 ~ "a60mas",
                                        TRUE~"Unknown"))

###############################################
# Casos confirmados de de SARS-CoV-2 por sexo #
###############################################
# Contar el número de casos semanales por sexo
data2 %>%
  group_by(week = floor_date(pos_sampledt_FALSE, unit = "week"), case_gender) %>%
  count() %>%
  cast(week ~ case_gender) %>%
  replace_na(list(Female = 0, Male = 0, Unknown = 0)) %>%
  mutate(Total = rowSums(select(., Female, Male, Unknown))) -> tab1

# Generar cuadro dinámico
reactable(tab1)

# Trasponer la base de datos para graficar
tab1 %>%
  pivot_longer(cols = c(Female, Male, Unknown,Total ), names_to = "case_gender", values_to = "n") -> tab1

# Graficar la incidencia por semana
ggplot(tab1,aes(x=week,y=n,color=case_gender))+
  geom_line()+
  geom_point()+
  scale_color_discrete(breaks = c("Female", "Male", "Unknown","Total"))+
  scale_x_date(breaks = "1 month",
               date_labels = "%Y-%m")+
  labs(x="Fecha (año-mes)",y="Número absoluto de casos (n)",
       title="Figura 1. Casos registrados de SARS-CoV-2 por sexo",
       subtitle = "Del 2019 al 2021",
       color="Sexo")+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 90))

#####################################################
# Casos confirmados de SARS-CoV-2 por grupo de edad #
#####################################################
# Contar el número de casos semanales por sexo
data2 %>%
  group_by(week = floor_date(pos_sampledt_FALSE, unit = "week"), case_age2) %>%
  count() %>%
  cast(week ~ case_age2) %>%
  replace_na(list(a0_14 = 0, a15_24 = 0, a25_59 = 0, a60mas = 0)) %>%
  mutate(Total = rowSums(select(., a0_14, a15_24, a25_59,a60mas))) -> tab2

# Generar cuadro dinámico
reactable(tab2)

# Trasponer la base de datos para graficar
tab2 %>%
  pivot_longer(cols = c(a0_14, a15_24, a25_59,a60mas, Unknown ,Total), names_to = "case_age2", values_to = "n") -> tab2

# Graficar la incidencia por semana
ggplot(tab2,aes(x=week,y=n,color=case_age2))+
  geom_line()+
  geom_point()+
  scale_color_discrete(breaks = c("a0_14", "a15_24","a16_24", "a25_59","a60mas","Unknown","Total"))+
  scale_x_date(breaks = "1 month",
               date_labels = "%Y-%m")+
  labs(x="Fecha (año-mes)",y="Número absoluto de casos (n)",
       title="Figura 2. Casos registrados de SARS-CoV-2 grupos de edad",
       subtitle = "Del 2019 al 2021",
       color="Grupos de edad")+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 90))

#############################
# Análisis de supervivencia #
#############################
# Crear el tiempo de inicio y término de seguimiento
data2<-data2 %>% 
  mutate(start= ymd(pos_sampledt_FALSE), 
         end= ymd(died_dt_FALSE))

# Crear variable de evento (muerte)
data2$event <- ifelse(!is.na(data2$end), 1, 0)

# Asignar a los que no murieron el tope de la ventana de observación
data2$end <- replace(data2$end,
                     is.na(data2$end) , "2021-08-01")


# Calcular el número de semanas entre la fecha de inicio y final
data2<-data2 %>% 
  mutate(time=interval(start, end) / days(1)+1)

data2$time<-data2$time/7

# Eliminar casos que murieron antes de ser diagnosticados
data3<-filter(data2,time>=1 )


# Estimar la curva de supervivencia
survfit(Surv(time, event) ~ 1, data = data3)%>%
  ggsurvfit(color="salmon") +
  labs(x = "Semanas", y = "Probabilidad de supervivencia",
       title = "Figura 3. Diagrama de Kaplan-Meier") + 
  add_confidence_interval()+ 
  add_risktable()+
  theme_ipsum()+
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,78))
