########################################################################
########################################################################
##################### BIG DATA y ANALISIS DE DATOS #####################
###################### Tema: Exploración de Datos ######################
########################################################################
########################################################################
# Ejemplo Bihar y USA

# Installar Paquetes
install.packages("tidyverse") 
install.packages("dplyr") # Limpieza de datos 
install.packages("devtools") 
install.packages("ggplot2") # Visualización de gráficos con ggplot
install.packages("csvread") # Formado csv lectura 
install.packages("cowplot")

# Cargar Paquetes
library(tidyverse)
library(dplyr)
library(devtools)
library(ggplot2)
library(csvread)
library(cowplot)

# Definir directorio de trabajo
setwd("C:/Users/Lenovo/OneDrive - Departamento Nacional de Planeacion/Desktop/Exploración de Datos")

# Cargar y describir Datos
bihar_data <- read.csv("Bihar_sample_data.csv")
str(bihar_data)

summary(bihar_data)

# Mantenemos solo los adultos femeninos
bihar_adult_females <- filter(bihar_data, adult==1, female==1)

bihar_adult_females_trunc <- filter(bihar_data, adult == 1, female == 1, height_cm>120 , height_cm<200)

# Histograma
ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_histogram(fill="blue", color="darkblue")+
  xlab("Altura en  Centímetro, Bihar Females")

# Jugamos con los contenedores (grupos de rango)
bihar1 <- ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_histogram(fill="blue", color="darkblue", binwidth = 5)+
  xlab("bin width=5")+
  ylab("")

bihar2 <- ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_histogram(fill="blue", color="darkblue", binwidth = 10)+
  xlab("bin width=10")+
  ylab("")

bihar3 <- ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_histogram(fill="blue", color="darkblue", binwidth = 20)+
  xlab("bin width=20")+
  ylab("")

bihar4 <- ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_histogram(fill="blue", color="darkblue", binwidth = 50)+
  xlab("bin width=50")+
  ylab("")

plot_grid(bihar1, bihar2, bihar3, bihar4, labels="Altura de Mujeres en Bihar", hjust=-1, vjust=0.2)

#############################
# Data "US_sample_data.csv" #
#############################

us_data <- read.csv("US_sample_data.csv")

us_adult_females_trunc <- filter(us_data,female==1 , adult==1, height_cm>120 , height_cm<200)

# Histograma

ggplot(us_adult_females_trunc, aes(height_cm))+
  geom_histogram(fill="red", color="darkred")+
  xlab("Height in centimeters, US females")

# Kernel
# La estimación de la densidad del kernel es una forma no paramétrica de estimar
# la función de densidad de probabilidad de una variable aleatoria.

ggplot(us_adult_females_trunc, aes(height_cm))+
  geom_histogram(data=us_adult_females_trunc, aes(height_cm , ..density..), fill="white" , color="darkred")+
  geom_density(kernel="gaussian", aes(height_cm))

# Jugamos con los contenedores (grupos de rango)
US1 <- ggplot(us_adult_females_trunc, aes(height_cm))+
  geom_histogram(data=us_adult_females_trunc, aes(height_cm , ..density..), fill="white" , color="darkred")+
  geom_density(kernel="gaussian", aes(height_cm), bw=1)+
  xlab("bw=1")+
  ylab("")

US2 <- ggplot(us_adult_females_trunc, aes(height_cm))+
  geom_histogram(data=us_adult_females_trunc, aes(height_cm , ..density..), fill="white" , color="darkred")+
  geom_density(kernel="gaussian", aes(height_cm), bw=5)+
  xlab("bw=5")+
  ylab("")

US3 <- ggplot(us_adult_females_trunc, aes(height_cm))+
  geom_histogram(data=us_adult_females_trunc, aes(height_cm , ..density..), fill="white" , color="darkred")+
  geom_density(kernel="gaussian", aes(height_cm), bw=10)+
  xlab("bw=10")+
  ylab("")

US4 <- ggplot(us_adult_females_trunc, aes(height_cm))+
  geom_histogram(data=us_adult_females_trunc, aes(height_cm , ..density..), fill="white" , color="darkred")+
  geom_density(kernel="gaussian", aes(height_cm), bw=20)+
  xlab("bw=20")+
  ylab("")

plot_grid(US1, US2, US3, US4, labels="Female Height in the US", hjust=-1, vjust=0.2)

# Ahora combinamos los dos histogramas
ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_histogram(data=bihar_adult_females_trunc, aes(height_cm),fill="blue", color="darkblue" )+
  geom_histogram(data=us_adult_females_trunc, aes(height_cm), fill="red", color="darkred" )

# No tiene sentido así, entonces no contemos sino que usemos la densidad
ggplot() +
  geom_histogram(data = bihar_adult_females_trunc, aes(x = height_cm, y = ..density..), 
                 fill = "blue", color = "darkblue", alpha = 0.5, bins = 30) +
  geom_histogram(data = us_adult_females_trunc, aes(x = height_cm, y = ..density..), 
                 fill = "red", color = "darkred", alpha = 0.5, bins = 30) +
  labs(title = "Distribución de alturas de mujeres adultas (Bihar vs. EE.UU.)",
       x = "Altura (cm)", y = "Densidad") +
  theme_minimal()

# Hacemos más visibles los puntos.
ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_freqpoly(data=bihar_adult_females_trunc, aes(height_cm, ..density.. ), color="darkblue" )+
  geom_freqpoly(data=us_adult_females_trunc, aes(height_cm , ..density..),  color="darkred" )+
  xlab("Height in centimeters")

ggplot(bihar_adult_females_trunc, aes(height_cm))+
  geom_density(data=bihar_adult_females_trunc, aes(height_cm), color="darkblue" )+
  geom_density(data=us_adult_females_trunc, aes(height_cm),  color="darkred" )+
  xlab("Height in centimeters")

# Representamos el CDF
ggplot() +
  stat_ecdf(data = bihar_adult_females_trunc, aes(x = height_cm), 
            color = "darkblue", size = 1) +
  stat_ecdf(data = us_adult_females_trunc, aes(x = height_cm), 
            color = "darkred", size = 1) +
  labs(title = "Distribución acumulada de alturas de mujeres adultas",
       x = "Altura en centímetros",
       y = "Probabilidad acumulada") +
  theme_minimal()
