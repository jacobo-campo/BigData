########################################################################
########################################################################
##################### BIG DATA y ANALISIS DE DATOS #####################
###################### Tema: Exploración de Datos ######################
########################################################################
########################################################################
# TITANIC

# Librerias
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("corrplot")
install.packages("ggpubr")
install.packages("caret")
install.packages("plotly")
install.packages("jpeg")

# Cargar
library(tidyverse)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(corrplot)
library(ggpubr)
library(caret)
library(plotly)
library(jpeg)

# Directorio
setwd("F:/Clases Memoria/Clases/UCATOLICA/4. Big Data y Análisis de Datos/Clase 5")

# Cargar Datos
df_titanic <- read.csv("titanic3.csv")

head(df_titanic)

str(df_titanic)

summary(df_titanic)

colSums(is.na(df_titanic))

colSums(df_titanic=="")

# Limpieza de Datos
# Si el pasajero tenía familia (sibsp, parch)
df_titanic$family <- 'Sin familia'
df_titanic$family[df_titanic$sibsp>0] <- 'Con Familia'
df_titanic$family[df_titanic$parch>0] <- 'Con Familia'

# Eliminar las columnas name, sibsp, parch, ticket, fare, embarked, boat, body, home.dest
df_titanic <- select(df_titanic, -name)
df_titanic <- select(df_titanic, -sibsp)
df_titanic <- select(df_titanic, -parch)
df_titanic <- select(df_titanic, -ticket)
df_titanic <- select(df_titanic, -fare)
df_titanic <- select(df_titanic, -embarked)
df_titanic <- select(df_titanic, -boat)
df_titanic <- select(df_titanic, -body)
df_titanic <- select(df_titanic, -home.dest)

# Eliminar registros que contengan datos no asignados 
df_titanic <- df_titanic %>% drop_na(pclass)
df_titanic <- df_titanic %>% drop_na(age)

# Visualización por columnas con datos no asignados
colSums(is.na(df_titanic))

# Visualización por columnas con datos vacíos
colSums(df_titanic=="")

# Transformación Columnas
df_titanic$survived[df_titanic$survived==1] <- 'Vivió'
df_titanic$survived[df_titanic$survived==0] <- 'Murió'

df_titanic$pclass[df_titanic$pclass==1] <- 'Primera'
df_titanic$pclass[df_titanic$pclass==2] <- 'Segunda'
df_titanic$pclass[df_titanic$pclass==3] <- 'Tercera'

df_titanic$sex[df_titanic$sex=='male'] <- 'Hombre'
df_titanic$sex[df_titanic$sex=='female'] <- 'Mujer'

df_titanic$cabin <- substr(df_titanic$cabin,1,1)

# Factores a las columnas
df_titanic$pclass <- factor(df_titanic$pclass)
df_titanic$survived <- factor(df_titanic$survived)
df_titanic$sex <- factor(df_titanic$sex)
df_titanic$cabin <- factor(df_titanic$cabin)
df_titanic$family <- factor(df_titanic$family)

df_titanic$age <- round(df_titanic$age, 0)

head(df_titanic)

str(df_titanic)

summary(df_titanic)

#########################
# ANALISIS EXPLORATORIO #
#########################

factor2number <- function(x)
{data.frame(levels(x), 1:length(levels(x)), row.names=1)[x,1]}

df_cat <- df_titanic

df_cat$pclass <- factor2number(df_cat$pclass)
df_cat$survived <- factor2number(df_cat$survived)
df_cat$sex <- factor2number(df_cat$sex)
df_cat$cabin <- factor2number(df_cat$cabin)
df_cat$family <- factor2number(df_cat$family)

str(df_cat)

# Matriz de Correlaciones
mat <- cor(df_cat[,c("survived","pclass","age","sex","cabin","family")]
           , ,use="complete")
corrplot(mat)

############
# Gráficos #
############
# Pasajeros según Clase
pl_df <-
  ggplot(data = df_titanic, mapping = aes(x = pclass, fill = pclass)) +
  geom_bar(color = "black") + 
  scale_fill_manual(values = c("#7A5156", "#F6222E", "#30ff2E")) +
  labs(title ="Pasajeros según Clase") +
  xlab("Clase") + 
  ylab("Cantidad") +
  theme(legend.position = "none")
pl_graf <- ggplotly(pl_df)
pl_graf


# Pasajeros según Sexo
pl_df <-
  ggplot(data = df_titanic, mapping = aes(x = sex, fill = sex)) +
  geom_bar(color = "black") + 
  scale_fill_manual(values = c("#F6222E", "#30ff2E")) +
  labs(title ="Pasajeros según Sexo") +
  xlab("Sexo") + 
  ylab("Cantidad") +
  theme(legend.position = "none")
pl_graf <- ggplotly(pl_df)
pl_graf


# Sobrevivientes por Clase
dfg <- df_titanic %>% 
  group_by(pclass,survived) %>% 
  count()

pl_df <-  
  ggplot(data = dfg, mapping = aes(x = reorder(survived, n), y = n, fill = pclass)) +
  geom_col(color = "#E4E1E3", position = "dodge") +
  scale_fill_manual(values = c("#7A5156", "#F6222E", "#30ff2E")) +
  labs(title ="Sobrevivientes por Clase") +
  xlab("Sobrevivientes") + 
  ylab("Cantidad") +  
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))

pl_graf <- ggplotly(pl_df)
pl_graf

#####
# Densidad de edad vs boxplot de sobrevivencia
p1 <- ggplot(data = df_titanic, aes(x = age, fill = survived)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = survived), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
p2 <- ggplot(data = df_titanic, aes(x = survived, y = age, color = survived)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()

final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("Sobrevivientes / Edad", size = 15))
final_plot

# Densidad de pclass vs boxplot de sobrevivencia
p1 <- ggplot(data = df_titanic, aes(x = pclass, fill = survived)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = survived), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()

p2 <- ggplot(data = df_titanic, aes(x = survived, y = pclass, color = survived)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()

final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("Sobrevivientes / Clase", size = 15))
final_plot

# Densidad de columna sex vs boxplot de sobrevivencia
p1 <- ggplot(data = df_titanic, aes(x = sex, fill = survived)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = survived), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
p2 <- ggplot(data = df_titanic, aes(x = survived, y = sex, color = survived)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("Sobrevivientes / Sexo", size = 15))
final_plot


############################
# Arboles de Clasificacion #
############################

set.seed(1992)
idx <- createDataPartition(y=df_titanic$survived, times=1, p=0.70, list=FALSE)
dataTrain <- df_titanic[idx, ]
dataTest <- df_titanic[idx, ]

# Arbol RPART
mod_arbol0 <- rpart(survived ~ ., data=dataTrain, method="class")
mod_arbol0
rpart.plot(mod_arbol0)


# Por Sexo
set.seed(1992)
idx <- createDataPartition(y = df_titanic$sex, times = 1, p = 0.70, list = FALSE)
dataTrain <- df_titanic[idx, ]
dataTest <- df_titanic[-idx, ]

# Arbol RPART
modÁrbol0 <- rpart(sex ~ ., data = dataTrain, method = "class")
modÁrbol0
rpart.plot(modÁrbol0)


# Por Clase
set.seed(1992)
idx <- createDataPartition(y = df_titanic$pclass, times = 1, p = 0.70, list = FALSE)
dataTrain <- df_titanic[idx, ]
dataTest <- df_titanic[-idx, ]

# Arbol RPART
modÁrbol0 <- rpart(pclass ~ ., data = dataTrain, method = "class")
modÁrbol0
rpart.plot(modÁrbol0)
