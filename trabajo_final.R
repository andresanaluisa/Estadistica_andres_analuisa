
# AUTOR ------------------------------------------------------------------

# NOMBRE: LUIS ANDRES ANALUISA 
# CORREO: and_lui88@hotmail.com
# FECHA: 3101-2023
# TRabajo final estadistcia 

# LIBRERIAS   --------------------------------------------------------------

library(tidyverse)
library(haven)
library(texreg)

options(digits = 5) 
# DATA  -------------------------------------------------------------------
# 
# enemdu2021 <-
#   read_sav(
#     "E:\\DIPLOMADO\\Microeconometría-20230119T154432Z-001\\Microeconometría\\TRABAJO FINAL\\personas_2021_anual.sav"
#   )
# 
# write.table(
#   enemdu2021,
#   file = "enemdu2021.csv",
#   append = FALSE,
#   quote = TRUE,
#   sep = ";",
#   eol = "\n",
#   na = "NA",
#   dec = ".",
#   row.names = TRUE,
#   col.names = TRUE,
#   qmethod = c("escape", "double"),
#   fileEncoding = ""
# )


a <-
  read.csv2(
    "C:\\Users\\Lenovo i7\\Desktop\\EMENDU 2021\\final_\\enemdu2021.csv",
    header = T,
    sep = ";",
    quote = "\"",
    dec = ".",
    fill = TRUE,
    comment.char = ""
  )

# ANALISIS EXPLORATORIO  --------------------------------------------------

# PANORAMA NACIONAL 

# CONSTRUCCION DE POBLACIONES 

# Poblacion en edad de trabajar 

a$pet <-if_else(a$p03>=15,1,0)

# Población Económicamente Inactiva - PEI 

a$pei <-case_when(a$pet==0 & a$p20 ==2 & a$p21 ==12 & a$p22==2 & a$p32 ==11 & a$p34<=7 & a$p35==2~1,
                  a$pet==0 & a$p20 ==2 & a$p21 ==12 & a$p22==2 & a$p32 ==11 & a$p34>=8 & a$p34<=12~1)

# Población Económicamente Activa - PEA  
a$pea <- case_when(a$pet ==1 & a$p20 ==1 ~ 1,
                   a$pet ==1 & a$p20 ==2 & a$p21 <=11~ 1,
                   a$pet ==1 & a$p20 ==2 & a$p21 ==12 & a$p22==1~ 1,
                   a$pet ==1 & a$p20 ==2 & a$p21 ==12 & a$p22==2 & a$p32 <=10~ 1,
                   a$pet ==1 & a$p20 ==2 & a$p21 ==12 & a$p22==2 & a$p32 ==11 & a$p34 <=7 & a$p35==1~ 1)
  

# Población con Empleo

a$empleo <-case_when(
  a$pea ==1 & a$p20 ==1~1,
  a$pea ==1 & a$p20 ==2 & a$p21 <=11~1,
  a$pea ==1 & a$p20 ==2 & a$p21 ==12 & a$p22==1~1)


# Población sin Empleo

# Desempleo abierto

a$des_ab <- case_when(a$pea ==1 & a$p20 ==2 & a$p21 ==12 & a$p22==2 & a$p32 <=10~1)

# Desempleo oculto

a$des_oc <- case_when(a$pea ==1 & a$p20 ==2 & a$p21 ==12 & a$p22==2 & a$p32 ==11 & a$p34 <=7 & a$p35==1~1)

# POblacion con desempleo

a$desempleo <- ifelse((a$des_ab | a$des_oc)==1, 1, 0)

  

# Composición de la  población
library(data.table)

cp <- a %>% summarise(pet= sum(pet*fexp, na.rm = TRUE),
                           pea=sum(pea*fexp,na.rm = TRUE),
                           pei= sum(pei*fexp,na.rm = TRUE),
                           empleo= sum(empleo*fexp,na.rm = TRUE),
                           desempleo= sum(desempleo*fexp,na.rm = TRUE)
                            )
cp               

poblaciones <- c("pet","pea","pei","empleo","desempleo")
cp1 <- t(cp)

cp2 <- data.frame(poblaciones,ceiling(cp1))


# poblaciones 

cp3 <- cp2 %>% filter(poblaciones%in%c("pet","pea","pei"))

ggplot(cp3, aes(x = poblaciones, y = ceiling.cp1.)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ceiling.cp1.), vjust = -1, colour = "black")+ 
  labs(title = "POBLACIONES")

# mercado laboral

cp4 <- cp2 %>% filter(poblaciones%in%c("desempleo","empleo"))
ggplot(cp2, aes(x = poblaciones, y = ceiling.cp1.)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ceiling.cp1.), vjust = -1, colour = "black")+ 
  labs(title = "MERCADO LABORAL ")


# MODELO PROVIT  ----------------------------------------------------------

 # CONTRUCCION DE VARIABLES
 
# Poblacion empelo formal e informal 1 formal 2 informal 3 domestico
library(tidyverse) 
 
pmp <-  a %>% filter(a$secemp < 3) 
`
# informalidad 1 "Informal" 0 "Formal" 
   

# poblacion informal 

table(pmp$informalidad)
informal <- mean(a$fexp)*73089 #informal
formal <- mean(a$fexp)*84650 #formal


pmp$informalidad <- case_when(pmp$secemp == 1 ~ 0,
                               pmp$secemp == 2 ~ 1,
                               pmp$secemp == 3 ~ 0)


pmp$informalidad <- factor(pmp$informalidad, labels = c("formal","informal"))



# sexo: 0 "mujer" 1 "hombre" 
 
pmp$sexo <- case_when(pmp$p02 == 2 ~ 0,
                       pmp$p02 == 1 ~ 1) 

pmp$sexo <- factor(pmp$sexo, labels = c("mujer","hombre"))


prop.table(table(pmp$sexo,pmp$informalidad), margin = 2)

barplot(prop.table(table(pmp$sexo,pmp$informalidad), margin = 2), 
        main = "Informalidad",
        legend.text = c("mujer","hombre"),
        beside = T)


#  Jefe del hogar 1 "Jefe del Hogar" 0 "otro"
 
 pmp$jefeh <- case_when(pmp$p04 == 1 ~ 1,
                        pmp$p04 != 1 ~ 0) 

 pmp$jefeh <- factor(pmp$jefeh, labels = c("otro","jefe"))
 
 prop.table(table(pmp$jefeh,pmp$informalidad), margin = 2)
 
 barplot(prop.table(table(pmp$jefeh,pmp$informalidad), margin = 2), 
         main = "Jefe de hogar",
         legend.text = c("otro","jefe"),
         beside = T)
 
# Ambito geografico  0"Rural" 1"Urbano"

pmp$area1 <- case_when(pmp$area == 1 ~ 1,
                       pmp$area == 2 ~ 0,)

pmp$area1 <- factor(pmp$area1, labels = c("urbano","rural"))

prop.table(table(pmp$area1,pmp$informalidad), margin = 2)

barplot(prop.table(table(pmp$area1,pmp$informalidad), margin = 2), 
        main = "Area(urbano-rural)",
        legend.text = c("urbano","rural"),
        beside = T)

# Autopercepción étnica 
# 0 "Blanco y meztizo" 1 "Indígena, afro, negro, montubio y otro"

pmp$etnia <- case_when(
  pmp$p15 == 1 ~ 1,
  pmp$p15 == 2 ~ 1,
  pmp$p15 == 3 ~ 1,
  pmp$p15 == 4 ~ 1,
  pmp$p15 == 5 ~ 1,
  pmp$p15 == 6 ~ 0,
  pmp$p15 == 7 ~ 0,
  pmp$p15 == 8 ~ 1,
)

pmp$etnia <- factor(pmp$etnia, labels = c("Blanco y meztizo","indígena,afro,negro,montubio y otro"))

prop.table(table(pmp$etnia,pmp$informalidad), margin = 2)

barplot(prop.table(table(pmp$etnia,pmp$informalidad), margin = 2), 
        main = "Etnia",
        legend.text = c("Blanco y meztizo","indígena,afro,negro,montubio y otro"),
        beside = T)


# *Dominio geógrafico
# 1"Costa" 2 "Sierra" 3 "Oriente"4 "Galapagos"


pmp$region <- case_when(
  pmp$prov %in% c(7, 8, 9) ~ 1,
  pmp$prov %in% c(1:6, 10:11, 17:18) ~ 2,
  pmp$prov %in% c(14:16, 21:22, 19) ~ 3,
  pmp$prov == 20 ~ 4
) 

pmp$region <- factor(pmp$region, labels = c("Costa","Sierra", "Oriente", "Galapagos"))

prop.table(table(pmp$region,pmp$informalidad), margin = 2)

barplot(prop.table(table(pmp$region,pmp$informalidad), margin = 2), 
        main = "Region",
        legend.text = c("Costa","Sierra", "Oriente", "Galapagos"),
        beside = T)

# Rama de actividad
# 1 "Extractiva"2 "Industria"3 "Construccion"
# 4"Comercio"5 "Servicios"
  
pmp$rama <- case_when (
  pmp$rama1 %in% c(1, 2) ~ 1,
  pmp$rama1 == 3 ~ 2,
  pmp$rama1 == 6 ~ 3,
  pmp$rama1 == 7 ~ 4,
  pmp$rama1 %in% c(4:5, 8:21) ~ 5
)
                       
pmp$rama <- factor(pmp$rama, labels = c("Extractiva","Industria", "COnstruccion", "Comercio", "Servicios"))


prop.table(table(pmp$rama,pmp$informalidad), margin = 2)

barplot(prop.table(table(pmp$rama,pmp$informalidad), margin = 2), 
        main = "Actividad",
        legend.text = c("Extractiva","Industria", "COnstruccion", "Comercio", "Servicios"),
        beside = T)

# Ingreso laboral mensual (ocupaciones principal y secundarias)

z <-select(pmp, "p63",
                     "p66",
                     "p68b",
                     "p69",
                     "p71b",
                     "p72b",
                     "p73b",
                     "p74b",
                     "p76",
                     "p78"
           )
z[z == "999999"] <- 0
z[z == NA] <- 0
z[z == NaN] <- 0

pmp$salariom <- rowSums(z, na.rm = T)


pmp %>% group_by(informalidad) %>% summarise(sueldos = mean(salariom))



# Años de educación
# Homologacion realizada de acuerdo a formulario

  
pmp$educacion <-   case_when(
  pmp$p10a == 0 ~ pmp$p10b,
  pmp$p10a %in% c(1, 2, 4, 5) ~ pmp$p10b,
  pmp$p10a == 6 ~ pmp$p10b + 7,
  pmp$p10a == 7 ~ pmp$p10b + 10,
  pmp$p10a %in% c(8, 9) ~ pmp$p10b + 13,
  pmp$p10a == 10 ~ pmp$p10b + 18
)


a_edu <- prop.table(table(pmp$educacion,pmp$informalidad), margin = 2)

write.table(a_edu, file = "a_edu.csv", append = FALSE, quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ",", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
a_edu 

barplot(a_edu[,1], 
        main = "Anio de Eduacion Formal")
        

barplot(a_edu[,1], 
        main = "Anio de Eduacion Informal")

# * Experiencia


pmp$exper <-  pmp$p45


a_exper <- prop.table(table(pmp$exper,pmp$informalidad), margin = 2)

write.table(a_exper, file = "a_exper.csv", append = FALSE, quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ",", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
a_exper 

barplot(a_exper[,1], 
        main = "Anio de Exper Formal")


barplot(a_edu[,1], 
        main = "Anio de Exper Informal")

# Experincia la cuadrado
 
pmp$exper2 <-  pmp$p45^2
  
# base modelo

l <-pmp %>% 
  select(
    "informalidad",
    "sexo",
    "jefeh",
    "area",
    "etnia",
    "region",
    "rama",
    "salariom",
    "educacion",
    "exper",
    "exper2") 




# MODELADO ----------------------------------------------------------------

# Modelo

library(texreg)
library(tidyverse)

# modelo

model_1 <- glm(informalidad ~ .,
               data= l, 
               family = binomial("logit")
               ) 


summary(modelo.rpart)

# resumen

summary(model_1)

# odds ratios

screenreg(model_1,
          custom.model.names = "Modelo 1 - Odds Ratios",
          override.coef    = exp(coef(model_1)),
          # además, omitiremos el coeficiente del intercepto
          omit.coef = "Inter")




# POST ESTIMCION   --------------------------------------------------------


# Ro

library(pscl)

pR2(model_1)[["McFadden"]]

# matriz de confucin 
model_1

library(tidyverse)
library(vcd)

predicciones <- ifelse(test = model_1$fitted.values > 0.5, yes = 1, no = 0)

matriz_confusion <- table(model_1$model$informalidad, predicciones,
                          dnn = c("observaciones", "predicciones"))


mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))
1+1
(65905+42093)/(65905+42093+19228+12074)

# EL modelo tiene una efectividad del 77,52% para predecir la informalidad. 
# ANOVA -------------------------------------------------------------------

library(tidyverse)

an <- l %>% select("region","salariom") 


# medas
tapply(an$salariom, an$region, mean)

# normalidad

ks.test(an$region, "pnorm")
ks.test(an$salariom, "pnorm")

# Tenemos pruebas suficientes para decir que los dos conjuntos de datos de muestra no provienen de la misma distribución. 


#  Grafico 

# grafico
Boxplot(an$salariom ~ an$region)  

# Analicis de varianza test de levin 

# Hipótesis nula (H 0 ) : La varianza entre los grupos es igual.
# Hipótesis alternativa (H A ) : La varianza entre los grupos no es igual


# nomocedasticidad 
library(car)
leveneTest(an$salariom ~ an$region)

# El valor p de la prueba es 7.865e-11 , que es menor que nuestro nivel de significancia de 0.05. 
# Por lo tanto, rechazamos la hipótesis nula y concluimos que la varianza entre los cautro grupos no es igual.



# COntraste de comparacion de medias 
an$region <- as.numeric(an$region)
an$salariom <- as.numeric(an$salariom)


summary(aov(an$salariom~an$region ))

# F VAlue es superior al detereminado por el ivel de confianza 
# rechazadmos H0: m1=m2=m3=m4 y determinamos que existe evidencia estadistica para 
# concluir que la media de salarios es diferente en cada region.

