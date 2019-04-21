# Install packages
install.packages("wesanderson")
install.packages(readr)
install.packages("viridis")
library (tidyverse)
library (readxl)
library(RColorBrewer)
library(viridis)  
library(plotly)
library(wesanderson)

# ---------------------------------------------------------------------------------------------------------------- 
# Lectura del dataset 1
tiempo_pantalla <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/tiempo_pantalla.csv")
View(tiempo_pantalla)
# ---------------------------------------------------------------------------------------------------------------- 
#limpieza inicial
tiempo_pantallaSinNA <-na.omit(tiempo_pantalla)
View(tiempo_pantallaSinNA)
tiempo_pantallalimpio <-na.omit(tiempo_pantalla)
View(tiempo_pantallalimpio)
#procesamiento
# filtro los primeros 10 personajes con mmás minutos en pantalla
primeros10 <- filter(tiempo_pantallaSinNA, (tiempo_pantallaSinNA$minutos_pantalla>103))
View(primeros10)
#ver
#ordeno <-tiempo_pantallaSinNA %>% order_by(tiempo_pantallaSinNA,ascendente) 
# ---------------------------------------------------------------------------------------------------------------- 
#ggplot - Gráfico original
# ---------------------------------------------------------------------------------------------------------------- 
p<- ggplot(data=primeros10, aes(x=nombre, y=minutos_pantalla), colour=nombre) + geom_col(aes(fill=nombre))
p +  labs(x = "Personajes", y = "Minutos en Pantalla", colour = " Personajes", 
          title = "Personajes con más minutos en pantalla",
          subtitle = "Temporada de la 1 a las 6")+  # xlab=""
  theme_classic()+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))+ #los meses se visualizan en forma vertical en el eje x
  theme(plot.title = element_text(family="Courier",
                                  size=rel(1),        #Tama?o relativo de la letra del t?tulo
                                  vjust=2,            #Justificaci?n vertical, para separarlo del gr?fico
                                  position_identity(center),   
                                  face="bold",        #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                  color="black", #Color del t?tulo: maroon, lightblue, lightblue,darkblue, darkorange, black.
                                  lineheight=2.0)) 

# leyenda modificada
p<- ggplot(data=primeros10, aes(x=nombre, y=minutos_pantalla), colour=nombre) + geom_col(aes(fill=nombre))
p +  labs(x = "Personajes", y = "Minutos en Pantalla", colour = " Personajes", 
          title = "Personajes con más minutos en pantalla",
          subtitle = "Temporada de la 1 a las 6")+  # xlab=""
  theme_classic()+
  theme (axis.text.x = element_blank())+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))+  #los meses se visualizan en forma vertical en el eje x
  theme(plot.title = element_text(family="Courier",
                                  size=rel(1),        #Tama?o relativo de la letra del t?tulo
                                  vjust=2,            #Justificaci?n vertical, para separarlo del gr?fico
                                  position_identity(center),   
                                  face="bold",        #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                  color="black", #Color del t?tulo: maroon, lightblue, lightblue,darkblue, darkorange, black.
                                  lineheight=2.0)) + scale_fill_discrete(name="Personajes")
# ---------------------------------------------------------------------------------------------------------------- 
# ---------------------------------------------------------------------------------------------------------------- 
#ggplot - Correcto para publicar con más minutos en pantalla
# ---------------------------------------------------------------------------------------------------------------- 
p<- ggplot(data=primeros10, aes(x=nombre, y=minutos_pantalla), colour=nombre) + geom_col(aes(fill=nombre))
p +  labs(x = "Personajes", y = "Minutos en Pantalla", colour = " Personajes", 
          title = "Personajes con más minutos en pantalla",
          subtitle = "Temporada de 1 a 6")+  # xlab=""
  theme_classic()+
  theme (axis.text.x = element_blank())+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))+  #los meses se visualizan en forma vertical en el eje x
  theme(plot.title = element_text(family="Courier",
                                  size=rel(1),        #Tama?o relativo de la letra del t?tulo
                                  vjust=2,            #Justificaci?n vertical, para separarlo del gr?fico
                                  position_identity(center),   
                                  face="bold",        #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                  color="black", #Color del t?tulo: maroon, lightblue, lightblue,darkblue, darkorange, black.
                                  lineheight=2.0),  legend.position= "", legend.text = element_blank())
# ---------------------------------------------------------------------------------------------------------------- 
#ggplot - Correcto para publicar con menos minutos en pantalla con paleta Darjeeling1
# ---------------------------------------------------------------------------------------------------------------- 
# con paleta de con paleta de WesAnderson
pal <- wes_palette("Darjeeling1", 10, type = "continuous")
# filtro los ultimos 10
tail(tiempo_pantallaSinNA)
ultimos10 <- filter(tiempo_pantallaSinNA, (tiempo_pantallaSinNA$minutos_pantalla<2.30))
View(ultimos10)
# ---------------------------------------------------------------------------------------------------------------- 
p<- ggplot(data=ultimos10, aes(x=nombre, y=minutos_pantalla), colour=nombre) + geom_col(aes(fill=nombre))
p +  labs(x = "Personajes", y = "Tiempo (min)", colour = " Personajes", 
          title = "Personajes con menos minutos en pantalla",
          subtitle = "Temporada de 1 a 6")+ 
  scale_fill_manual(values=pal)+ scale_x_discrete(limits=c("Ilyn Payne","Ternesio Terys","Quaithe", "Hugh of the Vale", "Rorge",
                                                             "Tickler", "Mhaegen","Biter", "Lhara", "Tobho Mott"))+
  theme_minimal()+
  theme (axis.text.x = element_blank())+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))+  #los meses se visualizan en forma vertical en el eje x
  theme(plot.title = element_text(family="Courier",
                                  size=rel(1.5),        #Tama?o relativo de la letra del t?tulo
                                  vjust=2,            #Justificaci?n vertical, para separarlo del gr?fico
                                  position_identity(center),   
                                  face="bold",        #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                  color="black", #Color del t?tulo: maroon, lightblue, lightblue,darkblue, darkorange, black.
                                  lineheight=2.0),  legend.position= "", legend.text = element_blank())
# ---------------------------------------------------------------------------------------------------------------- 
#ggplot - Correcto para publicar con menos minutos en pantalla con paleta color_brewer
# ----------------------------------------------------------------------------------------------------------------
#opcion 2 con scale_color_brewer(palette = "Dark2")
p<- ggplot(data=ultimos10, aes(x=nombre, y=minutos_pantalla), colour=nombre) + geom_col(aes(fill=nombre))
p +  labs(x = "Personajes", y = "Tiempo (min)", colour = " Personajes", 
          title = "Personajes con menos minutos en pantalla",
          subtitle = "Temporada de 1 a 6")+ 
  scale_color_brewer(palette = "Dark2")+ scale_x_discrete(limits=c("Ilyn Payne","Ternesio Terys","Quaithe", "Hugh of the Vale", "Rorge",
                                                           "Tickler", "Mhaegen","Biter", "Lhara", "Tobho Mott"))+
  theme_minimal()+
  theme (axis.text.x = element_blank())+
  theme(axis.text.x = element_text(angle = 50, vjust = 1.1, hjust=1, color="black"))+  #los meses se visualizan en forma vertical en el eje x
  theme(plot.title = element_text(family="Courier",
                                  size=rel(1.5),        #Tama?o relativo de la letra del t?tulo
                                  vjust=2, 
                                  hjust= 0.5,     #Justificación del título, permite centrarlo
                                  position_identity(center),   
                                  face="bold",        #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                  color="black", #Color del t?tulo: maroon, lightblue, lightblue,darkblue, darkorange, black.
                                  lineheight=2.0),  legend.position= "", legend.text = element_blank())
# ---------------------------------------------------------------------------------------------------------------- 
#ggplot - Correcto para publicar con más minutos en pantalla, con paleta Rushmore1 de WesAnderson
# ---------------------------------------------------------------------------------------------------------------- 
pal <- wes_palette("Rushmore1", 10, type = "continuous")  #preferidos: Rushmore1,GrandBudapest1, Darjeeling2
p<- ggplot(data=primeros10, aes(x=nombre, y=minutos_pantalla), colour=nombre) + geom_col(aes(fill=nombre))
p +  labs(x = "Personajes", y = "Tiempo (min)", colour = " Personajes", 
          title = "Personajes con más minutos en pantalla",
          subtitle = "Temporada de 1 a 6")+
  scale_fill_manual(values=pal)+ scale_x_discrete(limits=c("Tyrion Lannister","Jon Nieve","Daenerys Targaryen", "Cersei Lannister", "Sansa Stark", "Arya Stark",
                                                           "Jaime Lannister","Theon Greyjoy", "Samwell Tarly", "Jorah Mormont"))+
  theme_minimal()+
  #theme (axis.text.x = element_blank())
  theme(axis.text.x = element_text(angle = 50, vjust = 1.1, hjust=1, color="black"))+ #color="darkgrey /los meses se visualizan en forma vertical en el eje x
  theme(plot.title = element_text(family="Courier",
                                  size=rel(1.5),        #Tama?o relativo de la letra del t?tulo
                                  vjust=2, 
                                  hjust= 0.5, #Justificaci?n vertical, para separarlo del gr?fico
                                  position_identity(center),   
                                  face="bold",        #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                  color="darkred", #Color del t?tulo: maroon, lightblue, lightblue,darkblue, darkorange, black.
                                  lineheight=2.0), legend.position= "", legend.text = element_blank())

# ---------------------------------------------------------------------------------------------------------------- 
# Lectura del dataset 2
personajes_libros <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/personajes_libro.csv")
View(personajes_libros)
genero <- as.factor(personajes_libros$genero)
View(genero)
# agrupa por lealtad y cuenta la cantidad de hombres y mujeres
generoporlealtad <-personajes_libros %>% group_by(lealtad) %>%                 
count(genero)
View (prueba)

noblesporlealtad <-personajes_libros %>% group_by(lealtad) %>%                 
  count(noble)
View (noblesporlealtad)

# arrange para ordenar
gptres <- gpdos %>% 
  group_by(provincia)  %>% 
  count(generoVictima)%>% 


totalesXtemporada<-tiempo_pantalla %>%                 #Sumarizo total por concepto para el período 2016-2018
  group_by(nombre) %>%              
  summarise(totalminutos=sum(minutos_pantalla)) %>%  
  order_by(totalminutos)
View(totalesXtemporada)  


cambio_lealtades <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/cambio_lealtades.csv")
View (cambio_lealtades)
personajes_libros <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/personajes_libro.csv")
View(personajes_libros)