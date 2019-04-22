# Install packages
install.packages("wesanderson")
install.packages(readr)
#install.packages("viridis")
#install.packages("highcharter")
#library(highcharter)
library (tidyverse)
library (readxl)
library(RColorBrewer)
library(viridis)  
library(plotly)
library(wesanderson)

# ---------------------------------------------------------------------------------------------------------------- 
# Lectura del dataset 1: tiempo_pantalla
tiempo_pantalla <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/tiempo_pantalla.csv")
View(tiempo_pantalla)
# ---------------------------------------------------------------------------------------------------------------- 
#limpieza inicial del dataset 1
tiempo_pantallaSinNA <-na.omit(tiempo_pantalla)
View(tiempo_pantallaSinNA)
tiempo_pantallalimpio <-na.omit(tiempo_pantalla)
View(tiempo_pantallalimpio)
#procesamiento
# filtro los primeros 10 personajes con mmás minutos en pantalla
primeros10 <- filter(tiempo_pantallaSinNA, (tiempo_pantallaSinNA$minutos_pantalla>103))
View(primeros10)
#ver cómo hacer el ordenamiento
#ordeno <-tiempo_pantallaSinNA %>% order_by(tiempo_pantallaSinNA,ascendente) 
# ---------------------------------------------------------------------------------------------------------------- 
#ggplot - Exploración de datos: primer gráfico
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
# ggplot - Correcto para publicar con mayor cantidad de minutos en pantalla con colores por defecto
# ---------------------------------------------------------------------------------------------------------------- 
p<- ggplot(data=primeros10, aes(x=nombre, y=minutos_pantalla), colour=nombre) + geom_col(aes(fill=nombre))
p +  labs(x = "Personajes", y = "Minutos en Pantalla", colour = " Personajes", 
          title = "Personajes con mayor cantidad de minutos en pantalla",
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
#  Procesamiento previo para del segundo gráfico: menor cantidad de minutos en pantalla
# ---------------------------------------------------------------------------------------------------------------- 
#Verifico los últimos valores del dataset
tail(tiempo_pantallaSinNA)
#ultimos <- top_n(1,10,minutos_pantalla) %>% order_by(minutos_pantalla)
# filtro los ultimos 10
ultimos10 <- filter(tiempo_pantallaSinNA, (tiempo_pantallaSinNA$minutos_pantalla<2.30))%>% arrange(ascendente(tiempo_pantallaSinNA))
View(ultimos10)

# ---------------------------------------------------------------------------------------------------------------- 
# PUBLICADO - ggplot con menor cantidad de minutos en pantalla con paleta Darjeeling2
# ---------------------------------------------------------------------------------------------------------------- 
pal <- wes_palette("Darjeeling2", 10, type = "continuous")
p<- ggplot(data=ultimos10, aes(x=nombre, y=minutos_pantalla), colour=nombre) + geom_col(aes(fill=nombre))
p +  labs(x = "Personajes", y = "Tiempo (min)", colour = " Personajes", 
          title = "Personajes con menor cantidad de minutos en pantalla",
          subtitle = "Temporada de 1 a 6")+ 
  scale_fill_manual(values=pal)+ scale_x_discrete(limits=c("Ilyn Payne","Ternesio Terys","Quaithe", "Hugh of the Vale", "Rorge",
                                                             "Tickler", "Mhaegen","Biter", "Lhara", "Tobho Mott"))+
  #theme_grey()
  theme_bw()+
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
# opcion 2: ggplot con menor cantidad de minutos en pantalla con paleta color_brewer (Correcto para publicar)
# ----------------------------------------------------------------------------------------------------------------
#opcion 2 con scale_color_brewer(palette = "Dark2")
p<- ggplot(data=ultimos10, aes(x=nombre, y=minutos_pantalla), colour=nombre) + geom_col(aes(fill=nombre))
p +  labs(x = "Personajes", y = "Tiempo (min)", colour = " Personajes", 
          title = "Personajes con menor cantidad de minutos en pantalla",
          subtitle = "Temporada de 1 a 6")+ 
  scale_color_brewer(palette = "Dark2")+ scale_x_discrete(limits=c("Ilyn Payne","Ternesio Terys","Quaithe", "Hugh of the Vale", "Rorge",
                                                           "Tickler", "Mhaegen","Biter", "Lhara", "Tobho Mott"))+
  theme_minimal()+
  theme (axis.text.x = element_blank())+
  theme(axis.text.x = element_text(angle = 50, vjust = 1.1, hjust=1, color="black"))+  #los meses se visualizan en forma vertical en el eje x
  theme(plot.title = element_text(family="Courier",
                                  size=rel(1.5),       
                                  vjust=2, 
                                  hjust= 0.5,           #Justificación del título, permite centrarlo
                                  position_identity(center),   
                                  face="bold",          #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                  color="black",        #Color del t?tulo: maroon, lightblue, lightblue,darkblue, darkorange, black.
                                  lineheight=2.0),  legend.position= "", legend.text = element_blank())
# ---------------------------------------------------------------------------------------------------------------- 
# PUBLICADO - ggplot: con mayor cantidad de minutos en pantalla, con paleta Rushmore1 de WesAnderson
# ---------------------------------------------------------------------------------------------------------------- 
pal <- wes_palette("Rushmore1", 10, type = "continuous")  #preferidos: Rushmore1,GrandBudapest1, Darjeeling2
p<- ggplot(data=primeros10, aes(x=nombre, y=minutos_pantalla), colour=nombre) + geom_col(aes(fill=nombre))+
  labs(x = "Personajes", y = "Tiempo (min)", colour = " Personajes", 
          title = "Personajes con más minutos en pantalla",
          subtitle = "Temporada de 1 a 6")+
  scale_fill_manual(values=pal)+ scale_x_discrete(limits=c("Tyrion Lannister","Jon Nieve","Daenerys Targaryen", "Cersei Lannister", "Sansa Stark", "Arya Stark",
                                                           "Jaime Lannister","Theon Greyjoy", "Samwell Tarly", "Jorah Mormont"))+
  theme_bw()+
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
p

# ---------------------------------------------------------------------------------------------------------------- 
# Formateo de los índices para el gráfico de plotly
yaxis <- list(title = "USD (millones)",
              showline = TRUE,
              showgrid = FALSE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 10,
              tickfont = list(family = 'Arial',
                              size = 12,
                              color = 'rgb(82, 82, 82)'))
xaxis <- list(title = "Meses",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 5,
              tickfont = list(family = 'Arial',
                              size = 12,
                              color = 'rgb(82, 82, 82)'))


grafico <- plot_ly (x = ~(primeros10$nombre), y = ~(primeros10$minutos_pantalla), color = ~nombre,
        type = "bar") %>% layout(title= 'temporada 1 a 6', 
          xaxis= xaxis,
          yaxis = yaxis,
          showlegend =TRUE)
grafico
  
# ---------------------------------------------------------------------------------------------------------------- 
# Lectura del dataset 2: personaje_libros
# ---------------------------------------------------------------------------------------------------------------- 
personajes_libros <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/personajes_libro.csv")
View(personajes_libros)
genero <- as.factor(personajes_libros$genero)
View(genero)
# agrupa por lealtad y cuenta la cantidad de hombres y mujeres
generoporlealtad <-personajes_libros %>% group_by(lealtad) %>% count(genero)                 
View (generoporlealtad)
# A los que no tienen ninguna lealtad reempazo por sin Lealtad
generoporlealtad[generoporlealtad$lealtad == "Ninguna",1]<-"No pertenede a ninNo pertenece a ninguna casa noble"
# Renombro columnas
names (generoporlealtad) =c("lealtad", "genero","totalXgenero")
View(generoporlealtad)
# Selecciono los que tienen casa Noble, es decir, elimino los que no tienen Lealtad.
datosconCasaNoble <- generoporlealtad %>% filter(lealtad!="Sin Lealtad")  
View(datosconCasaNoble)

# Probar
#calculo el total de personajes para poder calcular luego el porcentaje
#TotalPersonajes <- (generoporlealtad %>% summarize(total=sum(generoporlealtad$totalXgenero)))
#View (TotalPersonajes)

#porcen<-generoporlealtad %>% ((generoporlealtad$totalXgenero/917)*100)
# ---------------------------------------------------------------------------------------------------------------- 
# Grafico para publicar en el tablero 
# ---------------------------------------------------------------------------------------------------------------- 
# Cantidad de personaje por géneros por casa Noble, se excluyen aquellos que no la tienen
ggplot(datosconCasaNoble, aes(x=lealtad,y=totalXgenero, colour=genero))+
geom_col(aes(fill=genero),width = .4)+
  #scale_fill_manual(values =c("purple", "lightblue")) +
  #scale_fill_manual(values=pal)+
  labs (title= "Cantidad de personaje por géneros por casa Noble", x = "Lealtad", y = "Cantidad por género")+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))+  #los meses se visualizan en forma vertical en el eje x
  theme(plot.title = element_text(family="Courier",
                                  size=rel(1.5),        #Tama?o relativo de la letra del t?tulo
                                  vjust=2,            #Justificaci?n vertical, para separarlo del gr?fico
                                  position_identity(center),   
                                  face="bold",        #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                  color="black", #Color del t?tulo: maroon, lightblue, lightblue,darkblue, darkorange, black.
                                  lineheight=2.0),  legend.position= "", legend.text = element_blank())
# ---------------------------------------------------------------------------------------------------------------- 
#plotly - # Grafico para publicar en el tablero
# ---------------------------------------------------------------------------------------------------------------- 
plot_ly (x = generoporlealtad$lealtad, y = generoporlealtad$totalXgenero, color = generoporlealtad$lealtad, text = paste('Lealtad','', generoporlealtad$lealtad,":",generoporlealtad$totalXgenero, sep=""), 
         hoverinfo = "text", type = "bar") %>% layout(xaxis = list(showline = F, 
                                               showticklabels = F, 
                                               fixedrange = T,
                                               showlegend =TRUE,
                                               title = "Lealtad"),
                                  yaxis = list(fixedrange = T, 
                                               title = ""))
# ---------------------------------------------------------------------------------------------------------------- 
#plotly
# ----------------------------------------------------------------------------------------------------------------  
p<- plot_ly (x = generoporlealtad$lealtad, y = generoporlealtad$totalXgenero, color = generoporlealtad$genero, text = paste('Lealtad','',generoporlealtad$lealtad,":",generoporlealtad$totalXgenero), 
         hoverinfo = "text", type = "bar") %>% layout(title= 'Temporada de la 1 a la 6', legend= 'Personaje',
                                                      xaxis = list(showline = F, 
                                                                   showticklabels = F, 
                                                                   fixedrange = T,
                                                                   showlegend =TRUE,
                                                                   title = "Lealtad"),
                                                      yaxis = list(fixedrange = T, 
                                                                   title = ""))
p
# ---------------------------------------------------------------------------------------------------------------- 
# Para probar y jugar
donutdata <- personajes_libros %>% 
  group_by(lealtad) %>% 
  (generoXLea = count(genero))
view(donutdata)

hc <-hchart(generoporlealtad, "pie", hcaes(name = lealtad, y = totalXgenero), innerSize = 300)

#
p %>% 
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = "<b>{point.key}</b>",
    pointFormatter = tooltip_chart(
      accesor = "ttdata",
      hc_opts = list(
        chart = list(type = "scatter"),
        plotOptions = list(scatter = list(marker = list(radius = 2)))
      ),
      height = 225
    ),
    positioner = JS(
      "function () {
      
        /* one of the most important parts! */
        xp =  this.chart.chartWidth/2 - this.label.width/2
        yp =  this.chart.chartHeight/2 - this.label.height/2
      
        return { x: xp, y: yp };
      
      }"),
    shadow = FALSE,
    borderWidth = 0,
    backgroundColor = "transparent",
    hideDelay = 1000
  )


sites %>% 
  count(ratingf) %>%
  plot_ly(type = "bar", 
          x = ratingf, 
          y = n, 
          color = ratingf, 
          text = paste(n,ratingf,sep=""), 
          hoverinfo = "text") %>%
  layout(xaxis = list(showline = F, 
                      showticklabels = F, 
                      fixedrange = T, 
                      title = ""),
         yaxis = list(fixedrange = T, 
                      title = ""))

noblefactor <- as.factor(personajes_libros$noble)
noblesporlealtad <- personajes_libros %>% group_by(lealtad) %>%                 
  sum(if(noblefactor== '1')0)
View (noblesporlealtad)

# arrange para ordenar
gptres <- gpdos %>% 
  group_by(provincia)  %>% 
  count(generoVictima)%>% 
# ---------------------------------------------------------------------------------------------------------------- 
# último dataset
cambio_lealtades <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/cambio_lealtades.csv")
View (cambio_lealtades)
