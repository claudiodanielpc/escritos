#Encuesta Nacional de Vivienda (ENVI) 2020
#Artículo para revista del INFONAVIT

##Borrar datos del entorno
rm(list=ls())


#Directorio para descargar de datos
#Esto se debe de cambiar en cada computadora
setwd("D:/datos")
#creamos carpeta en donde almacenaremos los archivos
dir.create("envi", showWarnings = F)


# Librerías ====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, srvyr,kableExtra, webshot)


#Carga de la fuente para visualización de los datos
windowsFonts(fuente = windowsFont("Arial"))

#Descarga de archivos====
url<-"https://www.inegi.org.mx/contenidos/programas/envi/2020/microdatos/envi_2020_base_de_datos_csv.zip"

temp <- tempfile()
download.file(url,
              mode = "wb",
              destfile = temp)
unzip(temp, exdir = "envi") 
unlink(temp)


#Lectura, limpieza y construcción de variables sobre archivo ENVI====
envi<-read.csv("envi/Bases de datos/TVIVIENDA.csv")%>%
  janitor::clean_names()%>%
    #renombrar
  rename(folio=1,
         tot_resid=p1_1,
         num_cuarto=p4_10a
         )%>%
  #Calcular indicador clásico de hacinamiento
  mutate(hac=if_else(
    (tot_resid / num_cuarto) > 2.5, 
    "Hacinamiento",
    "Fuera de hacinamiento"),
    #Hacinamiento numérico
    hacnum=tot_resid / num_cuarto,
#Reclasificación de metros
        metros=case_when( 
#Hasta 45          
p4_21_1<=45 | p4_21_2<=2 ~ "Hasta 45 m2",
#De 46 a 75
(p4_21_1 >= 46 & p4_21_1 <= 75) | (p4_21_2 == 3 | p4_21_2 ==  4) ~ "De 46 a 75 m2",
#De 76 a 100
(p4_21_1 >= 76 & p4_21_1 <= 100) | (p4_21_2 == 5) ~ "De 76 a 100 m2",
#De 101 a 150
(p4_21_1 >= 101 & p4_21_1 <= 150) | (p4_21_2 == 6) ~ "De 101 a 150 m2",
#Más de 150
(p4_21_1 >= 151 & p4_21_1 <= 997) | (p4_21_2 == 7 | p4_21_2 == 8) ~ "Más de 150 m2",
#No especificado
(p4_21_1 == 998 | p4_21_1 == 999) & (p4_21_2 == 9) ~ "No especificado"
          
          
        ),
metros=factor(metros,levels = c("Hasta 45 m2", 
                                "De 46 a 75 m2",
                                "De 76 a 100 m2",
                                "De 101 a 150 m2",
                                "Más de 150 m2",
                                "No especificado"
                                
                                )),
#Necesidad de ampliación
nec_amp=case_when(
  p6_1_2==1 | p6_1_3==1 | p6_1_4==1 ~ "Con necesidad",
  TRUE ~ "Sin necesidad"
  
)

    )

#Definición de diseño muestral para precisiones estadísticas====

options(survey.lonely.psu="certainty")

dism <-envi%>%
  as_survey_design(ids=upm_dis,
                   strata=est_dis,
                   weights=factor)


#Visualizaciones de datos====

#Directorio de trabajo para guardar visualizaciones
setwd("D:/Documentos/envi_infonavit")


#Gráfica 1====

envi%>%
  group_by(hac)%>%
  summarise(vivi=sum(factor))%>%
  mutate(pct=vivi/sum(vivi)*100)%>%
ggplot(., aes(fill=hac, y=pct, x="")) + 
  geom_bar(position="stack", stat="identity")+
  coord_polar("y", start=0) +
  theme_void() + 
  
  geom_text(aes(label = format(round(pct,1))),
            color = "white", size=10, fontface="bold", 
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual("Condición",
                    values=c("#B38E5D", "#285C4D"))+
  labs(title="Gráfico 1. Viviendas por condición de hacinamiento de sus residentes, 2020",
       subtitle = "(Porcentaje)",
       caption="Fuente: Elaboración propia con datos de INEGI. Encuesta Nacional de Vivienda (ENVI) 2020.")+
  theme(text=element_text(family="fuente"),
        plot.title = element_text(hjust = 0, size=30,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=18),
        legend.position = "right",
        legend.text = element_text(size=18),
        legend.title = element_text(size=18))


ggsave("graf1.png",
       height=10, width=20, units='in', dpi=300)


#Gráfica 2====

#Calculo de mediana
med<-dism%>%
  filter(hac=="Hacinamiento")%>%
summarise(mediana=survey_median(hacnum))%>%
  select(mediana)%>%
  as.numeric()


envi%>%
  filter(hac=="Hacinamiento")%>%
  ggplot(., 
         aes(hacnum,weight=factor)) +
  geom_density(fill="#285c4d")+
  geom_vline(aes(xintercept=median(hacnum)), color="#b38e5d",
             linetype="dashed", size=3)+
  annotate(geom = "text", x = 3.5, y = 0.8, 
           label = paste0("Mediana: ", round(med,2)),
           hjust = 0, vjust=1, fontface="bold", color="#d95f0e",
           size=6)+
  theme_minimal()+
  labs(title="Gráfico 2. Densidad de viviendas por condición de hacinamiento de sus residentes, 2020",
    x="Grado de hacinamiento",
       y="Densidad",
       caption = "
Nota: La línea punteada vertical indica la mediana de hacinamiento.       
Fuente: Elaboración propia con datos de INEGI. Encuesta Nacional de Vivienda (ENVI) 2020.")+
  theme(
    plot.title = element_text(hjust = 0, size=30,face="bold"),
    plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
    plot.caption = element_text(hjust = 0,size=18),
    axis.ticks.x = element_blank(),
    axis.text = element_text(size=15),
    legend.position = c(0.8,0.4),
    legend.title=element_text(colour="black",
                              size=20,face="bold"),
    legend.text=element_text(colour="black",
                             size=18,face="bold"),
    #Fuente del gráfico
    text=element_text(family="fuente"))    


ggsave("graf2.png",
       height=10, width=20, units='in', dpi=300)


#Gráfico 3====
  envi%>%
    filter(hac=="Hacinamiento" & metros!="No especificado")%>%
    mutate(grupo=case_when(
      hacnum>2.5 & hacnum<4 ~ "Mayor a 2.5 a menos de 4",
      hacnum>=4 ~ "De 4 y más"),
      grupo=factor(
        grupo, levels=c(
          "Mayor a 2.5 a menos de 4",
          "De 4 y más"
        )
        
      )
      )%>%
    
    group_by(metros,grupo)%>%
    summarise(viviendas=sum(factor))%>%
    ungroup()%>%
  ggplot(., 
       aes(x = grupo, y = metros, fill = viviendas)) + 
  geom_tile()+
    theme_minimal()+
    ##Colores del heatmap
    scale_fill_distiller("Viviendas",palette = "YlOrBr",direction=1,labels=scales::comma)+
    labs(title = "Gráfico 3. Metros cuadrados de construcción de la vivienda y 
grado de hacinamiento",
         x="Grado de hacinamiento",
         y="Metros cuadrados de construcción",
         subtitle = "(Viviendas)",
         caption="Nota: Se excluyen del gráfico a las viviendas que no especificaron los metros de construcción.
Fuente: Elaboración propia con datos de INEGI. Encuesta Nacional de Vivienda (ENVI) 2020.")+
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour="black"),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.title.align = 0.5,
        legend.text=element_text(colour="black",size=14,face="bold"),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(colour="black", size=13),
        axis.text.y=element_text(colour="black",size=13),
        axis.ticks=element_blank(),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        strip.text.x = element_text(size=20, color="black",
                                    face="bold"),
        plot.title=element_text(colour="black",hjust=0,size=30,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=18),
        text=element_text(family="fuente"))
  
  
  ggsave("graf3.png",
         height=10, width=20, units='in', dpi=300)

  
  #Cuadro 2====
  cuadro<-data.frame(minimo=c(45,75,100,140,150),
                     mediana=c(3.33,3.33,3.33,3.33, 3.33)
  )%>%
    mutate(espocup=minimo/mediana)%>%
    select(minimo, espocup)
  
  cuadro%>%
    mutate(espocup=round(espocup,2))%>%
    ##Crear tabla
    kable(caption=paste(
      text_spec("Cuadro 2. Metros cuadrados de construcción de vivienda y promedio de espacio ocupado por residente, 2020",
                bold=T, color="black",font_size = 18),
      sep="\n"),
      format="html",
      align = "c",
      col.names = c(
        "Superficie de construcción\n(metros cuadrados)",
        "Metros cuadrados promedio ocupados por cada residente"))%>%
    kable_styling(full_width = F, font_size = 14,
                  html_font = "Arial")%>%
    row_spec(0, bold = F, color = "white", background = "#285C4D")%>%
    footnote(general = "Elaboración propia con datos de INEGI. Encuesta Nacional de Vivienda (ENVI) 2020.
             
             Nota: Los metros cuadrados promedio ocupados por cada residente se obtuvieron dividiendo la superficie de construcción y la mediana de hacinamiento",
             general_title = "
Fuente: ",
             threeparttable=T)%>%
    as_image(file="cuadro2.png")
  
  
    
#Gráfico 4====
  
  envi%>%
    filter(hac=="Hacinamiento" & metros!="No especificado")%>%  
    group_by(metros,nec_amp)%>%
    summarise(viviendas=sum(factor))%>%
    ungroup()%>%
    mutate(viviendas=
             viviendas/1000)%>%
  ggplot(., aes(x=nec_amp, y=viviendas)) +
    
    geom_histogram(fill="#9ebcda",
                   position="identity", stat="sum")+
    coord_flip()+
    facet_wrap(~metros)+
    geom_text(aes(label=format(round(viviendas,2))),
              hjust=0,
              color="#35978f",
              size=6.5,fontface="bold")+
    scale_y_continuous("Viviendas",
                       limits =c(0,500),
                       breaks=c(0,500))+
    
    theme_minimal()+
    labs(title = "Gráfico 4. Viviendas con habitantes en hacinamiento, metros cuadrados de construcción y
necesidades autoidentificadas de ampliación",
         subtitle = "(Miles de viviendas)",
         x="Tipo",
         caption = "Notas: Se excluyen del gráfico a las viviendas que no especificaron los metros de construcción.
Se considera como 'Con necesidad' aquellas identificadas por sus residentes, ya sean de cuartos, baños o cocina. 
Se clasificó como 'Sin necesidad' a aquellas viviendas en donde no se identificaron necesidades 
por parte de los residentes, no se sabía o no se contestó a la pregunta.
Fuente: Elaboración propia con datos de INEGI. Encuesta Nacional de Vivienda (ENVI) 2020.")+
    theme(plot.title = element_text(hjust = 0, 
                                    size=30,
                                    face="bold"),
          plot.subtitle = element_text(hjust = 0,
                                       size=25, 
                                       face="italic"),
          plot.caption = element_text(hjust = 0,
                                      size=18),
          legend.position = "none",
          text=element_text("Arial",
                            size=20),
          axis.text.x=element_text(size=15),
          strip.background = element_rect(
            color="black", fill="#FC4E07", 
            size=1.5, linetype="solid"
          ),
          strip.text = element_text(
            size = 20, color = "black", 
            face = "bold.italic")
    )
  
  ##Salvar gráfico
  ggsave("graf4.png", width = 20, height = 15)  
  

  
  
  
  #Gráficos 5, 6,7====
  
  
  satisf<-envi%>%
    filter(hac=="Hacinamiento" & metros!="No especificado" &
             nec_amp=="Con necesidad"
    )%>%
    select(p6_4_1,p6_4_2,p6_4_3,p6_4_4,factor)%>%
    gather(key, value, -factor)%>%
    rename(
      sat=value,
      variable=key
    )%>%
    mutate(
      variable=case_when(
        variable=="p6_4_1" ~ "cuartos para dormir",
        variable=="p6_4_2" ~ " baños",
        variable=="p6_4_3" ~ "cocina",
        variable=="p6_4_4" ~ "sala comedor"
        
      ),
    sat=case_when(
      sat==1 ~ "Muy satisfechos",
      sat==2 ~ "Algo satisfechos",
      sat==3 ~ "Poco satisfechos",
      sat==4 ~ "Nada satisfechos"
      
    ),
    
    sat=factor(
      sat, levels=c("Muy satisfechos",
                    "Algo satisfechos",
                    "Poco satisfechos",
                    "Nada satisfechos"
      )
      
    )
    
      
    )

   
lista<-unique(satisf$variable)
num<-c(5:8)

for (i in seq_along(lista)) {
  satisf%>%
    filter(variable==lista[i])%>%
    group_by(sat)%>%
    summarise(viviendas=sum(factor))%>%
    ungroup()%>%
    mutate(viviendas=
             viviendas/1000,
           sat = fct_reorder(sat, desc(viviendas))
           
           )%>%
    ggplot(., aes(x=sat, y=viviendas)) +
    
    geom_histogram(fill="#9ebcda",
                   position="identity", stat="sum")+
    coord_flip()+
    geom_text(aes(label=format(round(viviendas,2))),
              hjust=0,
              color="#35978f",
              size=6.5,fontface="bold")+
    theme_minimal()+
    labs(title = paste0("Gráfico ",num[i],". Satisfacción con el tamaño de ",lista[i],
"\nde viviendas con necesidad de ampliación y cuyos habitantes\nse encuentran en hacinamiento"),
         subtitle = "(Miles de viviendas)",
         x="Grado de satisfacción",
        y="Miles de viviendas", 
         caption = "Notas: Se excluyen del gráfico a las viviendas que no especificaron los metros de construcción.
por parte de los residentes, no se sabía o no se contestó a la pregunta.
Fuente: Elaboración propia con datos de INEGI. Encuesta Nacional de Vivienda (ENVI) 2020.")+
    theme(plot.title = element_text(hjust = 0, 
                                    size=30,
                                    face="bold"),
          plot.subtitle = element_text(hjust = 0,
                                       size=25, 
                                       face="italic"),
          plot.caption = element_text(hjust = 0,
                                      size=18),
          legend.position = "none",
          text=element_text("Arial",
                            size=20),
          axis.text.x=element_text(size=15),
          strip.background = element_rect(
            color="black", fill="#FC4E07", 
            size=1.5, linetype="solid"
          ),
          strip.text = element_text(
            size = 20, color = "black", 
            face = "bold.italic")
    )    
    
    
    
  ##Salvar gráfico
  ggsave(paste0("graf",num[i],".png"), width = 20, height = 15)  
  
}
  
#Precisiones estadísticas de cálculos y visualizaciones====


#Gráfico 1
dism%>%
group_by(hac)%>%
  summarise(
    #Cuantificación de viviendas
    viviendas = survey_prop(vartype = c("cv", "se")))

  
#Gráfico 2
  dism%>%
    filter(hac=="Hacinamiento")%>%
    summarise(mediana=survey_median(hacnum,vartype = c("ci")))
  
  

#Gráfico 3
dism%>%
  filter(hac=="Hacinamiento" & metros!="No especificado")%>%
  mutate(grupo=case_when(
    hacnum>2.5 & hacnum<4 ~ "Mayor a 2.5 a menos de 4",
    hacnum>=4 ~ "De 4 y más"))%>%
  group_by(metros,grupo)%>%
  summarise(
  viviendas = survey_total(vartype = c("cv", "se")))



#Gráfico 4
dism%>%
  filter(hac=="Hacinamiento" & metros!="No especificado")%>%  
  group_by(metros,nec_amp)%>%
  summarise(
    viviendas = survey_total(vartype = c("cv", "se")))


##Gráfico 5
dism%>%
  filter(hac=="Hacinamiento" & metros!="No especificado" &
           nec_amp=="Con necesidad"
  )%>%
  group_by(p6_4_1)%>%
  summarise(
    viviendas = survey_total(vartype = c("cv", "se")))


##Gráfico 6
dism%>%
  filter(hac=="Hacinamiento" & metros!="No especificado" &
           nec_amp=="Con necesidad"
  )%>%
  group_by(p6_4_2)%>%
  summarise(
    viviendas = survey_total(vartype = c("cv", "se")))


##Gráfico 7
dism%>%
  filter(hac=="Hacinamiento" & metros!="No especificado" &
           nec_amp=="Con necesidad"
  )%>%
  group_by(p6_4_3)%>%
  summarise(
    viviendas = survey_total(vartype = c("cv", "se")))


##Gráfico 8
dism%>%
  filter(hac=="Hacinamiento" & metros!="No especificado" &
           nec_amp=="Con necesidad"
  )%>%
  group_by(p6_4_4)%>%
  summarise(
    viviendas = survey_total(vartype = c("cv", "se")))
