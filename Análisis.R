pacman::p_load(dplyr, ggplot2, plotly, sf)
options(digits = 3)

#CARGAR ARCHIVOS
folder <- "tidy_bases"

df <- read.csv(file.path(folder, "base_principal.csv"))
pobnal <- read.csv(file.path(folder, "base_población_nal.csv"))
pobdep <- read.csv(file.path(folder, "base_población_deps.csv"))
geometrias <- st_read(file.path(folder, "geometrias_dep.shp"))
colnames(geometrias) <- c("departamento", "geometry")


df_mes <- df%>%group_by(año, mes, id_depto)%>%
  summarise(intentos_suicidio = sum(intentos_suicidio),
            eventos_violentos = sum(eventos_violentos), 
            poblacion = sum(poblacion))

df_departamentos <- df%>%
  group_by(año, departamento, id_depto)%>%
  summarise (intentos_suicidio = sum(intentos_suicidio),
             eventos_violentos = sum(eventos_violentos))

df_departamentos <- left_join(df_departamentos, pobdep[,c("departamento", "año", "poblacion")])

df_departamentos <- df_departamentos%>%mutate(tasa_eventos_violentos = eventos_violentos/(poblacion/100000),
                          tasa_intentos_suicidio = intentos_suicidio/(poblacion/100000))

df_departamentos_promedio <- df_departamentos%>%
  group_by(departamento, id_depto)%>%
  summarise(eventos_violentos = sum(eventos_violentos),
            intentos_suicidio = sum(intentos_suicidio),
            tasa_promedio_EV = mean(tasa_eventos_violentos),
            tasa_promedio_IS = mean(tasa_intentos_suicidio))





victimas <- read.csv("victimas2019_2021.csv")

victimas[victimas$ESTADO_DEPTO=="Archip.De San Andres, Providencia y Santa Catalina",3] <- "San andres"


victimas <- victimas %>% arrange(ESTADO_DEPTO)%>%mutate (departamento = df_departamentos%>%arrange(departamento)%>%.$departamento)
victimas <- victimas[, c(2,5,4)]
colnames(victimas)<- c("año", "departamento", "victimas")


df_departamentos2 <- left_join(df_departamentos, victimas)
df_departamentos2 <- df_departamentos2%>%mutate(tasa_victimas = victimas/(poblacion/100000))


df_departamentos2 %>%summarise(cor(.$tasa_victimas, .$tasa_intentos_suicidio))



violencias <- violencias%>%mutate(mes = month(Fecha2))


violencas_departamento <- violencias %>% group_by(Año, Departamento)%>%summarise(victimas = sum(Total.de.Víctimas.del.Caso),
                                                                                 eventos_v = n())
colnames(violencas_departamento) <- c("año", "departamento", "victimas", "eventos_v")


base_cnmh <- left_join(df_departamentos, violencas_departamento)



base_cnmh <- base_cnmh%>%mutate(victimas = replace_na(victimas, 0),
                                eventos_v = replace_na(eventos_v,0),
                                eventos_v = sum(eventos_v),
                                tasa_victimas = victimas/(poblacion/100000),
                                tasa_eventos_v = eventos_v/(poblacion/100000))




base_cnmh %>% summarise(cor = cor(.$tasa_eventos_v, .$tasa_intentos_suicidio))



time_series <- plot_ly(semanas, type = "scatter", mode = "lines")%>%
  add_trace(x= ~Semana2, y= ~round(tasa, 1), name = "I. suicidio", line=list(color="73A9AD"))%>%
  add_trace(x= ~Semana2, y= ~round(tasa2, 1), name = "E. violentos", line=list(color="C4DFAA"))%>%
 
  layout(
    title = "Intentos de suicidio y eventos violentos en colombia \nTasas semanales por millón de habitantes (2019 - 2021) ", 
    
    legend= list(title = list(text="Tipo de Evento")),
    
    xaxis = list(zerolinecolor = '#d1d8e3',
                 zerolinewidth = 0,
                 gridcolor = '#d1d8e3',
                 title = "Fecha"),
    
    yaxis = list(zerolinecolor = '#d1d8e3',
                 zerolinewidth = 0,
                 title = "Tasa de eventos violentos y de intentos de suicidio",
                 gridcolor = '#d1d8e3'
                 ),
   
    
    #plot_bgcolor='B9FFF8',
    shapes = list(list(
      type="line", 
                  y0 = 0.07, 
                  y1=0.99, 
                  x0= lubridate::mdy("03-16-2020"),
                  x1=lubridate::mdy("03-16-2020"),  
                  opacity = 0.5,
                  yref="paper", 
                  line = list(color="grey", dash="dot")),
      list(
        type="line", 
        y0 = 0.07, 
        y1=0.99, 
        x0 = lubridate::mdy("04-28-2021"), 
        x1=lubridate::mdy("04-28-2021"), 
        yref="paper", 
        opacity = 0.5,
        line = list(color="grey", dash="dot"))))%>%
  add_text(showlegend = F, 
           x = c(lubridate::mdy("03-17-2020"), lubridate::mdy("04-28-2021")), 
           y= c(0, 0), 
           text = c("Inicio de la pandemia", "Inicio del estallido social"))


time_series





tasa <- df%>%
  group_by(Departamento, Año)%>%
  summarise(Eventos = sum(N_eventos),
            Intentos = sum(tasa_intentos),
            Año = mean(Año))%>%
  full_join(x = geometrias, y=.)



g <- ggplot()+
  geom_sf(data = tasa, aes(fill = Intentos))+
  facet_wrap(Año ~ .)




g


View(df%>%group_by(Año, Departamento)%>%summarise(sum(N_eventos)))

a <- st_read("gadm41_COL_shp/gadm41_COL_0.shp")

View(df%>%filter(Año==2021)%>% group_by(Departamento, Año)%>%summarise(n()))








r <- ggplot()+
  geom_sf(data= b, aes(fill = eventos))+
  scale_fill_viridis_b(option="viridis", direction = -1)
  #geom_sf(data= b, aes(size= eventos), alpha = 0.3, col = "red")+
  #labs(ti)


s <- ggplot()+
  geom_sf(data= b, aes(fill = Intentos_suicidio))+
  scale_fill_viridis_b(option="viridis", direction = -1)
#geom_sf(data= b, aes(size= eventos), alpha = 0.3, col = "red")+
#labs(ti)

violencias
df


df2 <- df%>%
  filter(Departamento == "NARINO" | Departamento == "BOGOTA, D.C.")%>%
  mutate(Departamento = case_when(Departamento=="NARINO"~"NARIÑO", 
                                  Departamento=="BOGOTA, D.C."~"CUNDINAMARCA" ))


dff <- df%>%filter(Departamento!="NARINO" | Departamento != "BOGOTA, D.C.")%>% left_join(x=., y= df2)

dff<- dff%>%mutate(Semana2 = ymd(Semana2))


join(dff, violencias)
  

n_violentos <- violencias%>%mutate(Semana2 = Fecha2)%>%group_by(Departamento, Semana2)%>%summarise(eventos_violentos = n())


final_base <- (left_join(dff, n_violentos))

final_base <- final_base%>%mutate(eventos_violentos = replace_na(eventos_violentos, 0))


rips <- read.csv("tidy_bases/rips2.csv")

colnames(rips) <- c("x", "departamento", "año", "atenciones")

rips <- rips%>%filter(año %in% 2019:2021)%>%group_by(año, departamento)%>% summarise(atenciones = sum(atenciones))



RIPS <- RIPS%>%separate(Departamento, c("id_depto", "depa"), sep= " - ")%>%mutate(id_depto = as.numeric(id_depto))

rips
rips$depa <- iconv(rips$depa, from = "UTF-8", to="LATIN1")
rips$depa <- iconv(rips$depa, from = "LATIN1", to="ASCII//TRANSLIT")
View(left_join(rips, df_departamentos2))


str_enc_list()



rips <- read.csv("C:/Users/Yudy/Downloads/Registros_Individuales_de_Prestaci_n_de_Servicios_de_Salud___RIPS.csv")


rips_final <- rips %>% filter(str_detect(Diagnostico, "^F\\d{3}")  & Año %in% 2019:2021)

rips_final2 <- rips %>% filter(str_detect(Diagnostico, "^Z[56]")  & Año %in% 2019:2021)
         

rips <- rbind(rips_final, rips_final2)  

rips <- rips%>% 
  filter(!str_detect(Diagnostico, "^F[078]|^F64|^Z50[125-9]|^Z5[1-9]"))%>%
  #separate(Departamento, c("cod_depto", "Departamento", sep="-" ))%>%
  mutate(Departamento = iconv(Departamento, to="LATIN1"))%>%
  select(Año, Departamento, TipoAtencion, Diagnostico, NumeroAtenciones)

a <- rips%>%group_by(Departamento)%>%summarise(n = n())


write.csv(rips, "tidy_bases/rips2.csv")




                                                                                 

geo <- st_read("tidy_bases/geometrias_dep.shp")
df <- read.csv("tidy_bases/base_is_año.csv")
df <- df%>%group_by(departamento)%>%summarise(intentos_suicidio = sum(intentos_suicidio))

geo <- left_join(geo, df, by= c("dprtmnt" = "departamento"))

plot_ly(geo)

plot_geo(geo, colors = "Viridis")


ggplotly(
  ggplot(geo)+
    geom_sf(aes(fill= intentos_suicidio))+
  scale_fill_viridis_b(option="viridis", direction = -1))

  