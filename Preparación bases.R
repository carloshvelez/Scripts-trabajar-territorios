pacman::p_load(dplyr, tidyr, stringr, readr, ggplot2, lubridate, sf)

source("Scripts/Funciones.R") ##Cargo función que modifiqué para calcular semanas epidemiológicas


folder <- "Data"
archivo_1 <- "acled_data.csv"
archivo_2 <- "i_suicidio_2019.csv"
archivo_3 <- "i_suicidio_2020.csv"
archivo_4 <- "i_suicidio_2021.csv"
archivo_5 <- "población.csv"



ubicacion <- st_read("gadm41_COL_shp/gadm41_col_1.shp")
ubicacion <- ubicacion %>% mutate(departamento = NAME_1)%>%select(departamento, geometry)

ubic_long_lat <- read.csv("Data/ubicación_departamentos_divipola.csv", sep = ";")
ubic_long_lat <- ubic_long_lat[,c(1,3,4)]
colnames(ubic_long_lat)<- c("id_depto", "latitud", "longitud")



eventos_violentos <- read.csv(file.path(folder, archivo_1), encoding = "Latin-1")
i_suicida_2019 <- read.csv(file.path(folder, archivo_2), sep=";", dec=",", strip.white = T)
i_suicida_2020 <- read.csv(file.path(folder, archivo_3), sep=";", dec=",", strip.white = T)
i_suicida_2021 <- read.csv(file.path(folder, archivo_4), sep=";", dec=",", strip.white = T)
i_suicida_2021[17,1]<- "NARIÑO"
población <- read.csv(file.path(folder, archivo_5), sep=";", dec=",", strip.white = T)

# PREPARAR BASE DE POBLACIÓN

## Población departamental.

población <- población%>%filter(AÑO %in% 2019:2021 & ÁREA.GEOGRÁFICA == "Total")%>%
  select(DP, DPNOM, AÑO, Total)%>%
  mutate(DPNOM = toupper(iconv(DPNOM, from="UTF-8",to="ASCII//TRANSLIT")), 
        DPNOM = toupper(iconv(DPNOM, from="ASCII//TRANSLIT",to= "UTF-8")),
        DPNOM = str_replace_all(DPNOM, "ARCHIPIELAGO DE SAN ANDRES", "SAN ANDRES Y PROVIDENCIA"),
        Total = as.numeric(str_remove_all(Total, "\\.")))
 
colnames(población)<- c("id_depto", "departamento", "año", "poblacion")




## población nacional
pob_nac <- data.frame(Año = c(2019, 2020, 2021),
                      población = c(49395678, 50372424, 51049498))


# PREPARAR BASES DE INTENCIÓN SUICIDA: 
i_suicida_2019 <-  pivot_longer(i_suicida_2019, cols = starts_with("Semana"), names_to = "semana", values_to = "intentos_suicidio") %>% mutate(año = 2019)
i_suicida_2020 <-  pivot_longer(i_suicida_2020, cols = starts_with("Semana"), names_to = "semana", values_to = "intentos_suicidio") %>% mutate(año = 2020,
                                                                                                                                               intentos_suicidio = as.numeric(intentos_suicidio))
i_suicida_2021 <-  pivot_longer(i_suicida_2021, cols = starts_with("Semana"), names_to = "semana", values_to = "intentos_suicidio") %>% mutate(año = 2021)

colnames(i_suicida_2020) <- colnames(i_suicida_2019)
colnames(i_suicida_2021) <- colnames(i_suicida_2019)

## Uno las bases de intención suicida. 
i_suicida <- rbind(i_suicida_2019, i_suicida_2020, i_suicida_2021)

## Preparo la base final de intención suicida. 
i_suicida <- i_suicida %>% 
  mutate(NDEP_PROCE = str_replace_all(NDEP_PROCE, c("BOGOTA"="BOGOTA, D.C.", "GUAJIRA"= "LA GUAJIRA", "NORTE SANTANDER" = "NORTE DE SANTANDER", "VALLE" = "VALLE DEL CAUCA", "SAN ANDRES" = "SAN ANDRES Y PROVIDENCIA", "NARIÑO"= "NARINO")),
         semana = parse_number(semana),
         intentos_suicidio = replace_na(intentos_suicidio, 0))%>%
  select(-2)%>%
  mutate(fecha = case_when(año==2021 ~ sapply(semana, Última_fecha_semana_epi, year = 2021)[6,],
                                         año==2020 ~ sapply(semana,  Última_fecha_semana_epi, year = 2020)[6,],
                                         año==2019 ~ sapply(semana,  Última_fecha_semana_epi, year = 2019)[6,]),
                       fecha = as_date(fecha),
                       mes = month(fecha))  ### En este punto tengo base por departamento, años, mes y semana. 
  
colnames(i_suicida)<- c("departamento", "semana", "intentos_suicidio", "año", "fecha", "mes")
### Uno la bse de intención suicida con la de población. 



i_suicida_año_mes_semana <- i_suicida[, c(5, 4, 6, 2, 1, 3)]
i_suicida_año_mes <- i_suicida%>%group_by(año, departamento, mes)%>%summarise(intentos_suicidio = sum(intentos_suicidio))
i_suicida_año <- i_suicida%>%group_by(año, departamento)%>%summarise(intentos_suicidio = sum(intentos_suicidio))


df <- left_join(i_suicida_año, población)



#PREPARAR LA BASE DE EVENTOS VIOLENTOS

## selecciono eventos entre 2019 y 2022 y excluyo las protestas pacíficas. Selecciono las variables que requiero y las renombro. 
eventos <- eventos_violentos %>% filter(year %in% 2019:2021 & sub_event_type!="Peaceful protest")%>%select(event_date, year,  fatalities, admin1)
colnames(eventos)<- c("fecha", "año", "fatalidades", "departamento")

## Busco que los nombres de los departamentos coincidan, para hacer operaciones de unión. 
eventos <- eventos %>% mutate(departamento = toupper(departamento))
sum(sort(unique(eventos$departamento))!=sort(unique(i_suicida$departamento))) ## Verifico que el nombre de departamentos sea el mismo.


## Agrego fechas y semanas epidemiológicas.
eventos <- eventos %>% mutate(fecha = dmy(fecha, tz = Sys.timezone()), # Fechas a formato estándar
                              mes = month(fecha),
                              semana_epi = epiweek(fecha))







# Preparo una base con eventos totales
eventos_año_mes_departamento <- eventos %>% group_by(año, mes, departamento)%>%summarise(eventos_violentos = n(),
                                                                                         fatalidades = sum(fatalidades))
eventos_año_departamento <- eventos%>%group_by(año, departamento)%>%summarise(eventos_año_departamento = n(),
                                                                              fatalidades = sum(fatalidades))



df <- left_join(df, eventos_año_departamento)






rips <- read.csv("C:/Users/Yudy/Downloads/Registros_Individuales_de_Prestaci_n_de_Servicios_de_Salud___RIPS.csv")

rips <- rips %>% filter(Año %in% 2017:2022 & str_detect(Diagnostico, "^F|^Z5[5-9]|^Z6[0-5]"))




#rips_final2 <- rips %>% filter(str_detect(Diagnostico, "^Z[56]")  & Año %in% 2019:2021)
#RIPS <- rbind(rips_final, rips_final2)  
#write.csv(rips, "DATA/rips_filtrados_año_diagnostico.csv")

#rips <- read.csv("DATA/rips_filtrados_año_diagnostico.csv")

rips <- rips%>% 
  mutate(Departamento = iconv(Departamento, to="LATIN1"))%>%
  select(Año, Departamento, TipoAtencion, Diagnostico, NumeroAtenciones)%>%
  separate(Departamento, c("id_depto", "depa"), sep= " - ")%>%
  mutate(id_depto = as.numeric(id_depto))


rips<- rips%>%mutate(clasificacion = case_when(str_detect(Diagnostico, "^F0")~"F0 - Trastornos mentales orgánicos incluídos los trastornos somáticos",
                                            str_detect(Diagnostico, "^F1")~"F1 - Trastornos debidos al uso de sustancias psicotrópicas",
                                            str_detect(Diagnostico, "^F2")~"F2 - Esquizofrenia, trastorno esquizotípico y trastornos de ideas delirantes",
                                            str_detect(Diagnostico, "^F3")~"F3 - Trastornos del humor (afectivos)",
                                            str_detect(Diagnostico, "^F4")~"F4 - Trastornos neuróticos, secundarios a situaciones estresantes y somatomorfos",
                                            str_detect(Diagnostico, "^F5")~"F5 - Trastornos del comportamiento asociados a disfunciones fisiológicas y a factores somáticos",
                                            str_detect(Diagnostico, "^F6")~"F6 - Trastornos de la personalidad y del comportamiento adulto",
                                            str_detect(Diagnostico, "^F7")~"F7 - Retraso Mental",
                                            str_detect(Diagnostico, "^F8")~"F8 - Trastornos del desarrollo psicológico",
                                            str_detect(Diagnostico, "^F9")~"F9 - Trastornos del comportamiento y de las emociones de comienzo habitual en la infancia y adolescencia",
                                            str_detect(Diagnostico, "^Z5[5-9]|^Z6[0-5]")~"Z55 - Z65 Personas con problemas potenciales psíquicos o psicosociales"
                                            ))

rips <- rips%>%filter(!is.na(clasificacion))

rips <- full_join(rips, ubic_long_lat)

rips$depa[rips$id_depto == 91] <- "Amazonas"
rips$depa[rips$id_depto == 94] <- "Guainía"
rips$depa[rips$id_depto == 95] <- "Guaviare"
rips$depa[rips$id_depto == 86] <- "Putumayo"
rips$depa[rips$id_depto == 88] <- "San andrés y providencia"
rips$depa[rips$id_depto == 91] <- "Amazonas"
rips$depa[rips$id_depto == 97] <- "Vaupés"
rips$depa[rips$id_depto == 99] <- "Vichada"

rips <- left_join(rips, población[,c(1,3,4)], by = c("Año" = "año", "id_depto" = "id_depto"))

write.csv(rips, "DATA/rips_filtrados_año_diagnostico_clasificacion.csv")

rips_año_depa_clasificacion <- rips%>%group_by(Año, id_depto, depa, clasificacion, latitud, longitud, poblacion)%>%summarise(atenciones_sm = sum(NumeroAtenciones))




rips_año_departamento <- rips%>%group_by(Año, id_depto, depa, latitud, longitud)%>%summarize(atenciones_sm = sum(NumeroAtenciones))

colnames(rips_año_departamento)<- c("año", "id_depto", "Departamento", "latitud", "longitud", "atenciones_sm" )

df <- left_join(df, rips_año_departamento, by=c("año", "id_depto"))


violencias <- read.csv("Data/violencias.csv")
violencias <-  violencias%>%group_by(Departamento, Año)%>%summarise(eventos_2 = n(),
                                                                    victimas_2 = sum(Total.de.Víctimas.del.Caso))


df <- left_join(df, violencias)



## base víctimas unidad de víctimas

victimas_año <- read_delim("victimas_año.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
victimas_años <- victimas_año %>%filter(VIGENCIA %in% 2019:2021 & COD_ESTADO_DEPTO != 0)


victimas_años2 <- victimas_años%>%group_by(VIGENCIA, ESTADO_DEPTO, COD_ESTADO_DEPTO)%>%summarise(victimas = sum(EVENTOS))

colnames(victimas_años2)<- c("año", "dpto", "id_depto", "victimas_uv")
victimas_años2



df <- left_join(df, victimas_años2)



#PREPARO BASES DE RIPS





## Preparo una base con las geometrías de cada departamento: 

ubicacion <- st_read("gadm41_COL_shp/gadm41_col_1.shp")
sort(unique(ubicacion$NAME_1))
nombres_departamento <- sort(unique(df$departamento))
ubicacion <- ubicacion%>%arrange(NAME_1) %>% mutate(Departamento_original = NAME_1,
                                                    departamento = nombres_departamento)%>%
                                  select(Departamento_original, departamento, geometry)
ubicacion%>%mutate(departamento = nombres_departamento)

ubic_long_lat <- read.csv("Data/ubicación_departamentos_divipola.csv", sep = ";")
ubic_long_lat <- ubic_long_lat[,c(1,3,4)]
colnames(ubic_long_lat)<- c("id_depto", "latitud", "longitud")

df_ubicacion <- left_join(ubicacion, df) %>%left_join(ubic_long_lat)


# GUARDAR LAS BASES
write.csv(df, file.path("tidy_bases", "base_principal.csv"))
st_write(df, file.path("tidy_bases", "base_principal_con_ubicacion.shp"))

write.csv(eventos, file.path("tidy_bases", "base_eventos_año_mes_semana.csv"))
write.csv(eventos_año_departamento, file.path("tidy_bases", "base_eventos_año.csv"))
write.csv(eventos_año_mes_departamento, file.path("tidy_bases", "base_eventos_año_mes.csv"))

write.csv(población, file.path("tidy_bases", "base_población_deps.csv"))
write.csv(pob_nac, file.path("tidy_bases", "base_población_nal.csv"))

write.csv(i_suicida_año_mes, file.path("tidy_bases", "base_is_año_mes.csv"))
write.csv(i_suicida_año, file.path("tidy_bases", "base_is_año.csv"))
write.csv(i_suicida_año_mes_semana, file.path("tidy_bases", "base_is_año_mes_semana.csv"))

write.csv(victimas_años2, "tidy_bases/victimas_uv_2019_2021.csv")

write.csv(rips_año_departamento, file.path("tidy_bases", "rips_año_departamento.csv"))
write.csv(rips_año_depa_clasificacion, file.path("tidy_bases", "rips_año_depa_clasificacion.csv"))

write.csv(rips, "tidy_bases/rips2.csv")
st_write(ubicacion, file.path("tidy_bases", "geometrias_dep.shp"))





