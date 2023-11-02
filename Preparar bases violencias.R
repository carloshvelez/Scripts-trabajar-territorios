pacman::p_load(dplyr, tidyr, lubridate)

source("Scripts/Funciones.R")

nombres_variables <- c("Código.DANE.de.Municipio", "ID.Caso", "Departamento","Año", "Mes", "Día", "Sexo",   "Etnia", "Ocupación",
             "Edad", "Longitud", "Latitud", "Región", "Modalidad", "Total.de.Víctimas.del.Caso",  "Tipo_violencia") 


acciones_belicas <- read.csv("Data/Violencias/acciones_belicas.csv")%>%
  filter(Año %in% (2019:2021))%>%
  mutate(Código.DANE.de.Municipio = as.numeric(Código.DANE.de.Municipio)) %>% 
  select(nombres_variables) %>% 
  filter (Departamento != "EXTERIOR")

asesinatos_selectivos <- read.csv("Data/Violencias/asesinatos_selectivos.csv")%>%
  filter(Año %in% (2019:2021))%>%
  mutate(Código.DANE.de.Municipio = as.numeric(Código.DANE.de.Municipio))%>% 
  select(nombres_variables) %>% 
  filter (Departamento != "EXTERIOR")

ataques_poblacionales <- read.csv("Data/Violencias/ataques_poblacionales.csv")%>%
  filter(Año %in% (2019:2021))%>%
  mutate(Código.DANE.de.Municipio = as.numeric(Código.DANE.de.Municipio))%>%
  select(nombres_variables[nombres_variables!="Modalidad"]) %>% 
  filter (Departamento != "EXTERIOR")

atentados_terroristas <- read.csv("Data/Violencias/atentados_terroristas.csv")%>%
  filter(Año %in% (2019:2021))%>%
  mutate(Código.DANE.de.Municipio = as.numeric(Código.DANE.de.Municipio))%>% 
  select(nombres_variables) %>%
  filter (Departamento != "EXTERIOR")

daño_bienes <- read.csv("Data/Violencias/daño_bienes.csv")%>%
  filter(Año %in% (2019:2021))%>%
  mutate(Código.DANE.de.Municipio = as.numeric(Código.DANE.de.Municipio))%>% 
  select(nombres_variables) %>% filter (Departamento != "EXTERIOR")

desaparicion_forzada <- read.csv("Data/Violencias/desaparición_forzada.csv")%>%
  filter(Año %in% (2019:2021))%>%
  mutate(Código.DANE.de.Municipio = as.numeric(Código.DANE.de.Municipio))%>%
  select(nombres_variables) %>% filter (Departamento != "EXTERIOR")

masacres <- read.csv("Data/Violencias/acciones_belicas.csv")%>%
  filter(Año %in% (2019:2021))%>%
  mutate(Código.DANE.de.Municipio = as.numeric(Código.DANE.de.Municipio))%>%
  select(nombres_variables) %>% 
  filter (Departamento != "EXTERIOR")

minas <- read.csv("Data/Violencias/masacres.csv")%>%
  filter(Año %in% (2019:2021))%>%
  mutate(Código.DANE.de.Municipio = as.numeric(Código.DANE.de.Municipio))%>% 
  select(nombres_variables) %>%
  filter (Departamento != "EXTERIOR")

reclutamiento_NNA <- read.csv("Data/Violencias/reclutamiento_NNA.csv")%>%
  filter(Año %in% (2019:2021))%>%
  mutate(Código.DANE.de.Municipio = as.numeric(Código.DANE.de.Municipio))%>% 
  select(nombres_variables) %>% filter (Departamento != "EXTERIOR")

secuestro <- read.csv("Data/Violencias/secuestro.csv")%>%
  filter(Año %in% (2019:2021))%>%
  mutate(Código.DANE.de.Municipio = as.numeric(Código.DANE.de.Municipio))%>%
  select(nombres_variables) %>% 
  filter (Departamento != "EXTERIOR")

violencia_sexual <- read.csv("Data/Violencias/violencia_sexual.csv")%>%
  filter(Año %in% (2019:2021))%>%
  mutate(Código.DANE.de.Municipio = as.numeric(Código.DANE.de.Municipio))%>% 
  select(nombres_variables) %>% 
  filter (Departamento != "EXTERIOR")


todas <- full_join(acciones_belicas, asesinatos_selectivos)%>%
  full_join(ataques_poblacionales)%>%
  full_join(atentados_terroristas)%>%
  full_join(daño_bienes)%>%
  full_join(desaparicion_forzada)%>%
  full_join(masacres)%>%full_join(minas)%>%
  full_join(reclutamiento_NNA)%>%
  full_join(secuestro)%>%
  full_join(violencia_sexual)



violencias <- todas %>% 
  unite("Fecha", c(Año, Mes, Día), sep = "-")%>%
  mutate(Fecha = ymd(Fecha),
         Año = year(Fecha), 
         Semana = epiweek(Fecha))%>%
  filter(!is.na(Fecha))%>%mutate( Fecha2 = case_when(Año==2021 ~ sapply(Semana, Última_fecha_semana_epi, year = 2021)[6,],
                                                     Año==2020 ~ sapply(Semana,  Última_fecha_semana_epi, year = 2020)[6,],
                                                     Año==2019 ~ sapply(Semana,  Última_fecha_semana_epi, year = 2019)[6,]),
                                  Fecha2 = as_date(Fecha2))


write.csv(violencias, "Data/violencias.csv")
 violencias <- read.csv("Data/violencias.csv")
 
violencias <-  violencias%>%group_by(Departamento, Año)%>%
  summarise(eventos_2 = n(),
            victimas_2 = sum(Total.de.Víctimas.del.Caso))
 