pacman::p_load(openxlsx, dplyr)

##Descargo bases

enlace_base <- "https://micrositios.centrodememoriahistorica.gov.co/observatorio/download/"

v_violencia_sexual <- read.xlsx(paste(enlace_base, "/victimas-vs/?wpdmdl=1274&refresh=630e3bd70a3e11661877207"))
c_violencia_sexual <- read.xlsx(paste(enlace_base, "/casos-vs/?wpdmdl=1260&refresh=630e3af8f0ead1661876984"))

v_secuestro <-read.xlsx(paste(enlace_base, "/victimas-se/?wpdmdl=1273&refresh=630e3af890d891661876984"))
c_secuestro <- read.xlsx(paste(enlace_base, "/casos-se/?wpdmdl=1259&refresh=630e3af9074851661876985"))

v_reclutamiento_NNA <- read.xlsx(paste(enlace_base, "/victimas-ru/?wpdmdl=1272&refresh=630e3af89a62a1661876984"))
c_reclutamiento_NNA <- read.xlsx(paste(enlace_base, "/casos-ru/?wpdmdl=1258&refresh=630e3af910e911661876985"))

v_minas <- read.xlsx(paste(enlace_base, "/victimas-mi/?wpdmdl=1271&refresh=630e3af8a3b261661876984"))
c_minas <- read.xlsx(paste(enlace_base, "/casos-mi/?wpdmdl=1257&refresh=630e3af91b05d1661876985"))

v_masacres <- read.xlsx(paste(enlace_base, "/victimas-ma/?wpdmdl=1270&refresh=630e3af8ace271661876984"))
c_masacres <- read.xlsx(paste(enlace_base, "/casos-ma/?wpdmdl=1256&refresh=630e3af9252781661876985"))

v_desaparicion_forzada <- read.xlsx(paste(enlace_base, "/victimas-df/?wpdmdl=1269&refresh=630e3af8b66441661876984"))
c_desaparición_forzada <- read.xlsx(paste(enlace_base, "/casos-desaparicion-forzada/?wpdmdl=1255&refresh=630e3af92e7e51661876985"))

v_daño_bienes <- read.xlsx(paste(enlace_base, "/victimas-danos-bienes-civiles/?wpdmdl=1268&refresh=630e3af8bf7d61661876984"))
c_daño_bienes <- read.xlsx(paste(enlace_base, "/casos-danos-bienes-civiles/?wpdmdl=1254&refresh=630e3af937f681661876985"))

v_atentados_terroristas <- read.xlsx(paste(enlace_base, "/victimas-atentados-terroristas/?wpdmdl=1267&refresh=630e3af8c8b041661876984"))
c_atentados_terroristas <- read.xlsx(paste(enlace_base, "/casos-atentados-terroristas/?wpdmdl=1253&refresh=630e3af941ae31661876985"))

v_asesinatos_selectivos <- read.xlsx(paste(enlace_base, "/victimas-asesinatos-selectivos/?wpdmdl=1266&refresh=630e3af8d22131661876984"))
c_asesinatos_selectivos <- read.xlsx(paste(enlace_base, "/casos-asesinatos-selectivos/?wpdmdl=1252&refresh=630e3af94ac161661876985"))

v_ataques_poblacionales <- read.xlsx(paste(enlace_base, "/victimas-ataques-poblaciones/?wpdmdl=1262&refresh=630e3af8dc4331661876984"))
c_ataques_poblacionales <- read.xlsx(paste(enlace_base, "/casos-ataques-poblaciones/?wpdmdl=1242&refresh=630e3af953de61661876985"))

v_acciones_belicas <- read.xlsx(paste(enlace_base, "/victimas-acciones-belicas/?wpdmdl=1261&refresh=630e3af8e63271661876984"))
c_acciones_belicas <- read.xlsx(paste(enlace_base, "/casos-acciones-belicas/?wpdmdl=1240&refresh=630e3af95ce351661876985"))




## Junto y guardo bases

variables <- c("ID.Caso", "Año", "Mes", "Día", "Departamento", "Modalidad", "Total.de.Víctimas.del.Caso", "Latitud", "Longitud")



violencia_sexual <- c_violencia_sexual%>%
  filter(Año %in% 2019:2021 & Departamento != "EXTERIOR")%>%
  select(all_of(variables))%>%
  mutate(Tipo_violencia = "Violencia sexual")

secuestro <- c_secuestro%>%
  filter(Año %in% 2019:2021 & Departamento != "EXTERIOR")%>%
  select(all_of(variables))%>%mutate(Tipo_violencia = "Secuestro")

reclutamiento_NNA <- c_reclutamiento_NNA%>%
  filter(Año %in% 2019:2021 & Departamento != "EXTERIOR")%>%
  select(all_of(variables))%>%
  mutate(Tipo_violencia = "Reclutamiento NNA")

minas <- c_minas %>%
  filter(Año %in% 2019:2021 & Departamento != "EXTERIOR")%>%
  select(all_of(variables))%>%
  mutate(Tipo_violencia = "Minas antipersonal")

masacres <- c_masacres%>%
  filter(Año %in% 2019:2021 & Departamento != "EXTERIOR")%>%
  select(all_of(variables))%>%
  mutate(Tipo_violencia = "Masacres")

desaparicion <- c_desaparición_forzada %>%
  filter(Año %in% 2019:2021 & Departamento != "EXTERIOR")%>%
  select(all_of(variables))%>%
  mutate(Tipo_violencia = "Desaparición forzada")


daño_bienes <- c_daño_bienes%>%
  filter(Año %in% 2019:2021 & Departamento != "EXTERIOR")%>%
  select(all_of(variables))%>%
  mutate(Tipo_violencia = "Daño a bienes civiles")


atentados_terroristas <- c_atentados_terroristas%>%
  filter(Año %in% 2019:2021 & Departamento != "EXTERIOR")%>%
  select(all_of(variables))%>%
  mutate(Tipo_violencia = "Atentados terroristas")

asesinatos_selectivos <- c_asesinatos_selectivos%>%
  filter(Año %in% 2019:2021 & Departamento != "EXTERIOR")%>%
  select(all_of(variables))%>%
  mutate(Tipo_violencia = "Asesinatos selectivos")

ataques_poblacionales <- c_ataques_poblacionales %>%
  filter(Año %in% 2019:2021 & Departamento != "EXTERIOR")%>%
  select(all_of(variables[variables!="Modalidad"]))%>%
  mutate(Modalidad = "ATAQUE POBLACIONAL")%>%
  mutate(Tipo_violencia = "Ataques poblacionales")

acciones_belicas <- c_acciones_belicas%>%
  filter(Año %in% 2019:2021 & Departamento != "EXTERIOR")%>%
  select(all_of(variables))%>%
  mutate(Tipo_violencia = "Acciones bélicas")


casos_violencia <- rbind(violencia_sexual, 
                         secuestro, 
                         reclutamiento_NNA, 
                         minas, 
                         masacres, 
                         desaparicion, 
                         daño_bienes, 
                         atentados_terroristas,
                         asesinatos_selectivos, 
                         ataques_poblacionales, 
                         acciones_belicas)

write.csv(casos_violencia, "Data/Violencias/casos_violencia.csv")


violencia_sexual <- full_join(v_violencia_sexual, c_violencia_sexual)%>%
  mutate(Tipo_violencia = "Violencia sexual")%>%
  write.csv("Data/Violencias/violencia_sexual.csv")

secuestro <- full_join(v_secuestro, c_secuestro)%>%
  mutate(Tipo_violencia = "Secuestro")%>%
  write.csv("Data/Violencias/secuestro.csv")

reclutamiento_NNA <- full_join(v_reclutamiento_NNA, c_reclutamiento_NNA)%>%
  mutate(Tipo_violencia = "Reclutamiento NNA")%>%
  write.csv("Data/Violencias/reclutamiento_NNA.csv")

minas <- full_join(v_minas, c_minas)%>%
  mutate(Tipo_violencia = "Minas antipersonal")%>%
  write.csv("Data/Violencias/minas.csv")

masacres <- full_join(v_masacres, c_masacres)%>%
  mutate(Tipo_violencia = "Masacres")%>%
  write.csv("Data/Violencias/masacres.csv")

desaparicion_forzada <- full_join(v_desaparicion_forzada, c_desaparición_forzada)%>%
  mutate(Tipo_violencia = "Desaparición forzada")%>%
  write.csv("Data/Violencias/desaparición_forzada.csv")

daño_bienes <- full_join(v_daño_bienes, c_daño_bienes)%>%
  mutate(Tipo_violencia = "Daño a bienes civiles")%>%
  write.csv("Data/Violencias/daño_bienes.csv")

atentados_terroristas <- full_join(v_atentados_terroristas, c_atentados_terroristas)%>%
  mutate(Tipo_violencia = "Atentados terroristas")%>%
  write.csv("Data/Violencias/atentados_terroristas.csv")

asesinatos_selectivos <- full_join(v_asesinatos_selectivos, c_asesinatos_selectivos)%>%
  mutate(Tipo_violencia = "Asesinatos selectivos")%>%
  write.csv("Data/Violencias/asesinatos_selectivos.csv")

ataques_poblacionales <- full_join(v_ataques_poblacionales, c_ataques_poblacionales)%>%
  mutate(Tipo_violencia = "Ataques poblacionales")%>%
  write.csv("Data/Violencias/ataques_poblacionales.csv")

acciones_belicas <- full_join(v_acciones_belicas, c_acciones_belicas)%>%
  mutate(Tipo_violencia = "Acciones bélicas")%>%
  write.csv("Data/Violencias/acciones_belicas.csv")

