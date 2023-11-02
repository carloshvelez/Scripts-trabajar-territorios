library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(broom)

folder <- "Data"
archivo_1 <- "población.csv"
archivo_2 <- "población_2005-2017.csv"
archivo_3 <- "Víctimas_por_año.csv"


población_1 <- read.csv(file.path(folder, archivo_1), sep=";", dec=",", strip.white = T)
población_2 <- read.csv(file.path(folder, archivo_2), sep=";", dec=",", strip.white = T)


población_1 <- población_1%>%
  filter(AÑO %in% 2018:2021 & ÁREA.GEOGRÁFICA == "Total")%>%
  select(c(1,2,3,5))

población_2 <- población_2%>%
  filter(AÑO ==2017 & ÁREA.GEOGRÁFICA == "Total")%>%
  select(c(1,2,3,5))

poblacion <- rbind(población_1, población_2)

colnames(poblacion)<- c("id_depto", "departamento", "año", "total")

rips <- read.csv("C:/Users/Yudy/Downloads/Registros_Individuales_de_Prestaci_n_de_Servicios_de_Salud___RIPS.csv")

rips <- rips %>% 
  filter(Año %in% 2017:2022 & str_detect(Diagnostico, "^F|^Z5[5-9]|^Z6[0-5]"))






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



rips$depa[rips$id_depto == 91] <- "Amazonas"
rips$depa[rips$id_depto == 94] <- "Guainía"
rips$depa[rips$id_depto == 95] <- "Guaviare"
rips$depa[rips$id_depto == 86] <- "Putumayo"
rips$depa[rips$id_depto == 88] <- "San andrés y providencia"
rips$depa[rips$id_depto == 91] <- "Amazonas"
rips$depa[rips$id_depto == 97] <- "Vaupés"
rips$depa[rips$id_depto == 99] <- "Vichada"

colnames(rips) <- c("año", "id_depto", "departamento", "tipo_atencion", "diagnostico", "n_atenciones", "clasificacion")

rips <- left_join(rips, poblacion[c(1,3,4)], by = c("año", "id_depto"))

rips_año <- rips%>%
  group_by(año, departamento, id_depto, total)%>%
  summarise(n_atenciones = sum(n_atenciones))

write.csv(rips, "DATA/rips_año_dx_clas_2017-2021.csv")
write.csv(rips_año, "DATA/rips_año.csv")

rips_año <- read.csv("DATA/rips_año.csv")

#victimas<- read_delim("victimas_año.csv", 
 #                              delim = ";", escape_double = FALSE, trim_ws = TRUE
  #                             )
victimas <- read_delim("victimas_año.csv", 
                      delim = ",", escape_double = FALSE, trim_ws = TRUE, skip=1
                      )


eventos_año <- victimas %>% 
  mutate(EVENTOS = parse_number(EVENTOS))%>%
  filter(VIGENCIA %in% (2017:2021))%>%group_by(VIGENCIA, COD_ESTADO_DEPTO, ESTADO_DEPTO)%>%summarise(eventos = sum(EVENTOS))

eventos_año[eventos_año$id_depto == 0,2] <- -1

colnames(eventos_año) <- c("año", "id_depto", "departamento", "n_eventos")

df <- right_join(rips_año[c(2,4,6)], eventos_año, by = c("año", "id_depto")) %>%full_join(poblacion[c(1,3,4)], by=c("año", "id_depto"))

df <- df%>%mutate(total = str_replace_all(total, "\\.", ""), 
            total = parse_number(total),
            tasa_eventos = n_eventos/(total/100000),
            tasa_atenciones = n_atenciones/(total/100000))

df2 <- df%>%group_by(id_depto)%>%summarise(eventos = sum(tasa_eventos),
                                           atenciones = sum(tasa_atenciones))

write.csv(df, "DATA/victimas_atenciones_2017-2021.csv")


pearson <- df%>%
  group_by(año)%>%
  summarise(tidy(cor.test(tasa_atenciones, tasa_eventos, method = "pearson")))%>%
  select(c(1, 2, 4))%>%
  round(3)

spearman <- df4%>%
  group_by(año)%>%
  summarise(tidy(cor.test(tasa_atenciones, tasa_eventos, method = "spearman")))%>%
  select(c(1, 2, 4))%>%
  round(3)

t_pearson <- df%>%
  summarise(tidy(cor.test(tasa_atenciones, tasa_eventos, method = "pearson")))%>%
  select(c(1,3))%>%mutate(año = 0)%>%
  round(3)

t_spearman <- df%>%
  summarise(tidy(cor.test(tasa_atenciones, tasa_eventos, method = "spearman")))%>%
  select(c(1,3))%>%
  mutate(año = 0)%>%
  round(3)

t_correlaciones <- right_join(t_pearson, t_spearman, by = "año")

t_correlacioes <- t_correlaciones[c(3,1,2,4,5)]

correlaciones <- right_join(pearson, spearman, by = "año")

correlaciones <- rbind(correlaciones, t_correlaciones)

colnames(correlaciones) <- c("año", "R", "P R", "Rho", "P Rho")

write.csv(correlaciones, "DATA/correlaciones_atenciones_eventos.csv")
