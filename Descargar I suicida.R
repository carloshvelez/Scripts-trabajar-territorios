enlace_base <- "http://portalsivigila.ins.gov.co/VigilanciaRutinaria"

is_2016 <- openxlsx::read.xlsx(paste(enlace_base, "/rutinaria_2016.xlsx", sheet = 4))
is_2016 <- is_2016 %>% filter(Nombre=="INTENTO DE SUICIDIO")

is_2017 <- openxlsx::read.xlsx(paste(enlace_base, "/rutinaria_2017.xlsx", sheet = 4))
is_2017 <- is_2017 %>% filter(Nombre=="INTENTO DE SUICIDIO")

is_2018 <- openxlsx::read.xlsx(paste(enlace_base, "/rutinaria_2018.xlsx", sheet = 4))
is_2018 <- is_2018 %>% filter(Evento=="INTENTO DE SUICIDIO")

is_2019 <- openxlsx::read.xlsx(paste(enlace_base, "/rutinaria_2019.xlsx", sheet = 3))
colnames(is_2019)<- is_2019[1,]
is_2019 <- is_2019[which(is_2019$NOM_EVE=="INTENTO DE SUICIDIO"):(which(is_2019$NOM_EVE=="Total INTENTO DE SUICIDIO")-1),]

is_2020 <- openxlsx::read.xlsx(paste(enlace_base, "/rutinaria_2020.xlsx", sheet = 3))
colnames(is_2020)<- is_2020[2,]
is_2020 <- is_2020[which(is_2020$NOM_EVE=="INTENTO DE SUICIDIO"):(which(is_2020$NOM_EVE=="Total INTENTO DE SUICIDIO")-1),]

is_2021 <- openxlsx::read.xlsx(paste(enlace_base, "/rutinaria_2021.xlsx", sheet = 1))
colnames(is_2021)<- is_2021[1,]
is_2021 <- is_2021%>%filter(evento=="INTENTO DE SUICIDIO")
