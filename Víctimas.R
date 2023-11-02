library(readxl)

victimas_año <- read_delim("victimas_año.csv", 
                           delim = ";", 
                           escape_double = FALSE, 
                           trim_ws = TRUE)

victimas_años <- victimas_año %>%
  filter(VIGENCIA %in% 2019:2021 & COD_ESTADO_DEPTO != 0)


victimas_años2 <- victimas_años%>%
  group_by(VIGENCIA, ESTADO_DEPTO, COD_ESTADO_DEPTO)%>%
  summarise(victimas = sum(EVENTOS))

colnames(victimas_años2)<- c("año", "dpto", "id_depto", "victimas_uv")
victimas_años2

df <- left_join(df, victimas_años2)

write.csv(victimas_años2, "victimas2019_2021.csv")
