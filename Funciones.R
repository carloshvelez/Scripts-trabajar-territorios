### Para calcular fechas asociadas a semanas epidemiológicas, sin error, tuve que eliminar una alerta de la función original (wktmo::dateFromWeek()). Esto no afecta el resultado

Última_fecha_semana_epi <- function(year, wkIndex, wkMethod = "ISO") 
{
  if (wkMethod != "ISO" & wkMethod != "epiSat" & wkMethod != 
      "epiSun" & wkMethod != "epiMon") {
    warning("Please check the week method. Should be 'ISO', 'epiSat', 'epiSun' or\n'epiMon'.")
    stop()
  }
  if (class(year) != "numeric") {
    warning("Please check 'year'. Should be a numeric value")
    stop()
  }
  if (class(wkIndex) != "numeric") {
    warning("Please check 'wkIndex'. Should be a numeric value")
    stop()
  }
  if (wkMethod != "ISO") {
    if (wkIndex < 0.5 | wkIndex > 52.49) {
      warning("wkIndex should be between 1 and 52.")
      stop()
    }
  }
  else {
    if (wkIndex < 0.5 | wkIndex > 53.49) {
      warning("wkIndex should be between 1 and 53.")
    }
  }
  year <- round(year[1], 0)
  wkIndex = round(wkIndex[1], 0)
  day1 <- strptime(paste(1, 1, year, sep = "-"), format = "%d-%m-%Y")
  day1w <- day1$wday
  dayStep <- 0
  if (wkMethod == "ISO") {
    if (day1w == 1 | day1w == 2 | day1w == 3 | day1w == 4) {
      dayStep <- 1 - day1w
    }
    else {
      if (day1w == 0) {
        dayStep <- 1
      }
      else {
        dayStep <- 8 - day1w
      }
    }
    w1 <- day1 + dayStep * 86400
    w53d4 <- w1 + 86400 * 52 * 7 + 3 * 86400
    
  }
  if (wkMethod == "epiSat") {
    if (day1w == 0 | day1w == 1 | day1w == 2) {
      dayStep <- -1 - day1w
    }
    if (day1w == 3 | day1w == 4 | day1w == 5) {
      dayStep <- 6 - day1w
    }
  }
  if (wkMethod == "epiSun") {
    if (day1w == 0 | day1w == 1 | day1w == 2) {
      dayStep <- -day1w
    }
    else {
      dayStep <- 7 - day1w
    }
  }
  if (wkMethod == "epiMon") {
    if (day1w == 1 | day1w == 2 | day1w == 3 | day1w == 4) {
      dayStep <- 1 - day1w
    }
    else {
      if (day1w == 0) {
        dayStep <- 1
      }
      else {
        dayStep <- 8 - day1w
      }
    }
  }
  w1 <- day1 + dayStep * 86400
  wxd1 <- w1 + 86400 * 7 * (wkIndex - 1)
  return(as.Date(wxd1 + 86400 * (0:6)))
}
