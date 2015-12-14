Sys.setenv(plotly_username="watanabe8760")
Sys.setenv(plotly_api_key="tze8yu7ial")

station <- readRDS('./rds/station.rds')

getStationLabel <- function(city) {
  station.label <- as.list(station$id[station$landmark == city])
  names(station.label) <- sapply(which(station$landmark == city), 
                                 FUN=function(i) {
                                   paste(station$id[i], 
                                         ':',
                                         station$name[i])
                                 })
    
  return(station.label)
}