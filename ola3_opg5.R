#### Pakker ####
library(httr)
library(jsonlite)
library(tidyverse)

#### Opgave 5.2 ####

#### API-key ####
dmi_key <- "22ca7c05-dc44-4fd5-815f-cd3b7ec2fc7c"

## Hent stationer
dmi_stationer <- GET(
  url = "https://dmigw.govcloud.dk/v2/metObs/collections/station/items?api-key=22ca7c05-dc44-4fd5-815f-cd3b7ec2fc7c",
)
dmi_stationer$status_code

## Hent content som text
dmicontent <- httr::content(dmi_stationer, as = "text")
dmicontentJSON <- jsonlite::fromJSON(dmicontent)

## Vælg features og lav om til dataframe
station_df <- as.data.frame(dmicontentJSON$features)
View(station_df)

## Sorter efter stationid og find tilsvarende by
by <- station_df %>%
  filter(properties$stationId == "05272")
print(by$properties$name)


#### Opgave 5.3 ####

## Hent forecast API collections
forecast_key <- "b0cf30bc-9f91-4eff-a85a-6bfddaa5f943"
forecast <- "https://dmigw.govcloud.dk/v1/forecastdata/collections"
forecast_res <- GET(forecast, add_headers("X-Gravitee-Api-Key" = forecast_key))
forecast_JSON <- fromJSON(content(forecast_res, "text"))


## Lav collection om til en dataframe for overskuelighed
forecast_df <- as.data.frame(forecast_JSON$collections) 
nrow(forecast_df) ## antal collections
# 18

## Hvilket ID har Lille Bælt? - dkss_lb - søg på littlebelt i dataframe
## Filformat - GRIB eller JSON? Alt efter API


#### Opgave 5.4 ####
## Anholt stationID: 06079
## Århus stationID: 06074
## ID er hentet fra dataframen fra opgave 5.2

###### Indhentning af vindhastighed data ######
## Stormen hærgede 20-21 oktober 

###### Anholt  ######

dmi_key <- "22ca7c05-dc44-4fd5-815f-cd3b7ec2fc7c"
anholt <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?datetime=2023-10-18T00%3A00%3A00Z%2F2023-10-21T12%3A31%3A12Z&stationId=06079&parameterId=wind_speed_past1h&bbox-crs=https%3A%2F%2Fwww.opengis.net%2Fdef%2Fcrs%2FOGC%2F1.3%2FCRS84"
anholt_res <- GET(anholt, add_headers("X-Gravitee-Api-Key" = dmi_key))
anholt_res$status_code
anholt_JSON <- fromJSON(content(anholt_res, "text"))

## Lav om til en dataframe
anholt_df <- as.data.frame(anholt_JSON$features) 

## POSIXct time
anholt_df$properties$observed <- as.POSIXct(anholt_df$properties$observed, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

## Vis class, skulle gerne printe POSIXct
class(anholt_df$properties$observed)



###### Århus ######

århus <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?datetime=2023-10-18T00%3A00%3A00Z%2F2023-10-21T12%3A31%3A12Z&stationId=06074&parameterId=wind_speed_past1h&bbox-crs=https%3A%2F%2Fwww.opengis.net%2Fdef%2Fcrs%2FOGC%2F1.3%2FCRS84"
århus_res <- GET(århus, add_headers("X-Gravitee-Api-Key" = dmi_key))
århus_res$status_code
århus_JSON <- fromJSON(content(århus_res, "text"))

## Lav om til en dataframe
århus_df <- as.data.frame(århus_JSON$features) 



###### Indhentning af vindretning data ######

###### Anholt ######

anholt_dir <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?datetime=2023-10-18T00%3A00%3A00Z%2F2023-10-21T12%3A31%3A12Z&stationId=06079&parameterId=wind_dir_past1h&bbox-crs=https%3A%2F%2Fwww.opengis.net%2Fdef%2Fcrs%2FOGC%2F1.3%2FCRS84"
anholt_dir_res <- GET(anholt_dir, add_headers("X-Gravitee-Api-Key" = dmi_key))
anholt_dir_res$status_code
anholt_dir_JSON <- fromJSON(content(anholt_dir_res, "text"))

## Lav om til en dataframe
anholt_dir_df <- as.data.frame(anholt_dir_JSON$features) 

###### Århus ######

århus_dir <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?datetime=2023-10-18T00%3A00%3A00Z%2F2023-10-21T12%3A31%3A12Z&stationId=06074&parameterId=wind_dir_past1h&bbox-crs=https%3A%2F%2Fwww.opengis.net%2Fdef%2Fcrs%2FOGC%2F1.3%2FCRS84"
århus_dir_res <- GET(århus_dir, add_headers("X-Gravitee-Api-Key" = dmi_key))
århus_dir_res$status_code
århus_dir_JSON <- fromJSON(content(århus_dir_res, "text"))

## Lav om til en dataframe
århus_dir_df <- as.data.frame(århus_dir_JSON$features) 




## Samler dato, vindhastigheder og retning i én df

storm_df <- data.frame(Dato = anholt_df$properties$observed)
storm_df$Anholt_speed <- anholt_df$properties$value
storm_df$Århus_speed <- århus_df$properties$value
storm_df$Anholt_dir <- anholt_dir_df$properties$value
storm_df$Århus_dir <- århus_dir_df$properties$value

## Opretter vindretninger baseret på kompasgrader

convert_direction <- function(degree) {
  if (degree >= 337.5 | degree < 22.5) return("N")
  else if (degree >= 22.5 & degree < 67.5) return("NØ")
  else if (degree >= 67.5 & degree < 112.5) return("Ø")
  else if (degree >= 112.5 & degree < 157.5) return("SØ")
  else if (degree >= 157.5 & degree < 202.5) return("S")
  else if (degree >= 202.5 & degree < 247.5) return("SV")
  else if (degree >= 247.5 & degree < 292.5) return("V")
  else if (degree >= 292.5 & degree < 337.5) return("NV")
}


# Anvend funktionen på vindretninger
storm_df$Anholt_dir_label <- sapply(storm_df$Anholt_dir, convert_direction)
storm_df$Århus_dir_label <- sapply(storm_df$Århus_dir, convert_direction)

##### GGplot #####

ggplot(data = storm_df, aes(x = Dato))+
  geom_line(aes(y = Anholt_speed), color = "cornflowerblue", size = 0.7)+
  geom_line(aes(y = Århus_speed), color = "darkorange", size = 0.7)+
  geom_text(aes(y = Anholt_speed, label = Anholt_dir_label), size = 3)+
  geom_text(aes(y = Århus_speed, label = Århus_dir_label), size = 3)+
  labs(y = "Vindhastighed (m/s)", 
       title = "Stormen som hærgede i oktober 2023 kom øst fra",
       subtitle = "Væsentligt højere vindhastigheder på Anholt end i Århus",
       caption = "Datakilde: DMI")+
  theme_minimal()
