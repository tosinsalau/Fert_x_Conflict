library(readr)
library(dplyr)
library(lubridate)
library(countrycode)

pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/'

Collab= read_csv(paste0(pth, "Collab_dat.csv"))
load(paste0(pth, 'acled_2001_2020.rda'))
Collab= Collab[, -1]


###geberarte isos, country name

Collab_final= merge(Collab, acled[, c("country",
                                      "iso3",
                                      "event_id_cnty", 
                                      "event_id_no_cnty", 
                              "iso")],
            by=c("event_id_cnty", "event_id_no_cnty"),all.x =TRUE)

Collab_final$cname = countrycode(Collab_final$country, 'country.name', 'country.name')
#create year variable
Collab_final= Collab_final %>% 
  mutate(year= substr(mon_yr, 1, 4))


save(Collab_final, file=paste0(pth, "Final_Data_collab.rda"))
