#collab data and robustness dat

library(readr)
library(dplyr)
library(lubridate)
library(countrycode)

pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/'

Collab= read_csv(paste0(pth, "Collab_dat.csv"))
rob_collab= read_csv("C:/Users/salau/OneDrive - Michigan State University/Research/Collab_2.csv")

load(paste0(pth, 'acled_2001_2020.rda'))
Collab= Collab[, -1]
rob_collab= rob_collab[, -1]


###geberarte isos, country name

Collab_final= merge(Collab, acled[, c("country",
                                      "iso3",
                                      "event_id_cnty", 
                                      "event_id_no_cnty", 
                              "iso")],
            by=c("event_id_cnty", "event_id_no_cnty"),all.x =TRUE)

rob_final = merge(rob_collab, acled[, c("country",
                                    "iso3",
                                    "event_id_cnty", 
                                    "event_id_no_cnty", 
                                    "iso")],
                  by=c("event_id_cnty", "event_id_no_cnty"),all.x =TRUE)

Collab_final$cname = countrycode(Collab_final$country, 'country.name', 'country.name')
rob_final$cname = countrycode(rob_final$country, 'country.name', 'country.name')

#create year variable
Collab_final= Collab_final %>% 
  mutate(year= substr(mon_yr, 1, 4))

rob_final= rob_final %>% 
  mutate(year= substr(mon_yr, 1, 4))

save(Collab_final, file=paste0(pth, "Final_Data_collab.rda"))
save(rob_final, file=paste0(pth, "Final_Data_robust_1.rda"))
