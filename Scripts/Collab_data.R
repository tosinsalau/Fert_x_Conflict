library(readr)
library(dplyr)

pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/'

Collab= read_csv(paste0(pth, "Collab_dat.csv"))
load(paste0(pth, 'acled_2001_2020.rda'))
Collab= Collab[, -1]


###country year collabs

Collab_final= merge(Collab, acled[, c("iso3",
                                      "event_id_cnty", 
                                      "event_id_no_cnty", 
                              "iso")],
            by=c("event_id_cnty", "event_id_no_cnty"),all.x =TRUE)


save(Collab_final, file=paste0(pth, "Final_Data_collab.rda"))
