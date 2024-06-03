#Getting data/subset

library(dplyr)
library(lubridate)
library(readr)
library(haven)
library(ggplot2)


pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/'

load(paste0(pth, 'acled_2001_2020.rda'))

###get subset of african countries
acf_c= c("Somalia", "Angola", "Mali", "Ivory Coast", "Democratic Republic of Congo", 
         "Algeria", "Libya", "Cameroon", "Sudan", "Ethiopia", "Morocco", "Guinea", "Niger", "Nigeria",
         "South Africa", "Uganda", "Kenya", "Egypt", "Mozambique", "Chad", "Zimbabwe", "Ghana", "Burundi",
         "Burkina Faso", "Malawi", "South Sudan", "Central African Republic", "Liberia", "Senegal", "Zambia",
         "Madagascar", "Namibia", "Benin", "Republic of Congo", "eSwatini", "Gambia", "Gabon", 
         "Guinea-Bissau", "Togo", "Eritrea", "Tanzania", "Rwanda", "Djibouti", "Lesotho",
         "Botswana", "Sierra Leone", "Equatorial Guinea")

africa= acled[acled$country %in% acf_c, ]


#format date to yr _month in africa
acledipe= africa %>% 
  mutate(date2=ymd(event_date))

acledipe = acledipe %>% 
  mutate(mon_yr=format_ISO8601(date2, precision = "ym"))

#subset to violent events

trip_BEV=acledipe[(acledipe$event_type %in% c("Battles", "Explosions/Remote violence",
                                              "Violence against civilians")),]

save(trip_BEV, file =paste0(pth, "trip_BEV.rda"))
save(acledipe, file=paste0(pth, "acledipe.rda"))
