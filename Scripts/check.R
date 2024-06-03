
pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/'

load(paste0(pth, 'trip_BEV.rda'))

#subset actors to only those with ten battle events and at least 50 fatalities
fat_actors = trip_BEV[(trip_BEV$fatalities>=10), ]

ghana= fat_actors %>% filter(country =="Ghana")

Kenya = fat_actors %>% filter(country =="Kenya")

Malawi = fat_actors %>% filter(country =="Malawi") #take malawi out

Rwanda = fat_actors %>% filter(country =="Rwanda")

Senegal = fat_actors %>% filter(country =="Senegal")


