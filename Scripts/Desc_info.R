#GET/SAVE relevant info

load(paste0(pth, 'trip_BEV.rda'))


# ~S descriptives
# - get no of battles for countries/or by country month_yr, 
# - get unique actors in country/by month

bats= trip_BEV %>% 
  group_by(country, mon_yr) %>% 
  summarise(weight=n())

uni=trip_BEV %>% 
  group_by(country, mon_yr) %>% 
  summarise(act= length(unique(c(actor1, actor2, assoc_actor_1, assoc_actor_2))))

#need to do for protests

