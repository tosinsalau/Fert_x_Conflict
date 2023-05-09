

# subset protest events to those in whihch a battle actor is involved
pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/'

load(paste0(pth, 'acledipe.rda'))

list_fat= read_csv(paste0(pth, "list_fat.csv"))

fat_10_actors = unlist(list_fat[2])

prots= acledipe[acledipe$event_type == 'Protests',]

prots = prots [
  prots $actor1 %in% fat_10_actors | 
    prots $actor2 %in% fat_10_actors |
    prots $assoc_actor_1 %in% fat_10_actors | 
    prots $assoc_actor_2 %in% fat_10_actors 
  ,]


a1 = prots[,c('actor1', "assoc_actor_1", 'actor2',"assoc_actor_2", 'mon_yr', 'notes')]

save(a1, file=paste0(pth, "a1.rda"))
