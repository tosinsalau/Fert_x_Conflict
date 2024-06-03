#Getting data/subset

library(dplyr)
library(lubridate)
library(readr)
library(haven)
library(ggplot2)


pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/'

load(paste0(pth, 'acled_2001_2020.rda'))

###get subset of african countries

Nigeria= acled[acled$country %in% "Nigeria", ]


#format date to yr _month in africa
Nig_data= Nigeria %>% 
  mutate(date2=ymd(event_date))

Nig_data = Nig_data%>% 
  mutate(mon_yr=format_ISO8601(date2, precision = "ym"))

#subset to violent events

trip_Nig=Nig_data[(Nig_data$event_type %in% c("Battles", "Explosions/Remote violence",
                                              "Violence against civilians")),]


#save(trip_Nig, file = paste0(pth, "trip_Nig.rda"))
save(Nig_data, file = paste0(pth, "Nig_data.rda"))
save(trip_Nig, file = paste0(pth, "trip_Nig.rda"))

ACTORS


#Get relevant actors

#subset actors to only those with ten battle events and at least 10 fatalities

load(paste0(pth, "trip_Nig.rda"))
fat_actors = trip_Nig[(trip_Nig$fatalities>=10), ]

#begin to get out actors: for actor1
char_freq= table(trip_Nig$actor1)

nam = which(char_freq >5)

chg= unique(names(nam))

##do for actor 2

fhar_freq= table(trip_Nig$actor2)

fam = which(fhar_freq >5)

fhg= unique(names(fam))

#do for assoc actors..... should this be more than 5 in assoc_actors or in both actors and assoc_actors

vhar_freq= table(trip_Nig$assoc_actor_1)

vam = which(vhar_freq >5)

vhg= unique(names(vam))

##do for assoc_actor 2

thar_freq= table(trip_Nig$assoc_actor_2)

tam = which(thar_freq >5)

thg= unique(names(tam))

#combine unique actors in both fhg and chg.. 
comb_act= unique(c(chg, fhg, vhg, thg))

# get rid of blanks and possible NAs
act_ov_10 = comb_act[!is.na(comb_act) & comb_act != '']

#STEP 1.. find act_ov 10 actors in fatalities .... so output is .. tehy appear more than 10 times in battles plus 50 and over fatals

over10fat = fat_actors[
  fat_actors$actor1 %in% act_ov_10 | 
    fat_actors$actor2 %in% act_ov_10 |
    fat_actors$assoc_actor_1 %in% act_ov_10 | 
    fat_actors$assoc_actor_2 %in% act_ov_10 
  ,]

#get actors from here and apply to protests

fat_10_actors = unique(
  c(
    over10fat$actor1, over10fat$actor2,
    over10fat$assoc_actor_1,
    over10fat$assoc_actor_2
  ) )

# get rid of blanks and possible NAs
fat_10_actors = fat_10_actors[!is.na(fat_10_actors) & fat_10_actors != '']

#ASIDE check= look for actors without police
# Create a list of characters
list_fat = list(fat_10_actors)

# Apply the code to each element in the list.... need for looking at armed groups
act_nopo = lapply(list_fat, function(x) 
  x[!grepl(paste(c("Military", "Police"), collapse = "|"), x)]
)

# Convert lists back to vector
act_nopo = unlist(act_nopo)
list_fat=unlist(list_fat)

#save=== actors without police/military, involved in 50 fatals, appear more than 10ce
Nig_act_nopo= as.data.frame(act_nopo)
Nig_list_fat= as.data.frame(list_fat)

write.csv(Nig_act_nopo, paste0(pth, "Nig_act_nopo.csv"))
write.csv(Nig_list_fat, paste0(pth, "Nig_list_fat.csv"))

# Split comma-separated values and create data frames
# a_df <- data.frame(word = unlist(strsplit(trip_BEV$assoc_actor_1, ";")))
# 
# b_df <- data.frame(word = unlist(strsplit(trip_BEV$assoc_actor_2, ";")))
# 
# # Perform expand.grid to get all possible combinations
# result <- expand.grid(unique(a_df$word), unique(b_df$word))
# 
# # Rename columns
#colnames(result) <- c("a", "b")


Protest data

# subset protest events to those in whihch a battle actor is involved
pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/'

load(paste0(pth, 'Nig_data.rda'))

Nig_list_fat= read_csv(paste0(pth, "Nig_list_fat.csv"))

Nig_fat_10_actors = unlist(Nig_list_fat[2])

prots= Nig_data[Nig_data$event_type == 'Protests',]

prots = prots [
  prots $actor1 %in% Nig_fat_10_actors | 
    prots $actor2 %in% Nig_fat_10_actors |
    prots $assoc_actor_1 %in% Nig_fat_10_actors | 
    prots $assoc_actor_2 %in% Nig_fat_10_actors 
  ,]


a1 = prots[,c('actor1', "assoc_actor_1", 'actor2',"assoc_actor_2", 'admin1', 'mon_yr', 'notes', 'event_id_cnty', 'event_id_no_cnty')]


Cleaning

ids = c('actor1', "assoc_actor_1", 'actor2',"assoc_actor_2")


for (id in ids) {
  a1[, id][which(grepl("Military Forces of Nigeria", a1[, id]))] = "Gov(Nigeria)"
  a1[, id][which(grepl("Police Forces of Nigeria ", a1[, id]))] = "Gov(Nigeria)"
}

a1 = a1[a1$actor1!=a1$actor2,]

#REMOVE ROWS THAT DONT HAVE AN ASSoC ACTOR TO FIGHT WITH
bat_Nig= a1[!c(a1$assoc_actor_1 == "", a1$assoc_actor_2 == "" ), ]

#export both
write.csv(bat_Nig, paste0(pth, "BEV_Nig.csv"))


