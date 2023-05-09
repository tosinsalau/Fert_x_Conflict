#Get relevant actors

pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/'

load(paste0(pth, 'trip_BEV.rda'))

#subset actors to only those with ten battle events and at least 50 fatalities
fat_actors = trip_BEV[(trip_BEV$fatalities>=10), ]

#begin to get out actors: for actor1
char_freq= table(trip_BEV$actor1)

nam = which(char_freq >5)

chg= unique(names(nam))

##do for actor 2

fhar_freq= table(trip_BEV$actor2)

fam = which(fhar_freq >5)

fhg= unique(names(fam))

#do for assoc actors..... should this be more than 5 in assoc_actors or in both actors and assoc_actors

vhar_freq= table(trip_BEV$assoc_actor_1)

vam = which(vhar_freq >5)

vhg= unique(names(vam))

##do for assoc_actor 2

thar_freq= table(trip_BEV$assoc_actor_2)

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
act_nopo= as.data.frame(act_nopo)
list_fat= as.data.frame(list_fat)

write.csv(act_nopo, paste0(pth, "act_nopo.csv"))
write.csv(list_fat, paste0(pth, "list_fat.csv"))

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
