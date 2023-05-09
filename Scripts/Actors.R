#Get relevant actors

pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fertilizer_Conflict/'

load(paste0(pth, 'trip_BEV.rda'))

#subset actors to only those with ten battle events and at least 50 fatalities


fat_actors = trip_BEV[(trip_BEV$fatalities>=50), ] 10?

#begin to get out actors: for actor1
char_freq= table(trip_BEV$actor1)

nam = which(char_freq >10)

chg= unique(names(nam))

##do for actor 2

fhar_freq= table(trip_BEV$actor2)

fam = which(fhar_freq >10)

fhg= unique(names(fam))


#do for assoc actors 1
char_freq= table(trip_BEV$actor1)

nam = which(char_freq >10)

chg= unique(names(nam))

#do for assoc actors 2

char_freq= table(trip_BEV$actor1)

nam = which(char_freq >10)

chg= unique(names(nam))


#combine unique actors in both fhg and chg.. 
comb_act= unique(c(chg, fhg))

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

# Apply the code to each element in the list
act_nopo = lapply(list_fat, function(x) 
  x[!grepl(paste(c("Military", "Police"), collapse = "|"), x)]
)

# Convert the list back to a vector
act_nopo = unlist(act_nopo)

#save=== actors without police/military, involved in 50 fatals, appear more than 10ce
act_nopo= as.data.frame(act_nopo)

write.csv(act_nopo, paste0(pth, "act_nopo.csv"))
write.csv(act_nopo, paste0(pth, "act_nopo_ERV.csv"))
write.csv(act_nopo, paste0(pth, "act_nopo_VC.csv"))
write.csv(act_nopo, paste0(pth, "act_nopo_BEV.csv"))



#trial
newbe= read_csv(paste0(pth, "BEV_cn_v1.csv"))

newbe= read_csv(paste0(pth, "BEV_cn_v1.csv"))
tryg= newbe[1:3,]


# Split comma-separated values and create data frames
a_df <- data.frame(word = unlist(strsplit(trip_BEV$assoc_actor_1, ";")))

b_df <- data.frame(word = unlist(strsplit(trip_BEV$assoc_actor_2, ";")))

# Perform expand.grid to get all possible combinations
result <- expand.grid(unique(a_df$word), unique(b_df$word))

# Rename columns
colnames(result) <- c("a", "b")
