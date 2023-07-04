#GET/SAVE relevant info
library(dplyr)
library(ggplot2)
library(RColorBrewer)
pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/'


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

pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/All_Data/'

load(paste0(pth, 'Final_Data_collab.rda'))

#get countries
cnts = c("Burkina Faso", "Burundi", "Côte d’Ivoire", "Ghana", "Kenya", "Malawi",
              "Mali", "Mozambique","Namibia", "Nigeria", "Niger", "Rwanda", "Senegal", "Tanzania", 
              "Uganda")

gp_cnt=Collab_final[Collab_final$cname %in% cnts, ]

prots = gp_cnt %>%
  filter(year %in% 2010:2020) %>%
  group_by(cname) %>%
  summarise(weight = n()) %>% 
  arrange(weight)
  


nb.cols <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)


ggplot(prots, aes(x = cname, fill=cname)) +
  geom_bar(aes(y=weight), position = "dodge", stat = "identity")+
  scale_fill_manual(values = mycolors)+
  guides(fill= "none") +
  xlab("") + 
  ylab("Protests") + # Set axis labels
  ggtitle("Protest by Country 2010-2020") +     # Set title
  theme_bw()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



