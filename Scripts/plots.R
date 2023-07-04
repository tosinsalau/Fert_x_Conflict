library(readr)
library(dplyr)
library(modelsummary)
library(MASS)
library(pscl)
library(RColorBrewer)
library(ggplot2)
library(lubridate)
pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/All_Data/'

load(paste0(pth, 'dat.rda'))

#descriptive stats

tmp= dat[, c("Subsidy_price1", "GDP_per_capita", "total_pop", "pop_0_14", "life_exp",
             "mort_rate", "urb_tot_pop","democ", "autoc", "pop_growth", "civ_war")]

tmp_renamed <- tmp %>% rename("Subsidy Price" = Subsidy_price1,
                              "Total Population" = total_pop,
                              "Youth bulge(% of total 14 and under)" = pop_0_14,
                              " Life Expectancy" = life_exp,
                              "Mortality Rate" = mort_rate, 
                              "Urban population (% of total)" =urb_tot_pop,
                              "Polity V democracy" = democ,
                              "Polity V Autocracy" = autoc,
                              "Population growth" =pop_growth,
                              "Civil War Occurrence" = civ_war)

datasummary_skim(tmp_renamed)
datasummary_skim(dat)


ggplot(dat, aes(Subsidy_price1,total_collab))+
  geom_point()+
  xlab("Amount of Subsidy ($)") + 
  ylab("Number of Collaborative Protests") + # Set axis label
  theme_bw()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

hist(dat$Subsidy_price1)
hist(dat$total_collab)
table(dat$total_collab)
cor(dat$Subsidy_price1, dat$total_collab)

#USE of FE 
#there is variation in years

dat$year2 <- factor(dat$year)
ggplot(dat, aes(year2, Subsidy_price1))+ 
  geom_boxplot()+
  #geom_line()+
  xlab("") + 
  ylab("Amount of Subsidy ($)") + # Set axis labels
  ggtitle("Subsidy Over Time") +     # Set title
  theme_bw()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#for unit = not much variation

# Define the number of colors you want
nb.cols <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)
# Create a ggplot with 18 colors 
# Use scale_fill_manual
  
ggplot(dat, aes(cname, Subsidy_price1, fill = cname)) +
  geom_boxplot() +
  guides(fill= "none") +
  scale_fill_manual(values = mycolors)+
  xlab("") + 
  ylab("Amount of Subsidy ($)") + # Set axis labels
  ggtitle("Subsidy Levels per Country") +     # Set title
  theme_bw()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#prices ranging on average, theres outliers, theres variation within units of iv, 
 
####
ggplot(dat, aes(year, Subsidy_price1, total_collab)) +
  geom_line() +
  facet_wrap(~cname)


# # Load the reshape2 package
# library(reshape2)
# 
# dat_long <- melt(dat, id.vars = c("year", "cname"))
# 
# # Plotting the two y variables
# ggplot(dat_long, aes(year, value)) +
#   geom_line() +
#   facet_wrap(~cname, ncol = 1) +
#   aes(color = variable)


**PROTESTS**

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



