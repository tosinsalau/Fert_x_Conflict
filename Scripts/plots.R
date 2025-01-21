library(readr)
library(dplyr)
library(modelsummary)
library(MASS)
library(pscl)
library(RColorBrewer)
library(ggplot2)
library(lubridate)
pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/All_Data/'

pth2= "C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/Nigerian_states/"


load(paste0(pth, 'dat.rda'))
load(paste0(pth, 'dat.rda'))
load(paste0(pth2, 'Urea_YR.rda'))

# dat <- dat %>% 
#   filter(country != "Malawi")
# 
# save(dat, file = paste0(pth, "dat.rda"))

#descriptive stats

# tmp= dat[, c("Subsidy_price1", "GDP_per_capita", "total_pop", "pop_0_14", "life_exp",
#              "mort_rate", "rur_tot_pop","democ", "autoc", "pop_growth", "civ_war")]

tmp= dat[, c("Subsidy_price1","mort_rate", "rur_tot_pop", "Elec_yr", "logGDP", "total_collab", "autoc")]

#tmp= dat[, c("Subsidy_price1","mort_rate", "rur_tot_pop", "GDP_per_capita")]

tmp_renamed <- tmp %>% rename("Subsidy Price" = Subsidy_price1,
                              "Mortality Rate" = mort_rate, 
                              "Rural population (% of total)" = rur_tot_pop,
                              "Election Year"= Elec_yr,
                              "Log(GDP per capita)" = logGDP,
                              "Collaborative Protest" = total_collab)
                             

#create missing function 


Range <- function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)

Missing = function(x) sum(is.na(x))



# data_summary <- datasummary_skim(tmp_renamed)
# datasummary_skim(dat)

datasummary(All(tmp_renamed) ~ Mean + SD + Min + Median + Max + Histogram + Missing, 
            data = tmp_renamed)


ggplot(dat, aes(Subsidy_price1,total_collab))+
  geom_point()+
  xlab("Amount of Subsidy ($)") + 
  ylab("Number of Collaborative Protests") + # Set axis label
  theme_bw()+
  facet_wrap(~cname)
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
  
  
  ggplot(Urea_YR, aes(year,subsidy))+
    geom_point()+
    xlab("") + 
    ylab("Amount of Subsidy ($)") + # Set axis label
    scale_x_continuous(breaks= c(2010, 2012, 2014, 2016, 2018, 2020))+
    theme_bw()+
    facet_wrap(~Area)+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
  
  ggplot(dat, aes(year,Subsidy_price1))+
    geom_line()+
    xlab("") + 
    ylab("Amount of Subsidy ($)") + # Set axis label
    scale_x_continuous(breaks= c(2010, 2012, 2014, 2016, 2018, 2020))
   
  
  ggplot(Urea_YR, aes(subsidy,weight))+
    geom_point()+
    xlab("") + 
    ylab("") + # Set axis label
    #scale_x_continuous(breaks= c(2010, 2012, 2014, 2016, 2018, 2020))+
    theme_bw()+
    facet_wrap(~State)
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
  
  
  andr= dat %>% 
    group_by(cname) %>% 
    summarise(avgg = mean(Subsidy_price1))
  
  andr2= dat %>% 
    group_by(cname) %>% 
    summarise(allt = sum(total_collab))
  
 
  
   
  

ggplot(Urea_MY, aes(subsidy, weight))+
  geom_point()+
  xlab("Amount of Subsidy ($)") + 
  ylab("Number of Collaborative Protests") + # Set axis label
  theme_bw()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



ggplot(dua, aes(avgg))+
  geom_point()+
  xlab("Amount of Subsidy ($)") + 
  #ylab("Number of Collaborative Protests") + # Set axis label
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
  facet_wrap(~cname, scales = "free")

ggplot(dat, aes(year,USDprice)) +
  geom_line() +
  scale_x_continuous(breaks= c(2010, 2012, 2014, 2016, 2018, 2020))+
  facet_wrap(~cname)+
  xlab("") + 
  ylab("USDprice")

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



