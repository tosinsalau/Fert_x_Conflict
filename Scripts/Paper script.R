#####New

#library
library(readr)
library(dplyr)
library(modelsummary)
library(MASS)
library(pscl)
library(RColorBrewer)
library(ggplot2)
library(lubridate)
library(readxl)
library(stargazer)
library(marginaleffects)
library(glmmADMB)
library(AER)
library(broom.mixed)
library(glmmTMB)
library(sjPlot)

#directory
pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/All_Data/'

pth2= "C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/Nigerian_states/"


#data

load(paste0(pth, 'dat.rda'))
load(paste0(pth, 'dat_rob.rda')) #robustness data- protests against the gov
load(paste0(pth, "protest_dat.rda"))  #noncollaborative protest data

load(paste0(pth2, 'Urea_YR.rda'))
load(paste0(pth2, 'Urea_MY.rda'))
elec_yr= read_excel("C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/elec_yr.xlsx")

elec_yr= rename(elec_yr, year = Year)

elec_yr= elec_yr[, -c(4:10)]

elec_yr$Country[elec_yr$Country == "Cote d'Ivoire"] <- 'Côte d’Ivoire'

dat= left_join(
  dat,
  elec_yr,
  by= c('year', 'Country')
)

#drop malawi

dat <- dat %>% 
  filter(country != "Malawi")

#gdp
dat$logGDP = log(dat$GDP_per_capita)
#Subnational models

model1=glm.nb(coop_events ~ subsidy, data=Urea_MY) ###1

model2=glm.nb(coop_events ~ subsidy, data=Urea_YR)


stargazer(model1, model2)

#Figure 2- prediction plot

plot_predictions(model1, condition='subsidy')+
  labs(y = "Probability of Collaborative Protest", x = 'Subsidy level') +
  theme_grey()+
  theme_bw()+
  theme(legend.position = 'bottom',
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank()) 

#Figure 3 - prediction plot 

plot_predictions(model2, condition='subsidy')+
  labs(y = "Probability of Collaborative Protest", x = 'Subsidy level') +
  theme_grey()+
  theme_bw()+
  theme(legend.position = 'bottom',
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank()) 


#Table 2 results for the pooled and random effects model

pool_mod = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop+ Elec_yr + elect_dem
                  + logGDP, data = dat)
summary(pool_mod)


pool_mod2 = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop+ Elec_yr + polity2 + Subsidy_price1 * polity2
                  + logGDP, data = dat)
summary(pool_mod2)

plot_predictions(
  pool_mod2, 
  condition=c('Subsidy_price1', 'polity2')
)
#Random effects ... need to install glmmADMB

# install.packages("R2admb")
# install.packages("glmmADMB", 
#                  repos=c("http://glmmadmb.r-forge.r-project.org/repos",
#                          getOption("repos")),
#                  type="source")

dat$fac_cn = as.factor(dat$cname)

glm_mod=glmmadmb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop  +
                   Elec_yr + logGDP
                 #log(GDP_per_capita)
                 + (1 | fac_cn),
                 data=dat, family ='nbinom') #polity not included
summary(glm_mod)



stargazer(pool_mod, glm_mod)

#Appendix


#Section B  - Table 1- descriptive stats country year

tmp= dat[, c("Subsidy_price1","mort_rate", "rur_tot_pop", "Elec_yr", "logGDP", "total_collab")]

tmp_renamed <- tmp %>% rename("Subsidy Price" = Subsidy_price1,
                              "Mortality Rate" = mort_rate, 
                              "Rural population (% of total)" = rur_tot_pop,
                              "Election Year"= Elec_yr,
                              "Log(GDP per capita)" = logGDP,
                              "Collaborative Protest" = total_collab)
#create missing function 
Missing = function(x) sum(is.na(x))

#datasummary
datasummary(All(tmp_renamed) ~ Mean + SD + Min + Median + Max + Missing, 
            data = tmp_renamed)

#Table 2- descriptive stats - subnational (month)

tmp_my= Urea_MY[, c("subsidy", "coop_events", "Urea_Intl", "USDprice")]

tmp_renamedmy <- tmp_my %>% rename("US Price" = USDprice,
                                    "International Price" = Urea_Intl,
                                    "Subsidy Price" = subsidy,
                                    "Collaborative Protest" = coop_events)


datasummary(All(tmp_renamedmy) ~ Mean + SD + Min + Median + Max + Missing, 
            data = tmp_renamedmy)


#Table 3- descriptive stats - subnational (year)

tmp_yr= Urea_YR[, c("subsidy", "coop_events", "USD_Int", "USDprice")]

tmp_renamedyr <- tmp_yr %>% rename("US Price" = USDprice,
                                   "International Price" = USD_Int,
                                   "Subsidy Price" = subsidy,
                                   "Collaborative Protest" = coop_events)


datasummary(All(tmp_renamedyr) ~ Mean + SD + Min + Median + Max + Missing, 
            data = tmp_renamedyr)

#Section C

#Figure 1- distribution plot

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


#b) Figue 2- subsidy price across states
ggplot(Urea_YR, aes(year,subsidy))+
  geom_point()+
  geom_line( color="steelblue")+
  xlab("") + 
  ylab("Amount of Subsidy ($)") + 
  scale_x_continuous(breaks= c(2010, 2012, 2014, 2016, 2018, 2020))+
  theme_bw()+
  facet_wrap(~State)+ 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())


#Section D  Table 4- Protests against Governments

pool_rob = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop+ Elec_yr
                  + logGDP, data = dat_rob, control=glm.control(maxit=1000))
summary(pool_rob)

stargazer(pool_rob)


#Table 5 -Linear Random and Fixed effects Model

data=dat[ , c("Country", "year", 'total_collab', 
              'Subsidy_price1', 'mort_rate', 'rur_tot_pop', 'Elec_yr', 'logGDP')]

# throw out missing
data = data[complete.cases(data),]

# add mean variables
data$Subsidy_price1_mean = data$mort_rate_1_mean = data$rur_tot_pop_1_mean = data$Elec_yr_1_mean = data$logGDP_1_mean= NA
countries = unique(data$Country)
head(data)
# loop through countries and get mean for each iv
for(i in 1:length(countries)){
  code = countries[i]
  subdata = data[data$Country==code,]
  data$Subsidy_price1_mean = ifelse(data$Country==code, mean(subdata$Subsidy_price1, na.rm=T), data$Subsidy_price1_mean)
  data$mort_rate_1_mean = ifelse(data$Country==code, mean(subdata$mort_rate, na.rm=T), data$mort_rate_1_mean)
  data$rur_tot_pop_1_mean = ifelse(data$Country==code, mean(subdata$rur_tot_pop, na.rm=T), data$rur_tot_pop_1_mean)
  data$Elec_yr_1_mean = ifelse(data$Country==code, mean(subdata$Elec_yr, na.rm=T), data$Elec_yr_1_mean)
  data$logGDP_1_mean = ifelse(data$Country==code, mean(subdata$logGDP, na.rm=T), data$logGDP_1_mean)
}

# data %>% group_by(country) %>% mutate(logHIV_1_mean = mean(logHIV_1, na.rm=TRUE))

# calculate demeaned variables
data$Subsidy_price1_demean = data$Subsidy_price1 - data$Subsidy_price1_mean
data$mort_rate_1_demean = data$mort_rate - data$mort_rate_1_mean
data$rur_tot_pop_1_demean = data$rur_tot_pop - data$rur_tot_pop_1_mean
data$Elec_yr_1_demean = data$Elec_yr - data$Elec_yr_1_mean
data$logGDP_1_demean = data$logGDP - data$logGDP_1_mean


# run bell and jones model

re3 = lmer(
  total_collab ~ Subsidy_price1_mean + Subsidy_price1_demean+
    rur_tot_pop_1_mean + rur_tot_pop_1_demean +
    logGDP_1_mean + logGDP_1_demean  +
    mort_rate_1_mean + mort_rate_1_demean +
    Elec_yr_1_mean + Elec_yr_1_demean +
    (1| Country), data = data
)
summary(re3)$'coefficients'

stargazer(re3)


#Section E: Table 6 - Logit Model

#Pooled logit

dat2= dat %>% 
  mutate(bin_cob= ifelse(total_collab > 0, 1, 0))


logit_mod = glm(bin_cob~ Subsidy_price1 + mort_rate + rur_tot_pop + Elec_yr + logGDP
                , data = dat2, family= binomial(link ="logit"))

summary(logit_mod)

#Generalized Linear Mixed Effect Model

log2 = glmer(bin_cob~ Subsidy_price1 + mort_rate + rur_tot_pop + Elec_yr + logGDP
             + (1 | Country), family= binomial(link ="logit"), data = dat2)
summary(log2)


stargazer(log2, logit_mod)


#Figure 3 - predictive plot form glmer

plot_predictions(log2, condition='Subsidy_price1', rug =TRUE)+
  labs(y = "Probability of Collaborative Protest", x = 'Subsidy level') +
  theme_grey()+
  #geom_jitter(data = moddat, aes(x = cinc, y = supp1, color = factor(supp1)))+
  theme_bw()+
  #scale_color_discrete(labels = c("0" = "No Foreign support", "1" = "Foreign support"))+
  theme(legend.position = 'bottom',
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank()) 


#Section F: Table 7 -  Non-collaborative protests

protest_dat$fac_cn = as.factor(protest_dat$Country)

poisMod = glmmadmb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + 
                     Elec_yr + logGDP + (1 | fac_cn), data=protest_dat, family= "poisson")

summary(poisMod)

stargazer(poisMod)

mean(protest_dat[,"total_collab"])
var(protest_dat[,"total_collab"])

#check if there's overdispersion.
poischeck = glm(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + 
                     Elec_yr + logGDP, data=protest_dat, family= "poisson")

dispersiontest(poischeck)


#Section G: Table 8 - Negative Binomial with FE

pool_mod2 = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + Elec_yr
                   + logGDP + factor(cname) - 1, data = dat) #leaving out a ref category
summary(pool_mod2)

stargazer(pool_mod2)



##################polity as control

#vdem

new_mod = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop+ Elec_yr + elect_dem
                  + logGDP, data = dat)
summary(new_mod)

stargazer(new_mod)

#random effects


rand_mod=glmmTMB(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop  +
                    Elec_yr + elect_dem + logGDP
                  + (1 | fac_cn),
                  data=dat, family = nbinom2) #polity not included
summary(rand_mod)


stargazer(rand_mod)

#polity - without levels

# new_mod2 = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop+ Elec_yr + polity2 
#                    + logGDP, data = dat)
# summary(new_mod2)
# 
# new_mod2a = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop+ Elec_yr + polity2 
#                   + Subsidy_price1*polity2 + logGDP, data = dat)
# summary(new_mod2a)
# 
# plot_model(new_mod2a, type = "pred",
#            terms = c("Subsidy_price1", "polity2"))
# 
# 
# plot_model(
#   new_mod2a, 
#   type = "pred",
#   terms= c('Subsidy_price1','polity2[-3, 9]'))
# 
# 
# 
# 
# dat2= na.omit(dat)
# 
# 
# rand_mod2 = glmmTMB(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + 
#                         Elec_yr + polity2 + Subsidy_price1 * polity2 +  logGDP + 
#                         (1 | fac_cn),
#                       data = dat, 
#                       family = nbinom2)
# 
# summary(rand_mod2)
# 
# 
# 
# plot_model(rand_mod2, type = "pred",
#            terms = c("Subsidy_price1", "polity2"))
# 
# 
# stargazer(new_mod2)

#polity with levels



dat2 = dat %>% 
  mutate(polis_dis = case_when(
    polity2 >= -10 & polity2 <= -6 ~ "Autocracy",
    polity2 >= -5 & polity2 <= 5 ~ "Anocracy",
    polity2 >= 6 & polity2 <= 10 ~ "Democracy",
    TRUE ~ NA_character_  # in case there are any other values outside the range
  )) %>%
  mutate(polis_dis = factor(polis_dis, levels = c("Autocracy", "Anocracy", "Democracy")))



dat3 = dat %>% 
  mutate(polis_dis = case_when(
    polity2 >= -10 & polity2 <= 5 ~ "Autocracy",
    polity2 >= 6 & polity2 <= 10 ~ "Democracy",
    TRUE ~ NA_character_  # in case there are any other values outside the range
  )) %>%
  mutate(polis_dis = factor(polis_dis, levels = c("Autocracy", "Democracy")))




#################polity (factor) as control

new_mod3 = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + 
                    rur_tot_pop+ Elec_yr + polis_dis 
                  + logGDP, data = dat2)
summary(new_mod3)


dat2a= na.omit(dat2)
library(nloptr)

# Try increasing the maximum number of iterations
rand_mod3 <- glmmTMB(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + 
                       Elec_yr + polis_dis + logGDP + 
                       (1 | fac_cn),
                     data = dat2a, 
                     family = nbinom2,
                     control = glmmTMBControl(optCtrl = list(maxfun = 10000)))


summary(rand_mod3)

#interaction - vdem 

int_mod1 = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + 
                    rur_tot_pop+ Elec_yr + elect_dem +
                    Subsidy_price1 * elect_dem +
                    logGDP, data = dat)


summary(int_mod1)

plot_predictions(
  int_mod1, 
  condition=c ("Subsidy_price1", 'elect_dem'))

#using range 
bkVals = c('0.159'='Low Elec Dem', '0.773'='High Elec Dem')

plot_predictions(int_mod1, 
                 condition = list("Subsidy_price1", elect_dem = range)) + #change name
theme_bw() +
  scale_color_discrete(labels=bkVals) +
  scale_fill_discrete(labels=bkVals) +
  labs(
    color='', fill='',
    x="Subsidy Price",
    y='Pred. Collab',
    title='Interaction Plot (Pooled)'
  )


#RE

init_mod1r <- glmmTMB(total_collab ~ Subsidy_price1 + elect_dem +
                       mort_rate + rur_tot_pop + Elec_yr 
                       + Subsidy_price1 * elect_dem  + logGDP + 
                       (1 | fac_cn),
                     data = dat, 
                     family = nbinom2,
                     control = glmmTMBControl(optCtrl = list(maxfun = 10000)))


summary(init_mod1r)


plot_model(init_mod1r,type = "pred",
           terms = c("Subsidy_price1", "elect_dem"))


bkVals2 = c('0.16'='Low Elec Dem', '0.77'='High Elec Dem')

plot_model(
  init_mod1r, 
  type = "int") + #change name
  theme_bw() +
  scale_color_discrete(labels=bkVals2) +
  scale_fill_discrete(labels=bkVals2) +
  labs(
    color='', fill='',
    x="Subsidy Price",
    y='Pred. Collab',
    title='Interaction Plot (RE)'
  )

#using quantile ..... hmm this is interesting 0.159 is way lower than 0.4162
quantile(dat$elect_dem, 0.25)

quantile(dat$elect_dem, 0.75)

bkVals3 = c('0.4162'='Low Elec Dem', '0.64525'='High Elec Dem')


plot_model(
  init_mod1r, 
  type = "pred",
  terms= c('Subsidy_price1','elect_dem[0.4162, 0.64525]'))+
  theme_bw() +
  scale_color_discrete(labels=bkVals3) +
  scale_fill_discrete(labels=bkVals3) +
  labs(
    color='', fill='',
    x="Subsidy Price",
    y='Pred. Collab',
    title='Interaction Plot (RE- Quantile)'
  )



#interacton -polity 

int_mod2 = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + 
                    rur_tot_pop+ Elec_yr + polis_dis +
                    Subsidy_price1 * polis_dis +
                    logGDP, data = dat2)

summary(int_mod2)


plot_model(int_mod2,type = "pred",
           terms = c("Subsidy_price1", "polis_dis")) +
  theme_bw() +
  scale_color_discrete() +
  scale_fill_discrete() +
  labs(
    color='', fill='',
    x="Subsidy Price",
    y='Pred. Collab',
    title='Interaction Plot - Polity (Pooled)'
  )

#RE


int_mod2r <- glmmTMB(total_collab ~ Subsidy_price1 +
                        mort_rate + rur_tot_pop + Elec_yr + polis_dis +
                       Subsidy_price1 * polis_dis  + logGDP + 
                        (1 | fac_cn),
                       data = dat2a, 
                       family = nbinom2,
                      control = glmmTMBControl(optCtrl = list(maxfun = 10000)))

summary(int_mod2r)



plot_model(
  int_mod2r, 
  type = "pred",
  terms = c("Subsidy_price1", "polis_dis")) +
  theme_bw() +
  scale_color_discrete() +
  scale_fill_discrete() +
  labs(
    color='', fill='',
    x="Subsidy Price",
    y='Pred. Collab',
    title='Interaction Plot - Polity (RE)'
  )


