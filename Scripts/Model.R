library(readr)
library(dplyr)
library(modelsummary)
library(MASS)
library(pscl)
library(ggplot2)
library(stargazer)
pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/All_Data/'

load(paste0(pth, 'dat.rda'))
load(paste0(pth, 'dat_rob.rda'))
#descriptive stats
datasummary_skim(dat)

plot(dat$Subsidy_price1, dat$total_collab)
hist(dat$Subsidy_price1)
hist(dat$total_collab)
table(dat$total_collab)
cor(dat$Subsidy_price1, dat$total_collab)


#model


mod1= lm(total_collab ~ Subsidy_price1, data = dat)
summary(mod1)


mod2= glm.nb(total_collab ~ Subsidy_price1 + log(GDP_per_capita)+ log(total_pop)+
               pop_0_14 + life_exp + mort_rate + urb_tot_pop +democ + pop_growth +
               civ_war + autoc + factor(year)+ factor(cname), data = dat)
summary(mod2)

stargazer(mod2, keep = c("Subsidy_price1", "GDP_per_capita", "total_pop",
                        "pop_0_14","life_exp","mort_rate","urb_tot_pop","democ", 
                        "pop_growth","civ_war","autoc", "part_dem",
                           "opp_free","elect_dem"))


zpoisMod = zeroinfl(total_collab ~ Subsidy_price1 + log(GDP_per_capita)+ log(total_pop)+
                      pop_0_14 + life_exp + mort_rate + urb_tot_pop + democ + pop_growth+
                      civ_war + autoc + factor(year)+ factor(cname), data = dat, dist='negbin', link="logit")
summary(zpoisMod)


######robust data
mod_rob= glm.nb(total_collab ~ Subsidy_price1 + log(GDP_per_capita)+ log(total_pop)+
                  pop_0_14 + life_exp + mort_rate + urb_tot_pop + democ + pop_growth +
                  civ_war + autoc + factor(year)+ factor(cname), data = dat_rob)
summary(mod_rob)


modr= lm(total_collab ~ Subsidy_price1 + log(GDP_per_capita)+ log(total_pop)+
               pop_0_14 + life_exp + mort_rate + urb_tot_pop +democ + pop_growth +
               civ_war + autoc + factor(year)+ factor(cname), data = dat_rob)
summary(modr)


zpoisMod = zeroinfl(total_collab ~ Subsidy_price1 + log(GDP_per_capita)+ log(total_pop)+
                      pop_0_14 + life_exp + mort_rate + urb_tot_pop +democ + pop_growth +
                      civ_war + autoc + factor(year)+ factor(cname), data = dat_rob, dist='negbin', link="logit")
summary(zpoisMod)

