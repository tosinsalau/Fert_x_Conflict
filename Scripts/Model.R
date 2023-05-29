library(readr)
library(dplyr)
library(modelsummary)
library(MASS)
pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/All_Data/'

load(paste0(pth, 'dat.rda'))

#descriptive stats
datasummary_skim(dat)

#model


mod= glm.nb(total_collab ~ Subsidy_price1 + log(GDP_per_capita)+
              log(total_pop)+ log(rural_pop) + rur_tot_pop + part_dem +
              opp_free + elect_dem +
              factor(year) +factor(cname), data = dat)
summary(mod)


mod= glm.nb(total_collab ~ Subsidy_price1 + log(GDP_per_capita)+ civ_war + urb_tot_pop + pop_0_14 +
              gdp_growth + life_exp + mort_rate + pop_growth +
              log(total_pop)+ log(rural_pop) + rur_tot_pop + part_dem +
              opp_free + elect_dem + polity +
              factor(year) +factor(cname), data = dat)
summary(mod)

                 