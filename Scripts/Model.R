library(readr)
library(dplyr)
library(modelsummary)
library(MASS)
library(pscl)
library(ggplot2)
library(stargazer)
library(lme4)
library(glmmADMB)
library(marginaleffects)
library(stargazer)
library(mvtnorm)
library(readxl)
library(brms)
library(AER)

pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/All_Data/'

load(paste0(pth, 'dat.rda'))
load(paste0(pth, 'dat_rob.rda')) #robustness data- protests against the gov
elec_yr= read_excel("C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/elec_yr.xlsx")

elec_yr= rename(elec_yr, year = Year)

elec_yr= elec_yr[, -c(4:10)]

elec_yr$Country[elec_yr$Country == "Cote d'Ivoire"] <- 'Côte d’Ivoire'
dat$logGDP = log(dat$GDP_per_capita)

dat_rob$logGDP = log(dat_rob$GDP_per_capita)
######

dat= left_join(
  dat,
  elec_yr,
  by= c('year', 'Country')
)

dat_rob= left_join(
  dat_rob,
  elec_yr,
  by= c('year', 'Country')
)

###drop Malawi

dat <- dat %>% 
  filter(country != "Malawi")

dat_rob <- dat_rob %>% 
  filter(country != "Malawi")

desstat= dat[, c("Subsidy")]

#descriptive stats
datasummary_skim(dat)
datasummary_skim(dat_rob)


plot(dat$Subsidy_price1, dat$total_collab)
hist(dat$Subsidy_price1)
hist(dat$total_collab)
table(dat$total_collab)
cor(dat$Subsidy_price1, dat$total_collab)

plot(dat$total_collab)
plot( density( dat[,"total_collab"] ) )

#model

plot(total_collab)
 
mod1= lm(total_collab ~ Subsidy_price1, data = dat)
summary(mod1)


mod2= glm.nb(total_collab ~ Subsidy_price1 + log(GDP_per_capita)+ log(total_pop)+
               pop_0_14 + life_exp + mort_rate + rur_tot_pop +
               civ_war + autoc + factor(year)+ factor(cname), data = dat, maxit=10000)

summary(mod2)

new = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop +
               civ_war + factor(cname), data = dat)

stargazer(mod2, keep = c("Subsidy_price1", "GDP_per_capita", "total_pop",
                        "pop_0_14","life_exp","mort_rate","urb_tot_pop","democ", 
                        "pop_growth","civ_war","autoc", "part_dem",
                           "opp_free","elect_dem"))

mod2new= glm.nb(total_collab ~ Subsidy_price1 + log(GDP_per_capita)+ log(total_pop)+
               pop_0_14 + life_exp + mort_rate + urb_tot_pop +democ + pop_growth +
               civ_war + autoc + factor(year)+ factor(cname), data = dat)
summary(mod2)
zpoisMod = zeroinfl(total_collab ~ Subsidy_price1 + log(GDP_per_capita)+ log(total_pop)+
                      pop_0_14 + life_exp + mort_rate + urb_tot_pop + democ + pop_growth+
                      civ_war + autoc + factor(year)+ factor(cname), data = dat, dist='negbin', link="logit")
summary(zpoisMod)


######robust data
mod_rob= glm.nb(total_collab ~ Subsidy_price1 + log(GDP_per_capita)+ log(total_pop)+
                  pop_0_14 + life_exp + mort_rate + urb_tot_pop + democ + pop_growth +
                  civ_war + autoc + factor(year)+ factor(cname), data = dat_rob, maxit=10000)
summary(mod_rob)


modr= lm(total_collab ~ Subsidy_price1 + log(GDP_per_capita)+ log(total_pop)+
               pop_0_14 + life_exp + mort_rate + urb_tot_pop +democ + pop_growth +
               civ_war + autoc + factor(year)+ factor(cname), data = dat_rob)
summary(modr)


zpoisMod = zeroinfl(total_collab ~ Subsidy_price1 + log(GDP_per_capita)+ log(total_pop)+
                      pop_0_14 + life_exp + mort_rate + urb_tot_pop +democ + pop_growth +
                      civ_war + autoc + factor(year)+ factor(cname), data = dat_rob, dist='negbin', link="logit")
summary(zpoisMod)


########################################################################

#Pooled Model

pool_mod = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop+ Elec_yr
                  + logGDP, data = dat)
summary(pool_mod)

plot_predictions(pool_mod, condition='Subsidy_price1')


####Robust
pool_rob = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop+ Elec_yr
                  + logGDP, data = dat_rob, control=glm.control(maxit=1000))
summary(pool_rob)

stargazer(pool_rob)

#renamed
pool_mod = glm.nb(Collaboration ~ SubsidyPrice + mort_rate + rur_tot_pop, data = dat)
summary(pool_mod)

plot_predictions(pool_mod, condition='SubsidyPrice')

####with fe
pool_mod2 = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + Elec_yr
                   + logGDP + factor(cname) - 1, data = dat) #leaving out a ref category
summary(pool_mod2)

stargazer(pool_mod2)


#random effects

#glmmadmb

dat$fac_cn = as.factor(dat$cname)

glm_mod=glmmadmb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + 
                   Elec_yr + logGDP
                   #log(GDP_per_capita)
                 + (1 | fac_cn),
                 data=dat, family ='nbinom') #polity not included
summary(glm_mod)

plot_predictions(glm_mod, condition='Subsidy_price1')


#fe

fe = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + 
              Elec_yr + logGDP+ factor(cname) - 1, 
            data = dat)



#using lmer -= linear random effects

lm_mod= lmer(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + 
              Elec_yr + logGDP+ + (1 | cname), 
            data = dat)
summary(lm_mod)$'coefficients'


stargazer(pool_mod, pool_mod2, keep= c("Subsidy_price1", "mort_rate",
                                                          "rur_tot_pop",
                                       "Elec_yr", "logGDP"))

#####
#### ALL
library(foreign)
# library(lmtest)
# library(apsrtable)
# library(pcse)
library(lme4)
# library(memisc)

# load data
data = read.dta('C:/Users/herme/Teaching/msu/mle/week10_time_missingData/time_ross/data/Ross.dta')

# run fixed effects model

fe = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + 
              Elec_yr + log(GDP_per_capita)+ factor(cname) - 1, 
    data = dat)

summary(fe)$'coefficients'[1:4,]


# pull out relev vars

#LOGgdp
dat$logGDP = log(dat$GDP_per_capita)

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

# run fixed effects model

fe = glm.nb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + 
              Elec_yr + logGDP + factor(cname) - 1, 
            data = dat)

summary(fe)$'coefficients'[1:4,]

# run random effects model
re = lmer(
  total_collab ~ 
    Subsidy_price1 + mort_rate + rur_tot_pop + 
    Elec_yr + logGDP +
    (1 | cname), data=dat)
summary(re)$'coefficients'

# run bell and jones model
# re3 = lmer(
#   logCMRwdi ~ 
#     logGDPcap_1_mean + logGDPcap_1_demean + 
#     logHIV_1_mean + logHIV_1_demean +
#     GDPgrowth_1_mean + GDPgrowth_1_demean + 
#     Polity_1_mean + Polity_1_demean + 
#     (1 | country), data=data)
# summary(re3)$'coefficients'

re3 = lmer(
  total_collab ~ Subsidy_price1_mean + Subsidy_price1_demean+
    rur_tot_pop_1_mean + rur_tot_pop_1_demean +
    logGDP_1_mean + logGDP_1_demean  +
    mort_rate_1_mean + mort_rate_1_demean +
    Elec_yr_1_mean + Elec_yr_1_demean +
    (1| Country), data = data
)
summary(re3)$'coefficients'


# hausman test
hausman = function(fixed,random) {
  rNames = names(fixef(random))
  fNames = names(coef(fixed))
  timevarNames = intersect(rNames,fNames)
  k = length(timevarNames)
  rV = vcov(random)
  rownames(rV)=rNames
  colnames(rV)=rNames
  bDiff = (fixef(random))[timevarNames] - coef(fixed)[timevarNames]
  vDiff = vcov(fixed)[timevarNames,timevarNames] - rV[timevarNames,timevarNames]
  (H = t(bDiff) %*% solve(vDiff) %*% bDiff)
  c(H=H,p.value=pchisq(H[1,1],k,lower.tail=FALSE))
}

hausman(fe, re)



######
modelplot(pool_mod, coef_omit = '(Intercept)'
          # ,coef_rename = c("muchweaker" = 'Rebels much weaker',
          #                 "strong"= "Rebels Stronger", 
          #                 "cl" = "Strong Central Command",
          #                 "gs"="Government Support",
          #                 "tk"= "Transnational Constitueny",
          #                 "tc"= "Territorial Control",
          #                 "moreactors"= "More than one Actor",
          #                 "riv"= "Rivalry",
          #                 "log_rgdpch"= "ln Gdp per Capita",
          #                 "cinc"= "Military Capability",
          #                 "pol6"="Democracy")
          )+
  geom_vline (aes(xintercept=0), linetype='dashed', color='red')


####

trycoef2=data.frame(summary(pool_mod)$"coefficients")
trycoef2$var = rownames(trycoef2)
#trycoef$mod = 'Nonviolent Spatial Model'

# rename a few cols

names(trycoef2)[1] = c('beta')
names(trycoef2)[4] = c('serror')

# estimate confidence intervals
trycoef2$hi95 = trycoef2$beta + qnorm(.975)*trycoef2$serror
trycoef2$hi90 = trycoef2$beta + qnorm(.95)*trycoef2$serror
trycoef2$lo95 = trycoef2$beta - qnorm(.975)*trycoef2$serror
trycoef2$lo90 = trycoef2$beta - qnorm(.95)*trycoef2$serror


# add significance column so we can color
trycoef2$sig = NULL
trycoef2$sig[trycoef2$lo90 > 0 & trycoef2$lo95 < 0] = "Positive (.10)"
trycoef2$sig[trycoef2$lo95 > 0] = "Positive"
trycoef2$sig[trycoef2$hi90 < 0 & trycoef2$hi95 > 0] = "Negative (.10)"
trycoef2$sig[trycoef2$hi95 < 0] = "Negative"
trycoef2$sig[trycoef2$lo90 < 0 & trycoef2$hi90 > 0] = "Insig"

#pick colors for various sig levels
coefp_colors = c(
  "Positive"=rgb(54, 144, 192, maxColorValue=255),
  "Positive (.10)" = rgb(54, 144, 192, maxColorValue=255),
  "Insig"= rgb(222, 45, 38, maxColorValue=255),
  "Negative (.10)"= rgb(222, 45, 38, maxColorValue=255),
  "Negative"= rgb(222, 45, 38, maxColorValue=255))


varKey = c(
  "logGDP"="log GDP per Capita",
  "Elec_yr"="Election year",
  "rur_tot_pop"= "Rural Population(% of total)",
  "mort_rate" ="Mortality Rate", 
  "Subsidy_price1"= "Subsidy Price")

# add cleaned variable names
trycoef2$cleanVar = varKey[match(trycoef2$var, names(varKey))]

trycoef2= trycoef2[-1, ]

#remove countries in fe
trycoef2= trycoef2[1:5,]

ggplot(
  trycoef2, aes(x=cleanVar, y=beta, color=sig) ) +
  geom_linerange(aes(ymin=lo95, ymax=hi95), alpha = .3, size = 0.3) +
  geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1) +
  geom_hline(aes(yintercept=0), linetype=2, color = "black") +
  geom_point(size=4, shape=20) +
  labs(x='', y='', color='') +
  ggtitle("Fixed Effect Model")+
  scale_color_manual(values=coefp_colors) +
  coord_flip() +
  #facet_wrap(~mod, ncol=3, scales='free_x') +
  theme_minimal() +
  theme(
    legend.position = 'none'
  )

################# methods r and r

#run a logit model

Pooled
dat2= dat %>% 
  mutate(bin_cob= ifelse(total_collab > 0, 1, 0))


logit_mod = glm(bin_cob~ Subsidy_price1 + mort_rate + rur_tot_pop + Elec_yr + logGDP
                , data = dat2, family= binomial(link ="logit"))
summary(logit_mod)

plot_predictions(logit_mod, condition='Subsidy_price1', rug =TRUE)+
  labs(y = "Probability of Collaborative Protest", x = 'Subsidy level') +
  theme_grey()+
  #geom_jitter(data = moddat, aes(x = cinc, y = supp1, color = factor(supp1)))+
  theme_bw()+
  #scale_color_discrete(labels = c("0" = "No Foreign support", "1" = "Foreign support"))+
  theme(legend.position = 'bottom',
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank()) 
  

#######

log2 = glmer(bin_cob~ Subsidy_price1 + mort_rate + rur_tot_pop + Elec_yr + logGDP
                + (1 | Country), family= binomial(link ="logit"), data = dat2)
summary(log2)

plot_predictions(log2, condition='Subsidy_price1', rug =TRUE)+
  labs(y = "Probability of Collaborative Protest", x = 'Subsidy level') +
  theme_grey()+
  #geom_jitter(data = moddat, aes(x = cinc, y = supp1, color = factor(supp1)))+
  theme_bw()+
  #scale_color_discrete(labels = c("0" = "No Foreign support", "1" = "Foreign support"))+
  theme(legend.position = 'bottom',
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank()) 




#scenarios

#run model without mortality rate

dat$fac_cn = as.factor(dat$cname)

mod2=glmmadmb(total_collab ~ Subsidy_price1 + rur_tot_pop + 
                   Elec_yr + logGDP + mort_rate
                 #log(GDP_per_capita)
                 + (1 | fac_cn),
                 data=dat, family ='nbinom') #polity not included

summary(mod2)

####first difference model

# Take the first difference within each country
data_diff <- dat2 %>%
  group_by(Country) %>%
  mutate(Fert_diff = USDprice - lag(USDprice))

new_diff = na.omit(data_diff)

moddiff=glmmadmb(total_collab ~ Fert_diff + mort_rate + rur_tot_pop + 
                   Elec_yr + logGDP + (1 | fac_cn),
                 data = new_diff, family = 'nbinom') #polity not included

summary(moddiff)

moddiff=glmer.nb(total_collab ~ Fert_diff + mort_rate + rur_tot_pop + 
                   Elec_yr + logGDP + (1 | Country),
                 data = new_diff) 

summary(moddiff)
#### brms
brm1 = brm(
  total_collab ~ Fert_diff + rur_tot_pop + mort_rate +
    Elec_yr + logGDP +
    (1 | fac_cn),# exogeneity assumption
  data = data_diff,
  chains = 4, 
  cores = 4, 
  seed= 6886,
  family = negbinomial(link='log'),
  prior = c(
    set_prior(
      prior = "normal(0, 10)", class = "Intercept" ),
    set_prior(
      prior = "normal(0, 10)", class = "b" )    
  )
)


summary(brm1)
plot_predictions(brm1, condition='Fert_diff')


######

load(paste0(pth, 'acledipe.rda'))


prots= acledipe[acledipe$event_type == 'Protests',]

cont= c("Burkina Faso", "Burundi", "Ivory Coast", "Ghana", 
        "Kenya", "Malawi","Mali", "Mozambique", "Nigeria", "Niger",
        "Senegal","Tanzania","Uganda", "Rwanda", "Namibia") 

prot_dat= prots[prots$country %in% cont, ]

Fert_price= read_csv(paste0(pth, "Final_Data_inputed.csv"))
load(paste0(pth, 'controls.rda'))


###collaborate events by year
collab_yr= prot_dat %>%
  group_by(year, country) %>% 
  summarise(total_collab = n())


#Fix cote d'ivoire
Fert_price$Country[Fert_price$Country== "Cote d'Ivoire"] = "Côte d’Ivoire"

#rename country to cnmae in Fert Prices
#Fert_price$cname = Fert_price$Country

#make year uppercase in collab_yr

collab_yr$Year = collab_yr$year
collab_yr$Country = collab_yr$country
collab_yr$Country[collab_yr$Country== "Ivory Coast"] = "Côte d’Ivoire"


#merge events to prices
Prot_prices= merge(Fert_price, collab_yr[,c("Country", "Year", "total_collab")],
                     by=c("Country", "Year"),all.x =TRUE)


#turn NAs to 0 collabs
Prot_prices$total_collab[is.na(Prot_prices$total_collab)] = 0


###merge controls

dat3$Country = dat3$country
dat3$Year = dat3$year

dat3$Country[dat3$Country== "Cote d'Ivoire"] = "Côte d’Ivoire"


final = merge(Prot_prices, dat3,
             by=c("Country", "Year"), all.x =TRUE)


#delete duplicate variables
final= subset(final, select = -c(Year,iso2c) )


protest_dat <- final%>%
  rename("GDP_per_capita"= "NY.GDP.PCAP.PP.KD",
         "total_pop" = "SP.POP.TOTL",
         "rural_pop" = "SP.RUR.TOTL", 
         "rur_tot_pop" = "SP.RUR.TOTL.ZS",
         "urb_tot_pop" = "SP.URB.TOTL.IN.ZS",
         "pop_0_14" = "SP.POP.0014.TO.ZS",
         "gdp_growth"= "NY.GDP.MKTP.KD.ZG", 
         "life_exp" = "SP.DYN.LE00.IN",
         "mort_rate"= "SP.DYN.IMRT.MA.IN",
         "pop_growth" = "SP.POP.GROW", 
         "part_dem" = "v2x_partipdem",
         "opp_free" = "v2x_frassoc_thick" ,
         "elect_dem" = "v2x_polyarchy")

save(protest_dat, file=paste0(pth, "protest_dat.rda"))

#########

pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/All_Data/'


load(paste0(pth, "protest_dat.rda"))

elec_yr= read_excel("C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/elec_yr.xlsx")

elec_yr= rename(elec_yr, year = Year)

elec_yr= elec_yr[, -c(4:10)]

elec_yr$Country[elec_yr$Country == "Cote d'Ivoire"] <- 'Côte d’Ivoire'
protest_dat$logGDP = log(protest_dat$GDP_per_capita)

######

protest_dat= left_join(
  protest_dat,
  elec_yr,
  by= c('year', 'Country')
)

protest_dat$fac_cn = as.factor(protest_dat$Country)

glm_mod=glmmadmb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + elect_dem +
                   Elec_yr + logGDP
                 #log(GDP_per_capita)
                 + (1 | fac_cn),
                 data=dat, family ='nbinom') #polity not included



poisMod = glmmadmb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + 
                Elec_yr + logGDP + (1 | fac_cn), data=protest_dat, family= "poisson")

summary(poisMod)


mean(protest_dat[,"total_collab"])
var(protest_dat[,"total_collab"])
dispersiontest(poisMod)

########bootstrapping

set.seed(8021)

comp_stat() = function(dat){
  dat <- dat %>%
  sample_frac(size=1, replace=TRUE)
  #dat = dat
  boot_mod = glmmadmb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + 
                Elec_yr + logGDP + (1 | fac_cn),
                  data=dat, family ='nbinom')
  return(coef(boot_mod))
}



comp_stat <- function(dat) {
  set.seed(123)  # Set seed for reproducibility
  dat <- dat %>% 
    sample_n(size = nrow(dat), replace = TRUE)
  
  boot_mod <- glmmadmb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + 
                         Elec_yr + logGDP + (1 | fac_cn),
                       data = dat, family = 'nbinom')
  
  return(coef(boot_mod))
}


# theta <- function(dat) {
#   glm.nb(total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop+ Elec_yr
#                     + logGDP, data = dat)
# }


#
boot_wrapper <- function(dat, bootstrap_samps){
  boot_samp_list <- replicate(bootstrap_samps, comp_stat(dat),  simplify = FALSE)
  boot_samp <- dplyr::bind_rows(boot_samp_list) #%>% 
   reshape2::melt()
  names(boot_samp) <- c("parameter", "estimate")
  boot_samp$bootstrap_samps <- bootstrap_samps
  return(boot_samp)
}


#

multi_boot_list <- lapply(c(1,10), boot_wrapper, dat=dat)
multi_boot_df <- bind_rows(multi_boot_list)



save(multi_boot_df, file =paste0(pth, "multi_boot_df.rda"))

load(paste0(pth,"multi_boot_df.rda" ))



# extract the final bootstrap run with the most samples
top_boot <- multi_boot_df %>% 
  filter(bootstrap_samps == max(bootstrap_samps),
         parameter == "Subsidy_price1")

quantile(top_boot$estimate, prob=c(.025,0.975))




######


bootstrap_glmmADMB <- function(data, formula, family, variable_of_interest, num_bootstrap_samples) {
  n <- nrow(data)
  bootstrap_coefs <- matrix(NA, nrow = num_bootstrap_samples, ncol = length(coef(glmmadmb(formula, data = data, family = family))))
  
  for (i in 1:num_bootstrap_samples) {
    # Sample with replacement
    bootstrap_indices <- sample(1:n, replace = TRUE)
    
    # Ensure at least one observation > 2 is included
    while (all(data[bootstrap_indices, variable_of_interest] <= 2)) {
      bootstrap_indices <- sample(1:n, replace = TRUE)
    }
    
    # Fit glmmADMB model to the bootstrap sample
    bootstrap_data <- data[bootstrap_indices, ]
    model <- glmmadmb(formula, data = bootstrap_data, family = family)
    
    # Store the model coefficients
    bootstrap_coefs[i, ] <- coef(model)
  }
  
  return(bootstrap_coefs)
}


set.seed(123)
dat = dat  # Replace with your actual dataset
formula <- total_collab ~ Subsidy_price1 + mort_rate + rur_tot_pop + Elec_yr + logGDP + (1 | fac_cn)
family <- 'nbinom'
variable_of_interest <- "totalcollab"
num_bootstrap_samples <- 1

bootstrap_results <- bootstrap_glmmADMB(dat, formula, family, variable_of_interest, num_bootstrap_samples)


######
#use=ing binary model

calc_wald_est_bootstrap <- function(dat2){
  dat2 <- dat2 %>% 
    sample_frac(size=1, replace=TRUE)
  logit_mod = glm(bin_cob~ Subsidy_price1 + mort_rate + rur_tot_pop + Elec_yr + logGDP
                  , data = dat2, family= binomial(link ="logit"))
  summary_logit <- summary(logit_mod)
  output <- data.frame(Subsidy_Price_est= logit_mod$coefficients['Subsidy_price1'])
                      
  return(output)
}



# boot_wrapper <- function(dat2, bootstrap_samps){
#   boot_samp_list <- replicate(bootstrap_samps, calc_wald_est_bootstrap(dat2), 
#                               simplify = FALSE)
#   boot_samp <- dplyr::bind_rows(boot_samp_list) %>% 
#     reshape2::melt()
#   names(boot_samp) <- c("parameter", "estimate")
#   boot_samp$bootstrap_samps <- bootstrap_samps
#   return(boot_samp)
# }
#   
set.seed(231)
boot_wrapper <- function(dat2, bootstrap_samps){

  boot_samp_list <- replicate(bootstrap_samps, calc_wald_est_bootstrap(dat2), 
                              simplify = FALSE)
  boot_samp <- dplyr::bind_rows(boot_samp_list) %>%
    reshape2::melt()
  names(boot_samp) <- c("parameter", "estimate")
  boot_samp$bootstrap_samps <- bootstrap_samps
  return(boot_samp)
}

multi_boot_list <- lapply(c(50, 100, 500, 1000, 2500), boot_wrapper, dat2=dat2)
multi_boot_df <- bind_rows(multi_boot_list)


ggplot(multi_boot_df, aes(x = estimate, fill = parameter, facet=bootstrap_samps)) +
  geom_histogram() +
  facet_grid(bootstrap_samps~parameter,  scales="free") +
  theme_bw() +
  theme(legend.position = "none",
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())



# extract the final bootstrap run with the most samples

top_boot <- multi_boot_df %>% 
  filter(bootstrap_samps == max(bootstrap_samps),
         parameter == "Subsidy_Price_est")

sd

quantile(top_boot$estimate, prob=c(.025,0.975))


######



#load boot library
library(boot)

set.seed(231)
# function to obtain R-Squared from the data
rsq <- function(formula, data, indices)
{
  d <- data[indices,] # allows boot to select sample
  
  fit <- glm(formula, family = binomial(link ="logit"), data=d)
  return(coef(fit))
}
# bootstrapping with 1000 replications
results <- boot(data=dat2, statistic=rsq,
                R=2500, formula=bin_cob~ Subsidy_price1 + mort_rate
                + rur_tot_pop + Elec_yr + logGDP)

# view results
results
plot(results)

######

library(tseries)

adf.test(dat$total_collab)
kpss.test(dat$total_collab)

adf test is significant showing series is not integrated
kpss test is insigniificant..... series is stationary

adf.test(dat$USDprice)
adf test is significant showing series is not integrated

kpss.test(dat$USDprice)
kpss test is insignificant ... series is stationary


Guide to differencing 
If both y(t)and x(t) are stationary, do a normal regression



deseasoned2 <- arima(test$sumfat,order=c(1,1,1),
                     seasonal=list(order=c(0,0,1),period=12)) 
deseasoned2

lagar1= arima(party$lagmacro, order= c(0,1,0), xreg = party[, c("party","partyapprove")])

try = arima(dat$total_collab, order= c(0,1,0), xreg = dat[, c("Subsidy_price1",
                                                              "mort_rate"),
                                                          "rur_tot_pop", 
                                                          "Elec_yr",
                                                          "logGDP  "])

