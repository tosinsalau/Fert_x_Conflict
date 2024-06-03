
library(haven)
library(dplyr)
library(lubridate)
library(readr)
library(MASS)
library(ggplot2)
library(modelsummary)
library(patchwork)
library(pscl)
library(ggeffects)
library(readxl)


Urea_price= read_csv("C:/Users/salau/OneDrive - Michigan State University/IPE/Final/Urea_price.csv")
Urea_Int=read_csv("C:/Users/salau/OneDrive - Michigan State University/IPE/Final/Urea_Int.csv")
Collab_nig= read_csv("C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/Nigerian_states/Collab_nig.csv")


Urea_price$date2 <- ymd(Urea_price$Date)

Urea_price = Urea_price%>% 
  mutate(mon_yr=format_ISO8601(date2, precision = "ym"))


Urea_Int= rename(Urea_Int, mon_yr = Date)

#####

pre_1=Collab_nig %>% 
  filter(total_collab==1) %>% 
  group_by(mon_yr) %>% 
  summarise(weight=n())

#---------------------------


#this is all prices
Pric_dat= left_join(
  Urea_Int,
  Urea_price %>% dplyr::select(mon_yr, USDprice),
  by=('mon_yr')
)

Pric_dat[is.na(Pric_dat)] = 0

#gap between world price and retail price:
#getsubsidy.... should be abke to justmerge this  with pre 1 and 2
Pric_dat= Pric_dat %>% 
  mutate(subsidy= Urea_Intl- USDprice)
####hmm there's a gap of months where theres no reported national price

# Urea_Intp= left_join(
#   Urea_Int,
#   pre_1,
#   by=('mon_yr')
# )
# 
# All_Urea=left_join(
#   Urea_Intp,
#   Urea_price %>% dplyr::select(mon_yr, USDprice),
#   by=('mon_yr')
# )


--------------------
  
 All_bat= left_join(
    Pric_dat,
    pre_1,
    by=('mon_yr')
  )


#gap between world price and retail price:


#turn NA to 0 = no coop...... not really a good idea
Urea_price[is.na(Urea_price)] = 0

Urea_Int[is.na(Urea_Int)] = 0

All_bat[is.na(All_bat)] = 0

#rename weight
All_bat= rename(All_bat, coop_events = weight)

#model1=glm.nb(weight ~ Urea_Intl, data=All_Urea)

model1=glm.nb(coop_events ~ subsidy, data=All_bat) ###1

# model2= glm.nb(coop_events ~ subsidy+log(Gdp)+log(Pop), data=All_Urea) ###2
# 
# model3= glm.nb(coop_events ~ subsidy+log(Gdp)+log(Pop)+ factor(mon_yr), data=All_Urea) ###3
# 
# hpoisMod1 = hurdle(coop_events ~ subsidy+log(Gdp)+log(Pop), data=All_Urea, dist='poisson')
# summary(hpoisMod1)
# 
# zpoisMod1 = zeroinfl(coop_events ~ subsidy+log(Gdp)+log(Pop), data=All_Urea, dist='negbin', link="logit")
# summary(zpoisMod1)  #weird
# 
# zpoisMod1 = zeroinfl(coop_events ~ subsidy+log(Gdp)+log(Pop), data=All_Urea, dist='negbin')
# summary(zpoisMod1)
------------------
  
  model1a=glm.nb(coop_events ~ subsidy, data=
                   All_bat) ###4

model2a=glm.nb(coop_events ~ subsidy+log(Gdp)+log(Pop), data=All_bat) ###5

model3a=glm.nb(coop_events ~ subsidy+log(Gdp)+log(Pop)+ factor(mon_yr), data=All_bat )  ##6

summary(glm.nb(coop_events ~ subsidy+log(Gdp)+log(Pop)+ USDprice +Urea_Intl, data=All_bat))$'coefficient'

#mean < variance = negbin
hpoisMod1a = hurdle(coop_events ~ subsidy+log(Gdp)+log(Pop), data=All_bat, dist='negbin')
summary(hpoisMod1a)

zpoisMod1a = zeroinfl(coop_events ~ subsidy+log(Gdp)+log(Pop), data=All_bat, dist='negbin')
summary(zpoisMod1a)


zpoisMod1aa = zeroinfl(coop_events ~ subsidy+log(Gdp)+log(Pop), data=All_bat, dist='negbin', link="logit")
summary(zpoisMod1aa)
##hurdle model
library(sandwich)  # Adjust standard errors
library(lmtest)    # Recalculate model errors with sandwich functions with coeftest()

# Robust standard errors with lm()
model3_robust <- coeftest(model3, 
                          vcov = vcovHC)

models <- list(
  "Protest (1)" = glm.nb(coop_events ~ subsidy, data=All_Urea),
  "Protest (2)" = glm.nb(coop_events ~ subsidy+log(Gdp)+log(Pop), data=All_Urea),
  'Battle events (1)'= glm.nb(coop_events ~ subsidy, data=All_bat),
  'Battle events (2)'= glm.nb(coop_events ~ subsidy+log(Gdp)+log(Pop), data=All_bat)
)

modelsummary(models, output = "table.docx")

cm <- c('subsidy'    = 'Subsidy',
        'log(Gdp)' = 'Log GDP per capita',
        'log(Pop)'    = 'Log Population')

modelsummary(
  models, 
  fmt = 2, 
  stars = c('*' = .1, '**' = .05, "***" = 0.01),
  coef_omit = 'Intercept',
  coef_map = cm
)

modelplot(model2, coef_omit = '(Intercept)')
modelplot(model2a, coef_omit = '(Intercept)')

datasummary_skim(All_Urea)
datasummary_skim(All_bat)
------------------------
  
  
  
  
  
  -----------------------------  
  #plot_cap(model2a, condition="subsidy", effect="subsidy")
  #weight increases when subsidy is reduced????
  #weight also increases when price reduces.... ohhh cuz subsidy reduces (i think)
  #weight turns positiveinsecond model with controls
  
  ggplot(All_Urea, aes(mon_yr, subsidy)) + 
  geom_point()

ggplot(All_bat, aes(mon_yr, weight))+
  geom_point()


ggplot(Urea_price, aes(Date, USDprice)) +
  geom_point() +
  scale_x_date()

#############plots descriptives

new_pric= Pric_dat %>% 
  mutate(Date=ym(mon_yr))


###Yes
ggplot(new_pric, aes(Date, subsidy)) +
  geom_line() +
  scale_x_date()+
  xlab("")+
  ylab("Amount of Subsidy ($)") + # Set axis labels
  ggtitle("Subsidy Over Time") +     # Set title
  theme_bw()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggplot(new_pric, aes(Date, USDprice)) +
  geom_line() +
  scale_x_date()+
  xlab("Years") + 
  ylab("National price ($)") + # Set axis labels
  ggtitle("National Urea Price Over Time") +     # Set title
  theme_bw()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())


ggplot(new_pric, aes(Date, Urea_Intl)) +
  geom_line() +
  scale_x_date()+
  xlab("Years") + 
  ylab("International Urea price ($)") + # Set axis labels
  ggtitle("International Urea Price Over Time") +     # Set title
  theme_bw()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())

####coop events

All_Urea= All_Urea %>% 
  mutate(Date=ym(mon_yr))

####YES
ggplot(All_Urea, aes(Date, coop_events)) +
  geom_line() +
  scale_x_date()+
  xlab("") + 
  ylab("No. of Events") + # Set axis labels
  ggtitle("Cooperative Protests Over Time") +     # Set title
  theme_bw()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


All_bat= All_bat %>% 
  mutate(Date=ym(mon_yr))

###YES
ggplot(All_bat, aes(Date, coop_events)) +
  geom_line() +
  scale_x_date()+
  xlab("") + 
  ylab("No. of Events") + # Set axis labels
  ggtitle("Cooperative Battles Over Time") +     # Set title
  theme_bw()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggplot(All_bat, aes(subsidy, coop_events)) +
  geom_point() +
  #scale_x_date()+
  xlab("subsidy") + 
  ylab("No. of Events") + # Set axis labels
  ggtitle("Cooperative Battles Over Time") +     # Set title
  theme_bw()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())

##YES
ggplot(All_Urea, aes(subsidy, coop_events)) +
  geom_point() +
  #scale_x_date()+
  xlab("Amount of Subsidy") + 
  ylab("No. of Events") + # Set axis labels
  ggtitle("Cooperative Battles Over Time") +     # Set title
  theme_bw()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())

###Put together cuz ~s annoyingly said so

All_Urea= rename(All_Urea, protest = coop_events)
All_Urea$battle= All_bat$coop_events


p= ggplot(All_Urea, aes(x= Date, y= battle))+
  geom_line()+
  scale_x_date()+
  xlab("") + 
  ylab("Battles") + # Set axis labels
  ggtitle("") +     # Set title
  theme_bw()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


q= ggplot(All_Urea, aes(x= Date, y= protest))+
  geom_line()+
  scale_x_date()+
  xlab("") + 
  ylab("Protests") + # Set axis labels
  ggtitle("") +     # Set title
  theme_bw()+
  theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


p+q+ 
  plot_annotation("Cooperative Battles & Protests Over Time", theme=theme(plot.title = element_text(hjust = 0.5)))

####################### Predictions 

ggplot(All_bat, aes(x=predict(model1a), y= weight)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values')



ggpredict(zpoisMod1a, terms="subsidy") %>% 
  ggplot(aes(x, y=predicted))+
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high))+
  scale_x_continuous()+
  theme_minimal()

tryy= ggpredict(zpoisMod1a,"subsidy", type="simulate")

ggplot(tryy,
       aes(x, predicted))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.3)+
  #geom_ribbon(aes(ymin=qt90lo, ymax=qt90hi), alpha=0.4)+
  theme_minimal()

#######results
stargazer(model1, model2, model1a, model2a, type= "latex",
          column.sep.width= "1pt",
          font.size="small",
          dep.var.labels="Cooperative events",
          covariate.labels=c("Subsidy", "Log GDP per Capita", "Log Population"), 
          add.lines=list(c('Fixed effects', 'No','No','No','No')))



stargazer(hpoisMod1, hpoisMod1a, type="latex",
          column.sep.width= "1pt",
          font.size="small",
          dep.var.labels="Cooperative events",
          covariate.labels=c("Subsidy", "Log GDP per Capita", "Log Population"))
###################OLD CODE below

#stil need to understand this code
for (id in match){
  a1[,id][which(grepl("Military Forces of Nigeria", a1[, id]))] ="Gov"
}

for (id in match){
  a1[,id][which(grepl("Police Forces of Nigeria ", a1[, id]))] ="Gov"
}

#Drop unidentified
for (id in match){
  a1 = a1[which(!grepl("Unidentified", a1[,id])),]
}

#actrs cant fight temselves so remove
a1 = a1[a1$actor1!=a1$actor2,]

#filter for conflict involving group with police and military #should have 171 or 170
try<-a1 %>% 
  filter_all(any_vars(str_detect(., pattern= c("Police", "Military")))) 


try2=a1 %>% 
  filter(actor1 == "Police Forces of Nigeria"| actor2== "Police Forces of Nigeria"| 
           assoc_actor_1 == "Police Forces of Nigeria"|assoc_actor_2 == "Police Forces of Nigeria"|
           actor1 == "Military Forces of Nigeria" |actor2 == "Military Forces of Nigeria"|
           assoc_actor_1 == "Military Forces of Nigeria"|assoc_actor_2 == "Military Forces of Nigeria")


#REMOVE ROWS THAT DONT HAVE AN ACTOR TO FIGHT WITH
new <- try2[!c(try2$assoc_actor_1 == "", try2$assoc_actor_2 == "" ), ]

few<- try2[!(try2$assoc_actor_1== ""), ]

bew<- try2[!(try2$assoc_actor_2== ""), ]

#group by
per_attack= new %>% 
  group_by(actor1, year, assoc_actor_1, actor2, assoc_actor_2) %>% 
  summarise(weight=n()) %>% 
  ungroup

#####


acledipe= aclednig %>% 
  mutate(date2=ymd(event_date))

acledipe %>% 
  mutate(mon_yr=format_ISO8601(date2, precision = "ym"))


Urea_price = Urea_price%>% 
  mutate(mon_yr=format_ISO8601(Date, precision = "ym"))

Urea_Int= rename(Urea_Int, mon_yr = Date)






-------------------------------------
  
  
