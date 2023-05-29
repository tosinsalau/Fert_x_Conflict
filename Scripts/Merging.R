library(readr)
library(dplyr)

pth = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/All_Data/'

load(paste0(pth, 'Final_Data_collab.rda'))
Fert_price= read_csv(paste0(pth, "Final_Data_inputed.csv"))
load(paste0(pth, 'controls.rda'))


###collaborate events by year
collab_yr= Collab_final %>%
  group_by(year, cname) %>% 
  summarise(total_collab= sum(collab_events))

#Fix cote d'ivoire
Fert_price$Country[Fert_price$Country== "Cote d'Ivoire"] = "Côte d’Ivoire"

#rename country to cnmae in Fert Prices
Fert_price$cname = Fert_price$Country

#make year uppercase in collab_yr
collab_yr$Year = collab_yr$year

#merge events to prices
collab_prices= merge(Fert_price, collab_yr[,c("cname", "Year", "total_collab")],
            by=c("cname", "Year"),all.x =TRUE)

#turn NAs to 0 collabs
collab_prices$total_collab[is.na(collab_prices$total_collab)] = 0

###merge controls
collab_prices$year = collab_prices$Year

final= merge(collab_prices, dat3,
            by=c("cname", "year"),all.x =TRUE)

#delete duplicate variables
final= subset(final, select = -c(Year,iso2c) )

dat <- final%>%
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


save(dat, file=paste0(pth, "dat.rda"))
