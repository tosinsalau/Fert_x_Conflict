#This script gets controls for the data

library(WDI)
library(readxl)
library(readr)
library(dplyr)
library(countrycode)

pth = "C:/Users/salau/OneDrive - Michigan State University/Research/Datasets/"
pth2 = 'C:/Users/salau/OneDrive - Michigan State University/Research/Fert_x_Conflict/'
#WB
wb = WDI(indicator=c("NY.GDP.PCAP.PP.KD", #GDP per capita (constant 2015 US$)
           "SP.POP.TOTL", #Total population 
           "SP.RUR.TOTL", #Rural population
           "SP.RUR.TOTL.ZS",#Rural population (% of total population)
           "SP.URB.TOTL.IN.ZS", #Urban population (% of total population)
           "SP.POP.0014.TO.ZS", #Population ages 0-14 (% of total population
           "NY.GDP.MKTP.KD.ZG", #GDP growth (annual %)
           "SP.DYN.LE00.IN", #Life expectancy at birth, total (years)
           "SP.DYN.IMRT.MA.IN", #Mortality rate, infant, male (per 1,000 live births))
           "SP.POP.GROW"), # Population growth (annual %)
           country="all", 
           start = 2000, end = 2020) %>% as_tibble()

wb$cname = countrycode(wb$country, 'country.name', 'country.name')

# check countries that got mislabeled
cntries = unique(data.frame(wb[,c('country', 'cname')]),stringsAsFactors=FALSE)

###UCDP
ucdp= read_csv(paste0(pth, 'ged221-csv/GEDEvent_v22_1.csv'))

ucdp_fin= ucdp[, c("year",
                   "country",
                   "country_id",
                   "gwnoa", 
                   "gwnob")]


ucdp_fin$cname = countrycode(ucdp_fin$country, 'country.name', 'country.name')

ucdp_fin<- ucdp_fin %>% 
  mutate(civ_war = 1)

#VDEM
vdem= readRDS(paste0(pth, "V-Dem-CY-FullOthers_R_v13/V-Dem-CY-Full+Others-v13.rds"))

vdem_fin =vdem[, c("country_name",
                   "country_id", 
                   "COWcode", 
                   "year",
                   "v2csantimv",
                   "v2x_libdem",
                   "v2x_partipdem",
                   "v2x_frassoc_thick",
                   "v2x_polyarchy")]

vdem_fin$cname = countrycode(vdem_fin$country_name, 'country.name', 'country.name')

#POLITY V
polity= read_excel(paste0(pth, "p5v2018.xlsx")) 

polity_fin = polity[, c("ccode",
                        "country", 
                        "scode",
                        "year", 
                        "democ",
                        "autoc",
                        "polity",
                        "polity2")]

polity_fin$cname = countrycode(polity_fin$country, 'country.name', 'country.name')

#Join all

dat1= merge(wb, vdem_fin[ , c("cname", 
                             "year",
                             "v2x_partipdem",
                             "v2x_frassoc_thick",
                             "v2x_polyarchy",
                             "v2x_libdem")], 
           by = c("cname","year"), all.x=TRUE)


dat1 <- dat1[complete.cases(dat1$cname), ]

dat2= merge(dat1, polity_fin[, c("cname", 
                              "year",
                              "autoc",
                              "democ",
                              "polity",
                              "polity2")],
            by=c("cname", "year"),all.x =TRUE)

#get unique years and countries form ucdp
ucdp_fin2 <- ucdp_fin %>%
  distinct(year, cname, civ_war)

dat3= merge(dat2, ucdp_fin2[, c("cname", 
                               "year",
                               "civ_war")],
            by = c("cname", "year"), all.x =TRUE)

#turn civ_war=0 for NAs
dat3$civ_war[is.na(dat3$civ_war)] = 0


save(dat3, file=paste0(pth2, "controls.rda"))
