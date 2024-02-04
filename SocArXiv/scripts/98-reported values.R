################################
#VALUES REPORTED IN MANUSCRIPT#
##############################

#US: total fertility rate
round(TFR.US.1990to2019 %>%
      filter(Year==1990) %>% 
      pull(`Total Fertility Rate`), 2)

round(TFR.US.1990to2019 %>%
        filter(Year==1997) %>% 
        pull(`Total Fertility Rate`), 2)

round(TFR.US.1990to2019 %>%
        filter(Year==2010) %>% 
        pull(`Total Fertility Rate`), 2)

round(TFR.US.1990to2019 %>%
        filter(Year==2019) %>% 
        pull(`Total Fertility Rate`), 2)

round(TFR.US.1990to2019 %>%
        filter(Year==2010) %>% 
        pull(`Total Fertility Rate`), 3)

round(TFR.US.1990to2019 %>%
        filter(Year==2010) %>% 
        pull(`Total Fertility Rate`) -
      TFR.US.1990to2019 %>%
        filter(Year==2019) %>% 
        pull(`Total Fertility Rate`), 2)

#US: temporary life expectancy
round(e1255.USandEU.1990to2019 %>% 
        filter(Country=="United States") %>% 
        filter(Year==2010) %>% 
        pull(`Temporary Life Expectancy`), 2)

round(e1255.USandEU.1990to2019 %>% 
        filter(Country=="United States") %>% 
        filter(Year==2019) %>% 
        pull(`Temporary Life Expectancy`), 2)

round(
  e1255.USandEU.1990to2019 %>% 
    filter(Country=="United States") %>% 
    filter(Year==2010) %>% 
    pull(`Temporary Life Expectancy`) -
  e1255.USandEU.1990to2019 %>% 
    filter(Country=="United States") %>% 
    filter(Year==2019) %>% 
    pull(`Temporary Life Expectancy`), 2)

round(
  (e1255.USandEU.1990to2019 %>% 
    filter(Country=="United States") %>% 
    filter(Year==2010) %>% 
    pull(`Temporary Life Expectancy`) -
  e1255.USandEU.1990to2019 %>% 
    filter(Country=="United States") %>% 
    filter(Year==2019) %>% 
    pull(`Temporary Life Expectancy`)) * 365.25, 0)

#US: selected cause-specific mortality rates (age-standardized)
round(
  nMx.US.12to55.2010to2019.cause.specific.standardized %>% 
    filter(Cause=="Malignant neoplasms") %>% 
    filter(Year==2010) %>% 
    pull(nmx_i.std), 2)

round(
  nMx.US.12to55.2010to2019.cause.specific.standardized %>% 
    filter(Cause=="Malignant neoplasms") %>% 
    filter(Year==2019) %>% 
    pull(nmx_i.std), 2)

round(
  100-
  nMx.US.12to55.2010to2019.cause.specific.standardized %>% 
    filter(Cause=="Malignant neoplasms") %>% 
    filter(Year==2019) %>% 
    pull(nmx_i.index), 1)

round(
  nMx.US.12to55.2010to2019.cause.specific.standardized %>% 
    filter(Cause=="Accidental drug poisonings") %>% 
    filter(Year==2010) %>% 
    pull(nmx_i.std), 2)

round(
  nMx.US.12to55.2010to2019.cause.specific.standardized %>% 
    filter(Cause=="Accidental drug poisonings") %>% 
    filter(Year==2019) %>% 
    pull(nmx_i.std), 2)

round(
  nMx.US.12to55.2010to2019.cause.specific.standardized %>% 
    filter(Cause=="Intentional self-harm (suicide)") %>% 
    filter(Year==2010) %>% 
    pull(nmx_i.std), 2)

round(
  nMx.US.12to55.2010to2019.cause.specific.standardized %>% 
    filter(Cause=="Intentional self-harm (suicide)") %>% 
    filter(Year==2019) %>% 
    pull(nmx_i.std), 2)

round(
  nMx.US.12to55.2010to2019.cause.specific.standardized %>% 
    filter(Cause=="Heart disease") %>% 
    filter(Year==2010) %>% 
    pull(nmx_i.std), 2)

#person-years observed
round(sum(PY.US.12to55.2010to2019.observed/1000000), 0) 

#person-years lost
round(sum(PY.lost.12to55.2010to2019), 0) 

#number of lost births = difference between observed and counterfactual
round(sum(Births.US.12to55.2010to2019.observed) -
        sum(Births.US.12to55.2010to2019.nMx.all.cause.fixed), 0) #Scenario 1

round(sum(Births.US.12to55.2010to2019.observed) -
        sum(Births.US.12to55.2010to2019.nMx.EU), 0) #Scenario 2

round((sum(Births.US.12to55.2010to2019.observed) -
         sum(Births.US.12to55.2010to2019.ASFR.fixed))/1000000, 1) #Scenario 3

round(
(sum(Births.US.12to55.2010to2019.observed) -
    sum(Births.US.12to55.2010to2019.ASFR.fixed))/
  (sum(Births.US.12to55.2010to2019.observed) -
     sum(Births.US.12to55.2010to2019.nMx.all.cause.fixed)), 0
) #Scenario 3 vs. Scenario 1

#Scenario 1: lost births by birth order
round(sum(Births.lost.US.12to55.2010to2019.birth.order[,,1]), 0) ##1 
round(sum(Births.lost.US.12to55.2010to2019.birth.order[,,2]), 0) ##2
round(sum(Births.lost.US.12to55.2010to2019.birth.order[,,3]), 0) ##3
round(sum(Births.lost.US.12to55.2010to2019.birth.order[,,4]), 0) ##4
round(sum(Births.lost.US.12to55.2010to2019.birth.order[,,5]), 0) ##5+

#lost births by cause of death (Scenario 4)
Births.lost.US.12to55.2010to2019.cause.specific %>% 
  mutate(`Births Lost`=round(`Births Lost`, 0)) 

#share of lost births attributable to accidental drug poisonings & suicide
Births.lost.US.12to55.2010to2019.cause.specific %>% 
  mutate(Share=`Births Lost`/sum(`Births Lost`)*100) %>% 
  filter(Cause %in% c("Accidental drug poisonings", "Intentional self-harm (suicide)")) %>% 
  summarize(Share=round(sum(Share), 1))

#share of lost births attributable to heart disease
Births.lost.US.12to55.2010to2019.cause.specific %>% 
  mutate(Share=`Births Lost`/sum(`Births Lost`)*100) %>% 
  filter(Cause=="Heart disease") %>% 
  summarize(Share=round(sum(Share), 1))

#projection accuracy
round(sum(Births.US.12to55.2010to2019.observed)-sum(Births.US.2010to2019$Births), 0)

round((sum(Births.US.12to55.2010to2019.observed)-sum(Births.US.2010to2019$Births))/
        sum(Births.US.2010to2019$Births)*100, 2)