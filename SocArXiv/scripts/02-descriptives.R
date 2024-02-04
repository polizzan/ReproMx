#######################
#DESCRIPTIVE ANALYSES#
#####################

#US and EU: trends in temporary life expectancy
e1255.USandEU.1990to2019 <- 
  nMx.US.12to55.1990to2019 %>% 
  add_row(nMx.DE.12to55.1990to2019) %>% 
  add_row(nMx.ENW.12to55.1990to2019) %>% 
  add_row(nMx.FR.12to55.1990to2019) %>% 
  add_row(nMx.IT.12to55.1990to2019) %>% 
  add_row(nMx.ES.12to55.1990to2019) %>% 
  group_by(Country, Year) %>% 
  summarize(`Temporary Life Expectancy`=ex(nmx=nMx)) %>% 
  ungroup()

#US: trends in total fertility rate
TFR.US.1990to2019 <- 
  ASFR.US.12to55.1990to2019 %>% 
  group_by(Year) %>% 
  summarize(`Total Fertility Rate`=sum(ASFR)) %>% 
  ungroup()

#US: age-standardized mortality by cause of death
nMx.US.12to55.2010to2019.cause.specific.standardized <-
  nMx.US.12to55.2010to2019.cause.specific %>% 
  left_join(nNx.US.12to55.2000to2019.standardized %>% 
              filter(Year==2010) %>% 
              select(-Year), by="Age") %>% ##standardized to reflect 2010 population structure
  group_by(Year, Cause) %>% 
  summarize(nmx_i.std=100000*sum(nmx_i*C), ##age standardization of cause-specific mortality rates
            nMx.std=100000*sum(nMx*C)) %>% ##age standardization of all-cause mortality rates
  ungroup() %>% 
  group_by(Cause) %>% 
  mutate(nmx_i.index=(nmx_i.std/nmx_i.std[Year==2010])*100) %>% ##percent change, relative to 2010 mortality rate
  ungroup()