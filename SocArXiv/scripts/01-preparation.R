###################
#DATA PREPARATION#
#################

######################
#All-cause mortality#
####################

#HMD.username <- " "
#HMD.password <- " "
#HFD.username <- " "
#HFD.password <- " "

#US: deaths (ages 2-56, years 2010-2019)
nDx.US.2to56.2010to2019 <- 
  readHMD(here("data", "USA_Deaths_1x1.txt"),
          fixup=TRUE) %>%
  select(Year, Age, Female) %>% 
  filter(between(Year, 2010, 2019)) %>% 
  filter(between(Age, 2, 56)) %>% 
  rename(nDx=Female)

#nDx.US.2to56.2010to2019 <- 
#  readHMDweb(CNTRY="USA", item="Deaths_1x1",
#             fixup=TRUE,
#             username=HMD.username,
#             password=HMD.password) %>%
#  select(Year, Age, Female) %>% 
#  filter(between(Year, 2010, 2019)) %>% 
#  filter(between(Age, 2, 56)) %>% 
#  rename(nDx=Female)

#US: mortality rates (ages 12-55, years 1990-2019)
nMx.US.12to55.1990to2019 <- 
  readHMD(here("data", "USA_Mx_1x1.txt"),
          fixup=TRUE) %>%
  select(Year, Age, Female) %>% 
  filter(between(Year, 1990, 2019)) %>% 
  filter(between(Age, 12, 55)) %>% 
  mutate(Country="United States") %>% 
  rename(nMx=Female)

#nMx.US.12to55.1990to2019 <- 
#  readHMDweb(CNTRY="USA", item="Mx_1x1",
#             fixup=TRUE,
#             username=HMD.username,
#             password=HMD.password) %>%
#  select(Year, Age, Female) %>% 
#  filter(between(Year, 1990, 2019)) %>% 
#  filter(between(Age, 12, 55)) %>% 
#  mutate(Country="United States") %>% 
#  rename(nMx=Female)

#US: mortality rates (ages 2-55, years 2010-2019)
nMx.US.2to55.2010to2019 <- 
  readHMD(here("data", "USA_Mx_1x1.txt"),
          fixup=TRUE) %>%
  select(Year, Age, Female) %>% 
  filter(between(Year, 2010, 2019)) %>% 
  filter(between(Age, 2, 55)) %>% 
  rename(nMx=Female)

#nMx.US.2to55.2010to2019 <- 
#  readHMDweb(CNTRY="USA", item="Mx_1x1",
#             fixup=TRUE,
#             username=HMD.username,
#             password=HMD.password) %>%
#  select(Year, Age, Female) %>% 
#  filter(between(Year, 2010, 2019)) %>% 
#  filter(between(Age, 2, 55)) %>% 
#  rename(nMx=Female)

#England & Wales: mortality rates (ages 12-55, years 1990-2019)
nMx.ENW.12to55.1990to2019 <- 
  readHMD(here("data", "GBRTENW_Mx_1x1.txt"),
          fixup=TRUE) %>%
  select(Year, Age, Female) %>% 
  filter(between(Year, 1990, 2019)) %>% 
  filter(between(Age, 12, 55)) %>% 
  mutate(Country="England & Wales") %>% 
  rename(nMx=Female)

#nMx.ENW.12to55.1990to2019 <- 
#  readHMDweb(CNTRY="GBRTENW", item="Mx_1x1",
#             fixup=TRUE,
#             username=HMD.username,
#             password=HMD.password) %>%
#  select(Year, Age, Female) %>% 
#  filter(between(Year, 1990, 2019)) %>% 
#  filter(between(Age, 12, 55)) %>% 
#  mutate(Country="England & Wales") %>% 
#  rename(nMx=Female)

#England & Wales: restrict to years >=2010
nMx.ENW.12to55.2010to2019 <- 
  nMx.ENW.12to55.1990to2019 %>% 
  filter(Year>=2010)
  
#France: mortality rates (ages 12-55, years 1990-2019)
nMx.FR.12to55.1990to2019 <- 
  readHMD(here("data", "FRATNP_Mx_1x1.txt"),
          fixup=TRUE) %>%
  select(Year, Age, Female) %>% 
  filter(between(Year, 1990, 2019)) %>% 
  filter(between(Age, 12, 55)) %>% 
  mutate(Country="France") %>% 
  rename(nMx=Female)

#nMx.FR.12to55.1990to2019 <- 
#  readHMDweb(CNTRY="FRATNP", item="Mx_1x1",
#             fixup=TRUE,
#             username=HMD.username,
#             password=HMD.password) %>%
#  select(Year, Age, Female) %>% 
#  filter(between(Year, 1990, 2019)) %>% 
#  filter(between(Age, 12, 55)) %>% 
#  mutate(Country="France") %>% 
#  rename(nMx=Female)

#France: restrict to years >=2010
nMx.FR.12to55.2010to2019 <-
  nMx.FR.12to55.1990to2019 %>% 
  filter(Year>=2010)

#Germany: mortality rates (ages 12-55, years 1990-2019)
nMx.DE.12to55.1990to2019 <- 
  readHMD(here("data", "DEUTNP_Mx_1x1.txt"),
          fixup=TRUE) %>%
  select(Year, Age, Female) %>% 
  filter(between(Year, 1990, 2019)) %>% 
  filter(between(Age, 12, 55)) %>% 
  mutate(Country="Germany") %>% 
  rename(nMx=Female)

#nMx.DE.12to55.1990to2019 <- 
#  readHMDweb(CNTRY="DEUTNP", item="Mx_1x1",
#             fixup=TRUE,
#             username=HMD.username,
#             password=HMD.password) %>%
#  select(Year, Age, Female) %>% 
#  filter(between(Year, 1990, 2019)) %>% 
#  filter(between(Age, 12, 55)) %>% 
#  mutate(Country="Germany") %>% 
#  rename(nMx=Female)

#Germany: restrict to years >=2010
nMx.DE.12to55.2010to2019 <-
  nMx.DE.12to55.1990to2019 %>% 
  filter(Year>=2010)

#Italy: mortality rates (ages 12-55, years 1990-2019)
nMx.IT.12to55.1990to2019 <- 
  readHMD(here("data", "ITA_Mx_1x1.txt"),
          fixup=TRUE) %>%
  select(Year, Age, Female) %>% 
  filter(between(Year, 1990, 2019)) %>% 
  filter(between(Age, 12, 55)) %>% 
  mutate(Country="Italy") %>% 
  rename(nMx=Female)

#nMx.IT.12to55.1990to2019 <- 
#  readHMDweb(CNTRY="ITA", item="Mx_1x1",
#             fixup=TRUE,
#             username=HMD.username,
#             password=HMD.password) %>%
#  select(Year, Age, Female) %>% 
#  filter(between(Year, 1990, 2019)) %>% 
#  filter(between(Age, 12, 55)) %>% 
#  mutate(Country="Italy") %>% 
#  rename(nMx=Female)

#Italy: restrict to years >=2010
nMx.IT.12to55.2010to2019 <-
  nMx.IT.12to55.1990to2019 %>% 
  filter(Year>=2010)

#Spain: mortality rates (ages 12-55, years 1990-2019)
nMx.ES.12to55.1990to2019 <- 
  readHMD(here("data", "ESP_Mx_1x1.txt"),
          fixup=TRUE) %>%
  select(Year, Age, Female) %>% 
  filter(between(Year, 1990, 2019)) %>%  
  filter(between(Age, 12, 55)) %>% 
  mutate(Country="Spain") %>% 
  rename(nMx=Female)

#nMx.ES.12to55.1990to2019 <- 
#  readHMDweb(CNTRY="ESP", item="Mx_1x1",
#             fixup=TRUE,
#             username=HMD.username,
#             password=HMD.password) %>%
#  select(Year, Age, Female) %>% 
#  filter(between(Year, 1990, 2019)) %>%  
#  filter(between(Age, 12, 55)) %>% 
#  mutate(Country="Spain") %>% 
#  rename(nMx=Female)

#Spain: restrict to years >=2010
nMx.ES.12to55.2010to2019 <-
  nMx.ES.12to55.1990to2019 %>% 
  filter(Year>=2010)

#US: fix nMx for ages 12-55 at 2010 level
nMx.US.2to55.2010to2019.fixed <- 
  nMx.US.2to55.2010to2019 %>% 
  mutate(nMx.fixed=rep(c(rep(NA, 10), ##set nMx for ages 2-11 to NA
                         nMx.US.2to55.2010to2019 %>% 
                         filter(between(Age, 12, 55)) %>% 
                         filter(Year==2010) %>% 
                         pull(nMx)), ##2010 values of nMx for ages 12-55
                       10), ##repeat ten times (years 2010-2019)
         nMx.fixed=case_when(between(Age, 2, 11) ~ nMx, ##set nMx for ages 2-11 to observed values
                             TRUE ~ as.numeric(nMx.fixed))) %>% 
  select(-nMx) %>% 
  rename(nMx=nMx.fixed)

#EU: average nMx (ages 12-55, years 2010-2019)
nMx.EU.12to55.2010to2019 <-
  as.data.frame(
    cbind(
    c(rep(2010:2019, times=1, each=44)),
    c(rep(12:55, times=10, each=1)),
    rowMeans(cbind(nMx.DE.12to55.2010to2019 %>% pull(nMx),
                   nMx.ENW.12to55.2010to2019 %>%  pull(nMx),
                   nMx.ES.12to55.2010to2019 %>%  pull(nMx),
                   nMx.FR.12to55.2010to2019 %>%  pull(nMx),
                   nMx.IT.12to55.2010to2019 %>% pull(nMx)))
    )
  )

names(nMx.EU.12to55.2010to2019) <- c("Year", "Age", "nMx")

#EU: add observed US mortality (ages 2-11)
nMx.EU.2to55.2010to2019 <- 
  nMx.EU.12to55.2010to2019 %>% 
  add_row(nMx.US.2to55.2010to2019 %>% 
          filter(between(Age, 2, 11))) %>% 
  arrange(Year, Age)

############
#Fertility#
##########

#US: births (2010-2019)
Births.US.2010to2019 <- 
  readHFD(here("data", "USA_birthsRR.txt"),
          fixup=TRUE) %>%
  select(Year, Age, Total) %>% 
  filter(between(Year, 2010, 2019)) %>% 
  group_by(Year) %>% 
  summarize(Births=sum(Total))

#Births.US.2010to2019 <- 
#  readHFDweb(CNTRY="USA", item="birthsRR",
#             fixup=TRUE,
#             username=HFD.username,
#             password=HFD.password) %>%
#  select(Year, Age, Total) %>% 
#  filter(between(Year, 2010, 2019)) %>% 
#  group_by(Year) %>% 
#  summarize(Births=sum(Total))

#US: ASFRs (ages 12-55, years 1990-2019)
ASFR.US.12to55.1990to2019 <- 
  readHFD(here("data", "USA_asfrRR.txt"),
          fixup=TRUE) %>%
  select(Year, Age, ASFR) %>% 
  filter(between(Year, 1990, 2019)) %>% 
  filter(between(Age, 12, 55)) 

#ASFR.US.12to55.1990to2019 <- 
#  readHFDweb(CNTRY="USA", item="asfrRR",
#             fixup=TRUE,
#             username=HFD.username,
#             password=HFD.password) %>%
#  select(Year, Age, ASFR) %>% 
#  filter(between(Year, 1990, 2019)) %>% 
#  filter(between(Age, 12, 55)) 

#US: restrict ASFRs to years >=2010 & reshape to wide format
ASFR.US.12to55.2010to2019 <-
  ASFR.US.12to55.1990to2019 %>% 
  filter(Year>=2010) %>% 
  pivot_wider(values_from=ASFR,
              names_from=Year) %>% 
  select(-Age)

#US: ASFRs by birth order (ages 12-55, years 2010-2019)
ASFR.bo.US.12to55.2010to2019 <- 
  readHFD(here("data", "USA_asfrRRbo.txt"),
          fixup=TRUE) %>%
  select(Year, Age, ASFR1, ASFR2, ASFR3, ASFR4, ASFR5p) %>% 
  rename(ASFR5=ASFR5p) %>% 
  filter(between(Year, 2010, 2019)) %>% 
  filter(between(Age, 12, 55))

#ASFR.bo.US.12to55.2010to2019 <- 
#  readHFDweb(CNTRY="USA", item="asfrRRbo",
#             fixup=TRUE,
#             username=HFD.username,
#             password=HFD.password) %>%
#  select(Year, Age, ASFR1, ASFR2, ASFR3, ASFR4, ASFR5p) %>% 
#  rename(ASFR5=ASFR5p) %>% 
#  filter(between(Year, 2010, 2019)) %>% 
#  filter(between(Age, 12, 55))

#############
#Population#
###########

#US: population counts (ages 2-56, years 2010-2020)
nNx.US.2to56.2010to2020 <- 
  readHMD(here("data", "USA_Population.txt"),
          fixup=TRUE) %>%
  filter(between(Year, 2010, 2020)) %>% 
  filter(between(Age, 2, 56)) %>% 
  rename(nNx=Female1) %>% 
  select(Year, Age, nNx)

#nNx.US.2to56.2010to2020 <- 
#  readHMDweb(CNTRY="USA", item="Population",
#             fixup=TRUE,
#             username=HMD.username,
#             password=HMD.password) %>%
#  filter(between(Year, 2010, 2020)) %>% 
#  filter(between(Age, 2, 56)) %>% 
#  rename(nNx=Female1) %>% 
#  select(Year, Age, nNx)

#US: exposures for age standardization (ages 12-55, years 2000-2019)
nNx.US.12to55.2000to2019.standardized <- 
  readHMD(here("data", "USA_Exposures_1x1.txt"),
          fixup=TRUE) %>% 
  filter(between(Year, 2000, 2019)) %>%
  filter(between(Age, 12, 55)) %>%
  group_by(Year) %>% 
  mutate(C=Female/sum(Female)) %>%
  ungroup() %>%
  select(Year, Age, C)

#nNx.US.12to55.2000to2019.standardized <- 
#  readHMDweb(CNTRY="USA", item="Exposures_1x1",
#             fixup=TRUE,
#             username=HMD.username,
#             password=HMD.password) %>%
#  filter(between(Year, 2000, 2019)) %>%
#  filter(between(Age, 12, 55)) %>%
#  group_by(Year) %>% 
#  mutate(C=Female/sum(Female)) %>%
#  ungroup() %>%
#  select(Year, Age, C)

############
#Migration#
##########

#US: annual number of net migrants (ages 2-55, years 2010-2019)
nIx.US.2to55.2010to2019 <- data.frame(matrix(nrow=54, ncol=10))
names(nIx.US.2to55.2010to2019) <- (2010:2019)

for (i in 2010:2019) {
  
nIx.US.2to55.2010to2019[, paste(i)]  <-  
  head(lead(nNx.US.2to56.2010to2020 %>% filter(Year==i+1) %>% pull(nNx)) - 
       (nNx.US.2to56.2010to2020 %>%  filter(Year==i) %>% pull(nNx)  - 
        nDx.US.2to56.2010to2019 %>% filter(Year==i) %>% pull(nDx)), 
        54)
  
}

#US: remove population counts for age 56 and year 2020
nNx.US.2to55.2010to2019 <- 
  nNx.US.2to56.2010to2020 %>% 
  filter(Year<2020 & Age<56)

###########################
#Cause-specific mortality#
#########################

#US: deaths (ages 12-55, years 2010-2019)
nDx.US.12to55.2010to2019.total <-
  read_delim(here("data", "Underlying Cause of Death_Total_12-55_2010-2019.txt"),
             delim="\t",
             col_select=c("Single-Year Ages Code", "Year", "Deaths"),
             col_types=c(`Single-Year Ages Code`="i", Year="i", Deaths="i"),
             ) %>%
  rename(Age=`Single-Year Ages Code`) %>%
  filter(!is.na(Year))

#US: deaths from accidental drug poisonings (ages 12-55, years 2010-2019)
nDx.US.12to55.2010to2019.drug <-
  read_delim(here("data", "Underlying Cause of Death_Drug overdoses_12-55_2010-2019.txt"),
             delim="\t",
             col_select=c("Single-Year Ages Code", "Year", "Deaths"),
             col_types=c(`Single-Year Ages Code`="i", Year="i", Deaths="i")) %>%
  rename(Age=`Single-Year Ages Code`) %>%
  filter(!is.na(Year)) %>% 
  mutate(Cause="Accidental drug poisonings (X40-X44)",
         `Cause code`="OD")

#US: deaths from residual causes (ages 12-55, years 2010-2019)
nDx.US.12to55.2010to2019.residual <- 
  crossing(Age=c(12:55),
           Year=c(2010:2019),
           Cause="Residual causes")

#US: disaggregate accident deaths in 2019 by transport and non-transport accidents
#add total number of deaths from all accidents for cause of death ranking below
US.12to55.2019.accident <-
  read_delim(here("data", "Underlying Cause of Death_113 causes_12-55_2015-2019.txt"),
             delim="\t",
             col_select=c("Single-Year Ages Code", "Year", 
                          "ICD-10 113 Cause List", "ICD-10 113 Cause List Code",
                          "Deaths"),
             col_types=c(`Single-Year Ages Code`="i", Year="i", 
                         `ICD-10 113 Cause List`="c", `ICD-10 113 Cause List Code`="c",
                         Deaths="i")) %>%
  rename(Cause=`ICD-10 113 Cause List`,
         `Cause code`=`ICD-10 113 Cause List Code`) %>%
  filter(!is.na(Year)) %>% 
  filter(Year==2019) %>% 
  filter(`Cause code` %in% c("GR113-113", "GR113-117")) %>% 
  mutate(Deaths=sum(Deaths)) %>% 
  group_by(Cause) %>% 
  slice(1) %>%  
  ungroup() %>% 
  select(-Year, -`Single-Year Ages Code`)

#US: ten leading causes of death (ages 12-55, year 2019)
#replace accidents with transport- and non-transport deaths from above
US.12to55.2019.leading <-
  read_delim(here("data", "Underlying Cause of Death_Leading causes_12-55_2019.txt"),
             delim="\t",
             col_select=c("15 Leading Causes of Death", 
                            "15 Leading Causes of Death Code",
                            "Deaths"),
             col_types=c(`15 Leading Causes of Death`="c", 
                         `15 Leading Causes of Death Code`="c", 
                         Deaths="i")) %>%
  rename(Cause=`15 Leading Causes of Death`, 
         `Cause code`=`15 Leading Causes of Death Code`) %>%
  filter(!is.na(Cause)) %>% 
  top_n(10, Deaths) %>% 
  filter(`Cause code`!="GR113-112") %>% 
  add_row(US.12to55.2019.accident)

#US: cause-specific mortality rates (ages 12-55, years 2010-2019)
nMx.US.12to55.2010to2019.cause.specific <-
  read_delim(here("data", "Underlying Cause of Death_113 causes_12-55_2010-2014.txt"),
             delim="\t",
             col_select=c("Single-Year Ages Code", "Year", 
                          "ICD-10 113 Cause List", "ICD-10 113 Cause List Code",
                          "Deaths"),
             col_types=c(`Single-Year Ages Code`="i", Year="i", 
                         `ICD-10 113 Cause List`="c", `ICD-10 113 Cause List Code`="c",
                         Deaths="i"))  %>%
  add_row(read_delim(here("data", "Underlying Cause of Death_113 causes_12-55_2015-2019.txt"),
                       delim="\t",
                     col_select=c("Single-Year Ages Code", "Year", 
                                  "ICD-10 113 Cause List", "ICD-10 113 Cause List Code",
                                  "Deaths"),
                     col_types=c(`Single-Year Ages Code`="i", Year="i", 
                                 `ICD-10 113 Cause List`="c", `ICD-10 113 Cause List Code`="c",
                                 Deaths="i"))) %>% 
  rename(Age=`Single-Year Ages Code`,
         Cause=`ICD-10 113 Cause List`,
         `Cause code`=`ICD-10 113 Cause List Code`) %>% 
  filter(!is.na(Year)) %>% 
  filter(`Cause code` %in% unique(US.12to55.2019.leading$`Cause code`)) %>% 
  left_join(nDx.US.12to55.2010to2019.drug %>%
            select(Year, Age, Deaths) %>% 
            rename(`Deaths (Drug overdoses)`=Deaths), by=c("Year", "Age")) %>% 
  mutate(Deaths=case_when(`Cause code`=="GR113-117" ~ Deaths-`Deaths (Drug overdoses)`,
                          TRUE ~ as.integer(Deaths))) %>% ##subtract accidental drug poisonings from nontransport accident deaths
  select(-`Deaths (Drug overdoses)`) %>% 
  add_row(nDx.US.12to55.2010to2019.drug) %>% ##add accidental drug poisonings
  arrange(Year, Age) %>% 
  rename(`Deaths (cause-specific)`=Deaths) %>% 
  left_join(nDx.US.12to55.2010to2019.total, by=c("Year", "Age")) %>% ##add total number of deaths 
  left_join(US.12to55.2019.leading %>% 
            select(Cause, Deaths) %>% 
            rename(Deaths2019=Deaths), by="Cause") %>% ##add ranking from 2019
  mutate(Cause=case_when(!`Cause code` %in% c("GR113-113", "GR113-117", "OD") ~ str_after_first(Cause, "\\#"),
                         TRUE ~ as.character(Cause)), ##improve label readability
         Cause=case_when(`Cause code` %in% c("GR113-124", "GR113-127") ~ str_before_nth(Cause, " \\(", 2),
                         TRUE ~ str_before_first(Cause, " \\(")), ##improve label readability
         Cause=case_when(Cause=="Nontransport accidents" ~ "Other nontransport accidents",
                         Cause=="Diseases of heart" ~ "Heart disease", ##correct/simplify cause names
                         TRUE ~ as.character(Cause))) %>% 
  mutate(nmx_i=`Deaths (cause-specific)`/Deaths) %>% ##share of deaths from given cause among all causes
  bind_rows(nDx.US.12to55.2010to2019.residual) %>% ##add residual causes
  arrange(Age, Year) %>% 
  mutate(Deaths2019=case_when(Cause=="Residual causes" ~ 0, ##arrange ranks
                              Cause=="Transport accidents" ~ Deaths2019-0.1,
                              Cause=="Accidental drug poisonings" ~ US.12to55.2019.leading %>% 
                                                                   filter(`Cause code`=="GR113-117") %>% 
                                                                   pull(Deaths)-0.2,
                              Cause=="Other nontransport accidents" ~ Deaths2019-0.3,
                              TRUE ~ as.numeric(Deaths2019))) %>% 
  mutate(Cause=as.factor(Cause),
         Cause=fct_reorder(Cause, Deaths2019, max, .desc=TRUE)) %>% ##order causes of death by 2019 ranking
  select(Age, Year, Cause, nmx_i) %>% 
  left_join(nMx.US.2to55.2010to2019, by=c("Year", "Age")) %>% ##add all-cause mortality rate
  mutate(nmx_i=nmx_i*nMx) %>% ##calculate cause-specific mortality rate
  group_by(Year, Age) %>% 
  mutate(nmx_i=case_when(Cause=="Residual causes" ~ nMx-sum(nmx_i, na.rm=TRUE),
                         TRUE ~ as.numeric(nmx_i))) %>% 
  ungroup()

#last "mutate" command:
#cause-specific death rate for "residual" causes = 
#difference between all-cause mortality rate 
#and sum of cause-specific mortality rates for leading causes

#US: all-cause mortality (ages 2-11, years 2010-2019)
nMx.US.2to11.2010to2019.cause.specific <-  
  crossing(Age=c(2:11),
           Year=unique(nMx.US.12to55.2010to2019.cause.specific$Year),
           Cause=unique(nMx.US.12to55.2010to2019.cause.specific$Cause)) %>% 
  left_join(nMx.US.2to55.2010to2019, by=c("Year", "Age"))

#US:
#add data sets for ages 2-11 and ages 12-55 together;
#for each cause separately, adjust all-cause mortality as if
#cause-specific mortality were fixed to 2010 levels (only ages 12-55)
nMx.US.2to55.2010to2019.cause.specific <-
  nMx.US.12to55.2010to2019.cause.specific %>% 
  filter(Year==2010) %>% 
  select(Age, Cause, nmx_i) %>% 
  rename(nmxi_2010=nmx_i) %>% 
  right_join(nMx.US.12to55.2010to2019.cause.specific, by=c("Age", "Cause")) %>% ##merge 2010 cause-specific mortality rates
  mutate(nMx=nMx-(nmx_i-nmxi_2010)) %>% ##adjust all-cause mortality by setting cause-specific mortality to 2010 levels
  bind_rows(nMx.US.2to11.2010to2019.cause.specific) %>% ##add unadjusted all-cause mortality for ages 2-11
  arrange(Age, Year) %>% 
  select(Age, Year, Cause, nMx)
