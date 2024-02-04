#############
# DATA FILE #
#############

# ---

##################################
# DATA SET FOR COUNTRY SELECTION #
##################################

## variables: 
## (1) UNWPP names,  
## (2) 'other wealthy nation' T/F, 
## (3) 'group of seven' T/F, 
## (4) 'europe' T/F

countries <- 
  rbind(
    c("Australia", TRUE, FALSE, FALSE),
    c("Austria", TRUE, FALSE, FALSE),    
    c("Belgium", TRUE, FALSE, FALSE),
    c("Canada", TRUE, TRUE, FALSE),
    c("Denmark", TRUE, FALSE, FALSE),
    c("Finland", TRUE, FALSE, FALSE),
    c("France", TRUE, TRUE, TRUE),
    c("Germany", TRUE, TRUE, TRUE),
    c("Iceland", TRUE, FALSE, FALSE),
    c("Ireland", TRUE, FALSE, FALSE),
    c("Italy", TRUE, TRUE, TRUE),
    c("Japan", TRUE, TRUE, FALSE),
    c("Luxembourg", TRUE, FALSE, FALSE),
    c("Netherlands", TRUE, FALSE, FALSE),
    c("New Zealand", TRUE, FALSE, FALSE),
    c("Norway", TRUE, FALSE, FALSE),
    c("Portugal", TRUE, FALSE, FALSE),
    c("Spain", TRUE, FALSE, TRUE),
    c("Sweden", TRUE, FALSE, FALSE),
    c("Switzerland", TRUE, FALSE, FALSE),
    c("United Kingdom", TRUE, TRUE, TRUE),
    c("United States of America", TRUE,  TRUE, TRUE)
  ) %>% data.frame()

names(countries) <- c("Name", "OWN", "GOS", "EU")

countries$OWN <- as.logical(countries$OWN)
countries$GOS <- as.logical(countries$GOS)
countries$EU <- as.logical(countries$EU)

#########################################
# LOAD UNWPP DATA FOR ALL OWN COUNTRIES #
#########################################

## load female population counts
unwpp.pop <-
  read_csv(here::here("data", "WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip")) %>% 
  filter(Location %in% countries$Name[countries$OWN], Time %in% 1950:2021) %>% 
  select(Name = Location, Year = Time, Age = AgeGrp, Population = PopFemale) %>% 
  mutate(Age = 0:100, .by = c(Year, Name)) %>% ## add numeric age variable
  mutate(Population = Population * 1000) ## adjust population counts   

if(max(years)>=2022){
  
  aux.1 <-
    read_csv(here::here("data", "WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.zip")) %>% 
    filter(Location %in% countries$Name[countries$OWN], Time %in% 2022:2100) %>% 
    select(Name = Location, Year = Time, Age = AgeGrp, Population = PopFemale) %>% 
    mutate(Age = 0:100, .by = c(Year, Name)) %>% 
    mutate(Population = Population * 1000) 
  
  unwpp.pop <-
    unwpp.pop %>% 
    add_row(aux.1)
  
}

## load female exposures
unwpp.exposure <- 
  read_csv(here::here("data", "WPP2022_PopulationExposureBySingleAgeSex_Medium_1950-2021.zip")) %>% 
  filter(Location %in% countries$Name[countries$OWN], Time %in% 1950:2021) %>% 
  select(Name = Location, Year = Time, Age = AgeGrp, Exposure = PopFemale) %>% 
  mutate(Age = 0:100, .by = c(Year, Name)) %>% 
  mutate(Exposure = Exposure * 1000)

if(max(years)>=2022){
  
  aux.2 <-
    read_csv(here::here("data", "WPP2022_PopulationExposureBySingleAgeSex_Medium_2022-2100.zip")) %>% 
    filter(Location %in% countries$Name[countries$OWN], Time %in% 2022:2100) %>% 
    select(Name = Location, Year = Time, Age = AgeGrp, Exposure = PopFemale) %>% 
    mutate(Age = 0:100, .by = c(Year, Name)) %>% 
    mutate(Exposure = Exposure * 1000) 
  
  unwpp.exposure <-
    unwpp.exposure %>% 
    add_row(aux.2)
  
}

## load birth counts
unwpp.births <-
  read_csv(here::here("data", "WPP2022_Fertility_by_Age1.zip")) %>% 
  filter(Location %in% countries$Name[countries$OWN], Time %in% 1950:2100) %>% 
  filter(Variant == "Medium") %>% 
  select(Name = Location, Year = Time, Age = AgeGrp, Births) %>% 
  mutate(Births = Births * 1000)

## load death counts
unwpp.deaths <-
  read_csv(here::here("data", "WPP2022_DeathsBySingleAgeSex_Medium_1950-2021.zip")) %>% 
  filter(Location %in% countries$Name[countries$OWN], Time %in% 1950:2021) %>% 
  select(Name = Location, Year = Time, Age = AgeGrp, Deaths = DeathFemale) %>% 
  mutate(Age = 0:100, .by = c(Year, Name)) %>% 
  mutate(Deaths = Deaths * 1000)  

if(max(years)>=2022){
  
  aux.3 <-
    read_csv(here::here("data", "WPP2022_DeathsBySingleAgeSex_Medium_2022-2100.zip")) %>% 
    filter(Location %in% countries$Name[countries$OWN], Time %in% 2022:2100) %>% 
    select(Name = Location, Year = Time, Age = AgeGrp, Deaths = DeathFemale) %>% 
    mutate(Age = 0:100, .by = c(Year, Name)) %>% 
    mutate(Deaths = Deaths * 1000)  
  
  unwpp.deaths <-
    unwpp.deaths %>% 
    add_row(aux.3)
  
}

## load sex ratios at birth
unwpp.srb <-
  read_csv(here::here("data", "WPP2022_Demographic_Indicators_Medium.zip")) %>% 
  filter(Location %in% countries$Name[countries$OWN], Time %in% 1950:2100) %>% 
  select(Name = Location, Year = Time, SRB)  %>% 
  mutate(SRB = SRB/100,
         Age = 0) ## add age zero for merging

####################
# MERGE UNWPP DATA #
####################

unwpp.aux <-
  unwpp.pop %>% 
  left_join(unwpp.exposure) %>% 
  left_join(unwpp.deaths) %>% 
  left_join(unwpp.births) %>% 
  mutate(Births=ifelse(Age %in% c(0:14, 50:999), 0, Births), ## 0 Births if age = 0-14 or 50+ 
         Age=ifelse(Age>85, 85, Age)) %>% ## aggregate counts above age 85
  summarize(across(c(Population, Exposure, Deaths, Births), ~ sum(.)), .by = c(Name, Year, Age)) %>% 
  left_join(unwpp.srb) %>% 
  arrange(Name, Year, Age)

## check if there are rows with zero exposures 
sum(unwpp.aux$Exposure==0)

######################################
# DETERMINE NUMBER OF MIGRANTS TO US #
######################################

## description of method:
## survive age- and year-specific population forward by one year (closed, female-dominant projection)
## compare to observed population in next year
## residual = net migration

## data frame with US data
US.df <-
  unwpp.aux %>% 
  filter(Name=="United States of America") %>% 
  mutate(ASMR=ifelse(Exposure==0, 0, Deaths/Exposure), ## calculate age-specific mortality rates
         ASFR=ifelse(Exposure==0, 0, Births/Exposure)) %>% ## calculate age-specific fertility rates
  mutate(Sx=life.table(nmx=ASMR)$Sx, .by = Year) ## calculate survivorship ratios

## calculate net migrants
US.mig <-
  map(years, function(x){
    
    ## female starting population
    pop.f <-
      US.df %>% 
      filter(Year==x) %>% 
      pull(Population) %>% 
      as.matrix()
    
    ## male starting population: female-dominant projection 
    pop.m <- as.matrix(rep(0, dim(pop.f)[1]))
    
    ## female survival probabilities 
    Sx.f <-
      US.df %>% 
      filter(Year==x) %>% 
      select(Year, Age, Sx) %>% 
      pivot_wider(names_from=Year,
                  values_from=Sx) %>% ## reshape to wide data set
      select(-Age) %>% 
      as.matrix() %>% 
      unname()
    
    Sx.m <- ## male survival probabilities: female-dominant projection
      mig.f <- ## female migration: closed projection
      mig.m <- ## male migration: closed projection
      matrix(0, nrow=dim(Sx.f)[1], ncol=dim(Sx.f)[2])
    
    ## female fertility rates
    Fx <- 
      US.df %>% 
      filter(Year==x) %>% 
      select(Year, Age, ASFR) %>% 
      pivot_wider(names_from=Year,
                  values_from=ASFR) %>% 
      select(-Age) %>% 
      as.matrix() %>% 
      unname()
    
    ## sex ratio at birth
    SRB <- 
      US.df %>% 
      filter(Year==x,
             Age==0) %>% 
      pull(SRB)
    
    ## project closed population forward
    US.proj <-
      CCPM(Pop_f=pop.f, 
           Pop_m=pop.m, 
           Sx_f=Sx.f, 
           Sx_m=Sx.m, 
           Fx=Fx,
           SRB=SRB, 
           Migra_f=mig.f, 
           Migra_m=mig.m, 
           stochastic=FALSE,
           out="Projection") %>% ## output: population
      data.frame()
    
    ## add variable names
    names(US.proj) <- c("Migration", "Age", "Sex", "Year")
    
    ## edit projection data set
    aux.mig <-
      US.proj %>% 
      filter(Year==1, Sex==0) %>% ## focus on female population at end of projection
      mutate(Year=Year + x, ## add year variable
             Name="United States of America") %>% ## add country variable 
      select(-Sex)
    
    ## calculate number of net migrants
    migration <-
      aux.mig %>% 
      left_join(US.df %>% select(Name, Year, Age, Population)) %>% ## merge observed population
      mutate(Migration=Population - Migration, ## migration = residual term
             Year=Year - 1) %>% ## migration occurs at end of previous year
      select(Name, Year, Age, Migration)
    
    migration
    
  })

## add US migration counts to unwpp data set
unwpp.combined <-
  unwpp.aux %>% 
  left_join(data.table::rbindlist(US.mig)) %>% ## combine year-wise estimates and merge
  filter(Year<=max(years)) ## restrict data set to period of interest
