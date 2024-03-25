#################
# ANALYSIS FILE #
#################

# ---

#####################
# CLEAR ENVIRONMENT #
#####################

rm(list = ls())

############################################
# INSTALL AND LOAD NECESSARY CRAN PACKAGES #
############################################

for(i in c("data.table", "gdata", "ggh4x", "ggmagnify", "here", 
           "plyr", "RColorBrewer", "svglite", "tidyverse")){

if(system.file(package = i) == ""){install.packages(i)}

}  
  
library(tidyverse)
library(ggmagnify)

######################
# SPECIFY USER INPUT #
######################

## select peer countries, one or more of:
## "OWN" (other wealthy nations), 
## "GOS" (group of seven countries), 
## "EU" (five largest countries in western europe)
## [order not important]

peer.list <- c("OWN" , "GOS", "EU")

## years for counterfactual population projection
years <- 1950:2049

## decades shown in plots
decades <- seq(1950, 2010, 10)

## first decade to include in inset of line plots [demographic indicators]
inset.decade <- 1980

## determine color palette for area plots [additional births & missing births]
color.additional.2 <- RColorBrewer::brewer.pal(n=5, "RdBu")[5]
color.additional.3 <- RColorBrewer::brewer.pal(n=5, "RdBu")[4]

color.missing.2 <- RColorBrewer::brewer.pal(n=5, "RdBu")[1]
color.missing.3 <- RColorBrewer::brewer.pal(n=5, "RdBu")[2]

## determine color palette for line plots [demographic indicators]
color.usa <- RColorBrewer::brewer.pal(n=12, "Paired")[6]
color.peer <- RColorBrewer::brewer.pal(n=12, "Paired")[1] 
color.avg <- RColorBrewer::brewer.pal(n=12, "Paired")[2]

#############################
# CHECK IF USER INPUT VALID #
#############################

if(any(!peer.list %in% c("OWN", "GOS", "EU"))){
  
  stop("Change values for 'peer.list'!", call. = FALSE)
  
}

if(any(!years %in% 1950:2100) | min(years) > 2090){
  
  stop("Change values for 'years'!", call. = FALSE)
   
}

if(any(!decades %in% seq(1950, 2090, 10))){
  
  stop("Change values for 'decades'!", call. = FALSE)
  
}

if(inset.decade < min(years) | inset.decade > max(years) | !inset.decade %in% seq(1950, 2090, 10)){
  
  stop("Change value for 'inset.decade'!", call. = FALSE)
  
}

############
# SET PATH #
############

here::i_am("scripts/01-analysis.R")

#########################
# CREATE OUTPUT FOLDERS #
#########################

if(!dir.exists(here::here("out"))){dir.create(here::here("out"))}
if(!dir.exists(here::here("out", "data"))){dir.create(here::here("out", "data"))}
if(!dir.exists(here::here("out", "plots"))){dir.create(here::here("out", "plots"))}

#########################
# LOAD CUSTOM FUNCTIONS #
#########################

source(here::here("scripts", "99-functions.R"))

######################
# PREPARE UNWPP DATA #
######################

source(here::here("scripts", "98-data.R"))

######################
# GENERATE VARIABLES #
######################

## determine number of loops necessary to estimate additional / missing births 
loops <- length(which(min(years + 1) + 0:1000 * 15 < max(years)))

#####################
# CLEAR ENVIRONMENT #
#####################

## keep only necessary objects
gdata::keep(unwpp.combined, countries,
            peer.list, years, decades, inset.decade, loops,
            color.missing.2, color.missing.3, 
            color.additional.2, color.additional.3,
            color.usa, color.peer, color.avg, 
            CCPM, life.table, sure = TRUE)

###############################
# LOOP THROUGH PEER COUNTRIES #
###############################
for(peer in peer.list){ ## start of loop
  
  #####################
  # COUNTRY SELECTION #
  #####################  
  
  ## filter countries depending on user input
  if(peer=="OWN"){
    comparison <-
      countries %>% 
      filter(OWN) %>% 
      pull(Name)
  }
  
  if(peer=="GOS"){
    comparison <-
      countries %>% 
      filter(GOS) %>% 
      pull(Name)
  }
  
  if(peer=="EU"){
    comparison <-
      countries %>% 
      filter(EU) %>% 
      pull(Name)
  }
  
  #############################################
  # GENERATE DATA SET FOR SUBSET OF COUNTRIES # 
  #############################################
  
  unwpp.df <-
    unwpp.combined %>% 
    filter(Name %in% comparison) %>% 
    add_row(
      unwpp.combined %>% 
        filter(Name %in% comparison) %>% 
        filter(Name!="United States of America") %>% 
        summarize(Population=sum(Population),
                  Exposure=sum(Exposure),
                  Deaths=sum(Deaths),
                  Births=sum(Births), .by=c(Year, Age)) %>% 
        mutate(Name="Average") ## add average of peer countries, excluding U.S.
    ) %>% 
    mutate(ASMR=ifelse(Exposure==0, 0, Deaths/Exposure), ## calculate age-specific mortality rates
           ASFR=ifelse(Exposure==0, 0, Births/Exposure)) %>% ## calculate age-specific fertility rates
    mutate(lx=life.table(nmx=ASMR)$lx,
           Lx=life.table(nmx=ASMR)$nLx,
           Tx=life.table(nmx=ASMR)$Tx,
           Sx=life.table(nmx=ASMR)$Sx, .by=c(Year, Name)) ## calculate life table functions
  
  ###############################################################################
  # LIVE BIRTHS: ANNUAL DIFFERENCE BETWEEN BASELINE & COUNTERFACTUAL PROJECTION #
  ###############################################################################
  
  # - BASELINE: U.S. MORTALITY / U.S. FERTILITY / U.S. MIGRATION
  # - COUNTERFACTUAL: PEER MORTALITY / U.S. FERTILITY / U.S. MIGRATION
  
  ## female starting population
  pop.f <-
    unwpp.df %>% 
    filter(Name=="United States of America",
           Year==min(years)) %>% 
    pull(Population) %>% 
    as.matrix()
  
  ## male starting population: female-dominant projection 
  pop.m <- as.matrix(rep(0, dim(pop.f)[1]))
  
  ## female survival probabilities: baseline 
  Sx.f.bl <-
    unwpp.df %>% 
    filter(Name=="United States of America",
           Year %in% years) %>% 
    select(Year, Age, Sx) %>% 
    pivot_wider(names_from=Year,
                values_from=Sx) %>% ## reshape to wide data set
    select(-Age) %>% 
    as.matrix() %>% 
    unname()
  
  ## female survival probabilities: counterfactual
  Sx.f.cf <-
    unwpp.df %>% 
    filter(Name=="Average",
           Year %in% years) %>% 
    select(Year, Age, Sx) %>% 
    pivot_wider(names_from=Year,
                values_from=Sx) %>%
    select(-Age) %>% 
    as.matrix() %>% 
    unname()
  
  ## female fertility rates
  Fx <- 
    unwpp.df %>% 
    filter(Name=="United States of America",
           Year %in% years) %>% 
    select(Year, Age, ASFR) %>% 
    pivot_wider(names_from=Year,
                values_from=ASFR) %>% 
    select(-Age) %>% 
    as.matrix() %>% 
    unname()
  
  ## sex ratio at birth
  SRB <- 
    unwpp.df %>% 
    filter(Name=="United States of America",
           Year %in% years,
           Age==0) %>% 
    pull(SRB)
  
  ## female in-migration
  mig.f.in <- 
    unwpp.df %>% 
    filter(Name=="United States of America",
           Year %in% years) %>% 
    select(Year, Age, Migration) %>% 
    mutate(Migration=ifelse(Migration < 0, 0, Migration)) %>% ## set out-migration to zero
    pivot_wider(names_from=Year,
                values_from=Migration) %>% 
    select(-Age) %>% 
    as.matrix() %>% 
    unname() 
  
  ## female out-migration
  mig.f.out <- 
    unwpp.df %>% 
    filter(Name=="United States of America",
           Year %in% years) %>% 
    select(Year, Age, Migration) %>% 
    mutate(Migration=ifelse(Migration < 0, -Migration, 0)) %>% ## set out-migration to positive, in-migration to zero
    pivot_wider(names_from=Year,
                values_from=Migration) %>% 
    select(-Age) %>% 
    as.matrix() %>% 
    unname() 
  
  ## female net migration
  mig.f <- mig.f.in - mig.f.out
  
  Sx.m <- ## male survival probabilities: female-dominant projection
    mig.m <- ## male migration: female-dominant projection
    matrix(0, nrow=dim(Sx.f.bl)[1], ncol=dim(Sx.f.bl)[2])
  
  ## baseline projection: live births
  births.bl <-
    CCPM(Pop_f=pop.f, 
         Pop_m=pop.m, 
         Sx_f=Sx.f.bl, 
         Sx_m=Sx.m, 
         Fx=Fx,
         SRB=SRB, 
         Migra_f=mig.f, 
         Migra_m=mig.m, 
         stochastic=FALSE,
         out="Births") %>% 
    data.frame()
  
  ## counterfactual projection: live births
  births.cf <-
    CCPM(Pop_f=pop.f, 
         Pop_m=pop.m, 
         Sx_f=Sx.f.cf, 
         Sx_m=Sx.m, 
         Fx=Fx,
         SRB=SRB, 
         Migra_f=mig.f, 
         Migra_m=mig.m, 
         stochastic=FALSE,
         out="Births") %>% 
    data.frame()
  
  ## assign variable names
  names(births.bl) <- 
    names(births.cf) <- c("Population", "Sex", "Year")
  
  ## calculate additional / missing female births
  CCPM.births.bl <-
    births.bl %>% 
    mutate(Year = Year + min(years)) %>% 
    summarize(Population = sum(Population), .by = Year) ## add female and male births
  
  CCPM.births.cf <-
    births.cf %>% 
    mutate(Year = Year + min(years)) %>% 
    summarize(Population = sum(Population), .by = Year)
  
  CCPM.diff.births <-
    CCPM.births.bl %>% 
    mutate(Population = Population - CCPM.births.cf$Population)
  
  ## clear environment
  rm(births.bl, births.cf)
  
  ##############################################################
  # LIVE BIRTHS: DISAGGREGATE ANNUAL DIFFERENCES BY GENERATION #
  ##############################################################
  
  # STEP 1
  # - START WITH CHILDREN BORN TO POPULATION ALIVE IN 1ST YEAR + ALL IN-MIGRANTS
  # - CORRECT FOR OUT-MIGRATION
  # - DETERMINE NUMBER OF CHILDREN BORN IN BOTH BL & CF SCENARIO
  # - NUMBER OF CHILDREN BORN ONLY IN BL = 'ADDITIONAL'
  # - NUMBER OF CHILDREN BORN ONLY IN CF = 'MISSING'
  
  ## female starting population: zero
  pop.f.null <- pop.f * 0
  
  ## adjust survivorship ratios: female children do not survive to have children themselves
  Sx.f.bl[1,] <- Sx.f.cf[1,] <- 0 
  
  ## baseline projection: live births to population alive in 1st year + all in-migrants
  births.bl <-
    CCPM(Pop_f=pop.f, 
         Pop_m=pop.m, 
         Sx_f=Sx.f.bl, 
         Sx_m=Sx.m, 
         Fx=Fx,
         SRB=SRB, 
         Migra_f=mig.f.in, 
         Migra_m=mig.m, 
         stochastic=FALSE,
         out="Births") %>% 
    data.frame()
  
  ## counterfactual projection: live births to population alive in 1st year + all in-migrants
  births.cf <-
    CCPM(Pop_f=pop.f, 
         Pop_m=pop.m, 
         Sx_f=Sx.f.cf, 
         Sx_m=Sx.m, 
         Fx=Fx,
         SRB=SRB, 
         Migra_f=mig.f.in, 
         Migra_m=mig.m, 
         stochastic=FALSE,
         out="Births") %>%
    data.frame()
  
  ## baseline projection: live births to out-migrants
  births.bl.out <-
    CCPM(Pop_f=pop.f.null, 
         Pop_m=pop.m, 
         Sx_f=Sx.f.bl, 
         Sx_m=Sx.m, 
         Fx=Fx,
         SRB=SRB, 
         Migra_f=mig.f.out, 
         Migra_m=mig.m, 
         stochastic=FALSE,
         out="Births") %>% 
    data.frame()
  
  ## counterfactual projection: live births to out-migrants
  births.cf.out <-
    CCPM(Pop_f=pop.f.null, 
         Pop_m=pop.m, 
         Sx_f=Sx.f.cf, 
         Sx_m=Sx.m, 
         Fx=Fx,
         SRB=SRB, 
         Migra_f=mig.f.out, 
         Migra_m=mig.m, 
         stochastic=FALSE,
         out="Births") %>% 
    data.frame()
  
  ## assign variable names
  names(births.bl) <- names(births.bl.out) <-
    names(births.cf) <- names(births.cf.out) <- c("Population", "Sex", "Year")
  
  ## check if migration adjustment can be carried out. 
  if(
    
    sum(births.bl.out$Population > births.bl$Population) != 0 |
    sum(births.cf.out$Population > births.cf$Population) != 0 
    
  ){print("Migration adjustment cannot be carried out.")
    break}
  
  ## correct for births to out-migrants: baseline
  births.bl <-
    births.bl %>% 
    mutate(Year = Year + min(years),
           Population = Population - births.bl.out$Population) 
  
  ## correct for births to out-migrants: counterfactual
  births.cf <-
    births.cf %>% 
    mutate(Year = Year + min(years),
           Population = Population - births.cf.out$Population) 
  
  ## children born in both scenarios
  both.births <-
    births.bl %>% 
    mutate(Population = ifelse(Population < births.cf$Population, Population, births.cf$Population))
  
  ## additional / missing births
  diff.births <-
    births.bl %>% 
    mutate(Population = Population - births.cf$Population)
  
  ## create empty list to store births
  both.births.list <- vector(mode = "list", length = 1)
  both.births.list[[1]] <- both.births
  
  diff.births.gen2 <- vector(mode = "list", length = 1)
  diff.births.gen2[[1]] <- diff.births
  
  ## clear environment
  rm(births.bl, births.cf, births.bl.out, births.cf.out, 
     pop.f, pop.f.null, pop.m,
     Sx.f.bl, Sx.f.cf, Sx.m,  
     Fx, SRB,
     mig.f, mig.f.in, mig.f.out, mig.m)
  
  # STEP II
  # - PROJECT NUMBER OF CHILDREN BORN IN BOTH BL & CF SCENARIO (ABOVE) 
  #     FORWARD UNDER BOTH BL & CF MORTALITY CONDITIONS
  # - DETERMINE NUMBER OF CHILDREN BORN IN BOTH BL & CF SCENARIO
  #     + NUMBER OF CHILDREN BORN ONLY IN BL ('ADDITIONAL') OR ONLY IN CF ('MISSING')
  # - REPEAT UNTIL END OF PROJECTION PERIOD IS REACHED
  
  ## run required amount of loops
  for(g in 2:(loops + 1)){
    
    ## determine first year of projection
    year <- min(years + 1) + (g - 2) * 15  
    
    ## survive female births forward to age 0: baseline & counterfactual
    start <-
      both.births %>% 
      filter(Sex==0) %>% 
      left_join( ## merge survival probabilities 
        
        unwpp.df %>% 
          filter(Name %in% c("Average", "United States of America")) %>% 
          filter(Age==0) %>%
          select(Year, Sx, Name) %>% 
          pivot_wider(names_from = "Name",
                      values_from = "Sx")
        
      ) %>% 
      mutate(`United States of America` = Population * `United States of America`,
             `Average` = Population * `Average`) %>% ## survive births forward
      mutate(Age = 0, Year = Year + 1) %>% ## assign new age and year
      filter(Year %in% years) %>% 
      arrange(Year) %>% 
      select(-c(Population, Sex))
    
    ## create empty lists
    both <- vector(mode = "list", length = 1)
    diff <- vector(mode = "list", length = 1)
    
    ## loop: project females aged 0 forward under BL & CF until end of period, record additional / missing births
    for(x in year:max(years)){
      
      ## female starting population: baseline population at age 0
      pop.f.bl <-
        c(
          start %>% 
            filter(Year==x) %>% 
            pull(`United States of America`),  
          rep(0, 85)
        ) %>% 
        as.matrix()
      
      ## female starting population: counterfactual population at age 0        
      pop.f.cf <-
        c(
          start %>% 
            filter(Year==x) %>% 
            pull(`Average`),  
          rep(0, 85)
        ) %>% 
        as.matrix()
      
      ## male starting population: female-dominant projection 
      pop.m <- as.matrix(rep(0, dim(pop.f.bl)[1]))
      
      ## female survival probabilities: baseline
      Sx.f.bl <-
        unwpp.df %>% 
        filter(Name=="United States of America",
               Year %in% x:max(years)) %>% 
        select(Year, Age, Sx) %>% 
        pivot_wider(names_from=Year,
                    values_from=Sx) %>%
        select(-Age) %>% 
        as.matrix() %>% 
        unname()
      
      ## female survival probabilities: counterfactual
      Sx.f.cf <-
        unwpp.df %>% 
        filter(Name=="Average",
               Year %in% x:max(years)) %>% 
        select(Year, Age, Sx) %>% 
        pivot_wider(names_from=Year,
                    values_from=Sx) %>% 
        select(-Age) %>% 
        as.matrix() %>% 
        unname()
      
      ## female fertility rates
      Fx <- 
        unwpp.df %>% 
        filter(Name=="United States of America",
               Year %in% x:max(years)) %>% 
        select(Year, Age, ASFR) %>% 
        pivot_wider(names_from=Year,
                    values_from=ASFR) %>% 
        select(-Age) %>% 
        as.matrix() %>% 
        unname()
      
      ## sex ratio at birth
      SRB <- 
        unwpp.df %>% 
        filter(Name=="United States of America",
               Year %in% x:max(years),
               Age==0) %>% 
        pull(SRB)
      
      Sx.m <- ## male survival probabilities: female-dominant projection
        mig.m <- ## male migration: closed projection
        mig.f <- ## female migration: closed projection
        matrix(0, nrow=dim(Sx.f.bl)[1], ncol=dim(Sx.f.bl)[2])
      
      ## adjust survivorship ratios: female children do not survive to have children themselves
      Sx.f.bl[1,] <- Sx.f.cf[1,] <- 0 
      
      ## baseline projection: live births
      births.bl <-
        CCPM(Pop_f=pop.f.bl, 
             Pop_m=pop.m, 
             Sx_f=Sx.f.bl, 
             Sx_m=Sx.m, 
             Fx=Fx,
             SRB=SRB, 
             Migra_f=mig.f, 
             Migra_m=mig.m, 
             stochastic=FALSE,
             out="Births") %>%
        data.frame()
      
      ## counterfactual projection: live births
      births.cf <-
        CCPM(Pop_f=pop.f.cf, 
             Pop_m=pop.m, 
             Sx_f=Sx.f.cf, 
             Sx_m=Sx.m, 
             Fx=Fx,
             SRB=SRB, 
             Migra_f=mig.f, 
             Migra_m=mig.m, 
             stochastic=FALSE,
             out="Births") %>%
        data.frame()
      
      ## assign variable names
      names(births.bl) <- 
        names(births.cf) <- c("Population", "Sex", "Year")  
      
      ## births in both scenarios
      aux.both <- 
        births.bl %>% 
        mutate(Year = Year + x,
               Population = ifelse(Population < births.cf$Population, Population, births.cf$Population))
      
      ## additional / missing births
      aux.diff <-
        births.bl %>% 
        mutate(Year = Year + x,
               Population = Population - births.cf$Population)
      
      ## store in list objects
      i <- x - min(start$Year) + 1
      
      both[[i]] <- aux.both
      diff[[i]] <- aux.diff
      
    }
    
    ## prepare birth counts for next loop
    both.births <- 
      data.table::rbindlist(both) %>% 
      summarize(Population = sum(Population), .by = c(Year, Sex))
    
    ## store births
    diff.births.gen2[[g]] <- data.table::rbindlist(diff) 
    
    both.births.list[[g]] <- both.births
    
  }
  
  ## clean environment
  rm(births.bl, births.cf, 
     pop.f.bl, pop.f.cf, pop.m,   
     Sx.f.bl, Sx.f.cf, Sx.m, 
     Fx, SRB,
     mig.f, mig.m,
     start, both.births, both, aux.both, diff.births, diff, aux.diff,   
     g, i, x, year)
  
  # STEP III
  # - PROJECT ADDITIONAL and MISSING BIRTHS FORWARD SEPARATELY
  #     - ADDITIONAL BIRTHS: BL MORTALITY
  #     - MISSING BIRTHS: CF MORTALITY
  # - RECORD LIVE BIRTHS, INCL. CHILDREN OF CHILDREN 
  #     = ADDITIONAL / MISSING BIRTHS IN 3RD+ GENERATION
  
  diff.births.gen3plus <-
    map(c("additional", "missing"), function(y){
      
      if(y=="additional"){ ## restrict to additional female births
        
        aux <- 
          data.table::rbindlist(diff.births.gen2) %>%
          filter(Population > 0, Sex==0) %>% 
          summarize(Population = sum(Population), .by = Year)
        
      }else{ ## restrict to missing female births
        
        aux <-
          data.table::rbindlist(diff.births.gen2) %>%
          filter(Population < 0, Sex==0) %>% 
          summarize(Population = sum(Population), .by = Year)
        
      }
      
      ## survive births forward using probabilities of survival 
      ## - from U.S., for additional births 
      ## - from peer, for missing births
      aux.2 <-
        aux %>% 
        left_join( ## merge survival probabilities 
          
          unwpp.df %>% 
            filter(Name %in% c("Average", "United States of America")) %>% 
            filter(Age==0) %>%
            select(Year, Sx, Name) %>% 
            pivot_wider(names_from = "Name",
                        values_from = "Sx")
          
        ) %>% 
        mutate(Population = ifelse(Population > 0, 
                                   Population * `United States of America`, 
                                   Population * `Average`)) %>% ## survive births forward
        mutate(Age = 0, Year = Year + 1) %>% ## assign new age and year
        filter(Year %in% years) %>%     
        arrange(Year) 
      
      ## project females aged 0 forward to end of projection period
      aux.3 <-
        map(unique(aux.2$Year), function(x){
          
          ## female starting population: population at age 0
          pop.f <-
            c(
              aux.2 %>% 
                filter(Year==x) %>% 
                pull(Population),  
              rep(0, 85)  
            ) %>% 
            as.matrix()
          
          ## male starting population: female-dominant projection 
          pop.m <- as.matrix(rep(0, dim(pop.f)[1]))
          
          if(y=="additional"){ ## if additional births
            
            ## female survival probabilities: baseline
            Sx.f <-
              unwpp.df %>% 
              filter(Name=="United States of America",
                     Year %in% x:max(years)) %>% 
              select(Year, Age, Sx) %>% 
              pivot_wider(names_from=Year,
                          values_from=Sx) %>% 
              select(-Age) %>% 
              as.matrix() %>% 
              unname()
            
          }else{ ## if missing births
            
            ## female survival probabilities: counterfactual
            Sx.f <-
              unwpp.df %>% 
              filter(Name=="Average",
                     Year %in% x:max(years)) %>% 
              select(Year, Age, Sx) %>% 
              pivot_wider(names_from=Year,
                          values_from=Sx) %>% 
              select(-Age) %>% 
              as.matrix() %>% 
              unname()
            
          }
          
          ## female fertility rates
          Fx <- 
            unwpp.df %>% 
            filter(Name=="United States of America",
                   Year %in% x:max(years)) %>% 
            select(Year, Age, ASFR) %>% 
            pivot_wider(names_from=Year,
                        values_from=ASFR) %>% 
            select(-Age) %>% 
            as.matrix() %>% 
            unname()
          
          ## sex ratio at birth
          SRB <- 
            unwpp.df %>% 
            filter(Name=="United States of America",
                   Year %in% x:max(years),
                   Age==0) %>% 
            pull(SRB)
          
          Sx.m <- ## male survival probabilities: female-dominant projection
            mig.m <- ## male migration: closed projection
            mig.f <- ## female migration: closed projection
            matrix(0, nrow=dim(Sx.f)[1], ncol=dim(Sx.f)[2])
          
          ## projection: live births
          births <-
            CCPM(Pop_f=pop.f, 
                 Pop_m=pop.m, 
                 Sx_f=Sx.f, 
                 Sx_m=Sx.m, 
                 Fx=Fx,
                 SRB=SRB,
                 Migra_f=mig.f, 
                 Migra_m=mig.m, 
                 stochastic=FALSE,
                 out="Births") %>% 
            data.frame()
          
          ## assign variable names
          names(births) <- c("Population", "Sex", "Year")  
          
          ## change year variable
          births$Year <- births$Year + x
          
          births
          
        })
      
      aux.3
      
    })
  
  ##############
  # DATA CHECK #
  ##############

  ## combine births observed in both BL and CF
  both.births.combined <-
    data.table::rbindlist(both.births.list, use.names = TRUE)
  
  ## combine additional & missing births from different generations
  diff.births.combined <-
    data.table::rbindlist(diff.births.gen2) %>% mutate(Indicator = ifelse(Population >= 0, "Additional", "Missing"), Generation = "2") %>% 
    add_row(data.table::rbindlist(diff.births.gen3plus[[1]]) %>% mutate(Indicator = "Additional", Generation = "3+")) %>% 
    add_row(data.table::rbindlist(diff.births.gen3plus[[2]]) %>% mutate(Indicator = "Missing", Generation = "3+"))
  
  ## compare with output from regular CCPM
  df.1 <- 
    diff.births.combined %>% 
    summarize(Population = sum(Population), .by = Year)
  
  df.2 <- CCPM.diff.births
  
  if(!all.equal(df.1, df.2)){
    print("Inconsistent results.") 
    break}
  if(!all.equal(CCPM.births.bl$Population - df.1$Population, CCPM.births.cf$Population)){
    print("Inconsistent results.")
    break} 
  
  ## plot of additional / missing births alongside output from regular CCPM
  ## determine y-axis range
  CCPM.min <- df.1 %>% filter(Year %in% years) %>% pull(Population) %>% min()
  CCPM.min <- plyr::round_any(CCPM.min / 1000, f = floor, accuracy = 5)
  
  CCPM.max <- df.1 %>% filter(Year %in% years) %>% pull(Population) %>% max()
  CCPM.max <- plyr::round_any(CCPM.max / 1000, f = ceiling, accuracy = 5)
  
  ## plot
  CCPM.plot <-
    df.1 %>% 
    filter(Year %in% years) %>% 
    ggplot(aes(x = Year, y = Population)) +
    ggh4x::stat_difference(aes(x = Year, ymin = 0, ymax = Population / 1000)) +
    scale_fill_manual("", 
                      values = c("+" = color.additional.2,
                                 "-" = color.missing.2),
                      labels = c("+" = "Additional", 
                                 "-" = "Missing")) +  
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_line(data = df.2 %>% filter(Year %in% years), 
              aes(x = Year, y = Population / 1000, linetype = "Baseline \U2013 Counterfactual"), 
              color = "black", linewidth = 0.5, inherit.aes = FALSE) +
    labs(x = "Year", y = "Births (in thousands)") +
    scale_x_continuous(breaks = seq(min(years), max(years + 1), 5),
                       labels = rev(rev(c(rbind(seq(min(years), max(years + 1), 10), "")))[-1])) +
    scale_y_continuous(breaks = seq(CCPM.min, CCPM.max, 5)) +
    scale_linetype_manual("", values = "longdash") +
    guides(fill = guide_legend(nrow = 1, byrow = FALSE, order = 1)) +
    theme_bw() +
    theme(aspect.ratio = 1,
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16, vjust = -1),
          axis.title.y = element_text(size = 16, vjust = 2.5),
          axis.line = element_line(colour = "black"),
          legend.position = "bottom",
          legend.text = element_text(size = 12)) +
    coord_cartesian(xlim = c(min(years), max(years + 1)),
                    ylim = c(CCPM.min, CCPM.max), 
                    clip = "on") 
  
  ## save plot as .pdf and .svg file
  cairo_pdf(here::here("out",  "plots", paste0("CCPM-",  peer, "-", min(years), "-", max(years), ".pdf")),
            width=10, height=7)
  
  print(CCPM.plot)
  
  dev.off()
  
  svg(here::here("out",  "plots", paste0("CCPM-",  peer, "-", min(years), "-", max(years), ".svg")),
      width=10, height=7)
  
  print(CCPM.plot)
  
  dev.off()
  
  ####################################
  # PLOT ADDITIONAL / MISSING BIRTHS #
  ####################################
  
  ## determine y-axis range
  births.min <-
    diff.births.combined %>% 
    filter(Year %in% min(decades):max(decades + 9),
           Population < 0) %>% 
    summarize(Population = sum(Population), .by = Year) %>% 
    pull(Population) %>% 
    min()
  
  births.max <-
    diff.births.combined %>% 
    filter(Year %in% min(decades):max(decades + 9),
           Population > 0) %>% 
    summarize(Population = sum(Population), .by = Year) %>% 
    pull(Population) %>% 
    max()

  births.min <- plyr::round_any(births.min / 1000, f = floor, accuracy = 5)  
  births.max <- plyr::round_any(births.max / 1000, f = ceiling, accuracy = 5)

  ## plot
  diff.births.plot <-
    diff.births.combined %>% 
    filter(Year %in% min(decades):max(decades + 9)) %>% 
    summarize(Population = sum(Population), .by = c(Year, Indicator, Generation)) %>% 
    complete(Year, Indicator, Generation, fill = list(Population = 0)) %>% 
    mutate(Generation = factor(Generation, levels = c("3+", "2")))  %>% 
    ggplot(aes(x = Year, y = Population / 1000, fill = interaction(Indicator, Generation))) +
    geom_area(stat = "identity", position = "stack") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +    
    geom_line(data = df.2 %>% filter(Year %in% min(decades):max(decades + 9)), 
              aes(x = Year, y = Population / 1000, linetype = "Baseline \U2013 Counterfactual"), 
              color = "black", linewidth = 0.5, inherit.aes = FALSE) +  
    labs(x = "Year", y = "Births (in thousands)") +
    scale_x_continuous(breaks = seq(min(decades), max(decades + 10), 5),
                       labels = rev(rev(c(rbind(seq(min(decades), max(decades + 10), 10), "")))[-1])) +
    scale_y_continuous(breaks = seq(births.min, births.max, 5)) +
    scale_fill_manual("", 
                      breaks = c("Additional.2", "Additional.3+", 
                                 "Missing.2", "Missing.3+"),
                      labels = c("Additional, 2nd generation", 
                                 "Additional, 3rd+ generation",
                                 "Missing, 2nd generation",
                                 "Missing, 3rd+ generation"),
                      values = c(color.additional.2, color.additional.3, color.missing.2, color.missing.3)) +
    scale_linetype_manual("", values = "longdash") +
    guides(fill = guide_legend(nrow = 2, byrow = FALSE, order = 1)) +
    theme_bw() +
    theme(aspect.ratio = 1,
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text = element_text(size = 14),
          axis.title.x = element_text(size = 16, vjust = -1),
          axis.title.y = element_text(size = 16, vjust = 2.5),
          axis.line = element_line(colour = "black"),
          legend.position = "bottom",
          legend.text = element_text(size = 12)) +
    coord_cartesian(xlim = c(min(decades), max(decades + 10)),
                    ylim = c(births.min, births.max), 
                    clip = "on") 
  
  ## save plot as .pdf and .svg file
  cairo_pdf(here::here("out",  "plots", paste0("diff-births-",  peer, "-", min(decades), "-", max(decades + 9), ".pdf")),
            width=8, height=8)
  
  print(diff.births.plot)
  
  dev.off()
  
  svg(here::here("out",  "plots", paste0("diff-births-",  peer, "-", min(decades), "-", max(decades + 9), ".svg")),
      width=8, height=8)
  
  print(diff.births.plot)
  
  dev.off()
  
  ## clean environment
  rm(CCPM.min, CCPM.max, births.min, births.max, 
     CCPM.plot, diff.births.plot, 
     df.1, df.2,
     both.births.list, diff.births.gen2, diff.births.gen3plus)
  
  #################################
  # FORMAL-DEMOGRAPHIC INDICATORS #
  #################################
  
  ## generate data set with l(50)
  lx.df <-
    unwpp.df %>% 
    filter(Age==50) %>% 
    select(Year, Name, Value = lx) %>% 
    mutate(Indicator = "l50",
           Label = "Probability of Survival to Age 50")
  
  ## generate data set with temporary/reproductive-age life expectancy
  temp.df <-
    unwpp.df %>% 
    filter(Age %in% c(15, 50)) %>%
    select(Year, Name, Age, lx, Tx) %>% 
    pivot_wider(names_from = "Age",
                values_from = c("lx", "Tx")) %>% 
    mutate(Value = (Tx_15-Tx_50)/lx_15) %>% 
    select(Year, Name, Value)  %>% 
    mutate(Indicator = "35e15", 
           Label = "Reproductive-age Life Expectancy")
  
  ## generate data set with reproduction-survival ratio
  RSR.df <-
    unwpp.df %>% 
    filter(Age %in% 15:49) %>% 
    summarize(TFR = sum(ASFR),
              NRR2sex = sum(Lx * ASFR),
              RSR2sex = ifelse(TFR==0, 0, NRR2sex/TFR), .by = c(Year, Name)) %>% 
    select(Year, Name, Value = RSR2sex)  %>% 
    mutate(Indicator = "RSR",
           Label = "Reproduction-survival Ratio")
  
  ## combine indicator-specific data sets
  formal_indicators.df <-
    lx.df %>% 
    add_row(temp.df) %>% 
    add_row(RSR.df)
  
  ## clean ennvironment
  rm(lx.df, temp.df, RSR.df)
  
  ## plot indicators in loop
  walk(unique(formal_indicators.df$Indicator), function(i){
    
    aux <- 
      formal_indicators.df %>% 
      filter(Year %in% min(decades):max(decades+9)) %>% 
      filter(Indicator == i) 
    
    label.y <- unique(aux$Label)
    
    ## x-axis range of plot 
    label.xmin <- min(decades)
    label.xmax <- max(decades)+10
    
    ## x-axis range of inset
    inset.xmin <- inset.decade
    inset.xmax <- max(decades)+10
    
    if(i %in% c("l50", "RSR")){
      
      ## y-axis range of plot
      label.ymin <- plyr::round_any(min(aux$Value[aux$Year %in% label.xmin:label.xmax]), f=floor, accuracy=0.05) ## minimum in observation period
      label.ymax <- 1 ## theoretical maximum of indicator
      
      ## y-axis range of inset
      inset.ymin <- plyr::round_any(min(aux$Value[aux$Year %in% inset.xmin:inset.xmax]), f=floor, accuracy=0.01) ## minimum in inset period
      inset.ymax <- label.ymax ## inset maximum = theoretical maximum
      
    }
    
    if(i=="35e15"){
      
      ## y-axis range of plot
      label.ymin <- plyr::round_any(min(aux$Value[aux$Year %in% label.xmin:label.xmax]), f=floor, accuracy=0.5) ## minimum in observation period
      label.ymax <- 35 ## theoretical maximum of indicator
      
      ## y-axis range of inset
      inset.ymin <- plyr::round_any(min(aux$Value[aux$Year %in% inset.xmin:inset.xmax]), f=floor, accuracy=0.1) ## minimum in inset period
      inset.ymax <- label.ymax ## inset maximum = theoretical maximum
      
    }
    
    ## position of inset in plot
    position.inset.xmin <- inset.xmin
    position.inset.xmax <- inset.xmax
    
    position.inset.ymin <- label.ymin + (label.ymax - label.ymin) * 0.05
    position.inset.ymax <- inset.ymin - (inset.ymin - label.ymin) * 0.1
    
    ## plot
    plot <-
      ggplot(data=aux %>% filter(Year %in% label.xmin:label.xmax, !Name %in% c("Average", "United States of America")), 
             aes(x=Year, y=Value, group=Name)) +
      geom_line(color=color.peer, linewidth=0.5) +
      geom_line(data=aux %>% filter(Year %in% label.xmin:label.xmax, Name=="United States of America"), 
                aes(x=Year, y=Value, group=Name), 
                color=color.usa, linewidth=0.75) +
      geom_line(data=aux %>% filter(Year %in% label.xmin:label.xmax, Name=="Average"), 
                aes(x=Year, y=Value, group=Name), 
                color=color.avg, linewidth=0.75, linetype="dashed") +
      labs(x="Year",
           y=label.y) +
      theme_bw() +
      theme(panel.grid=element_blank(),
            axis.text=element_text(size=12)) + ## label size of inset
      scale_x_continuous(breaks=seq(label.xmin, label.xmax, 5),
                         labels=seq(label.xmin, label.xmax, 5)) ## x-axis of inset
    
    if(i %in% c("l50", "RSR")){
      
      plot <-
        plot +  
        scale_y_continuous(breaks=seq(inset.ymin, inset.ymax, 0.01),
                           labels=sprintf("%4.2f", seq(inset.ymin, inset.ymax, 0.01))) ## y-axis of inset
      
    }
    
    if(i=="35e15"){
      
      plot <-
        plot +  
        scale_y_continuous(breaks=seq(inset.ymin, inset.ymax, 0.1),
                           labels=sprintf("%4.1f", seq(inset.ymin, inset.ymax, 0.1))) ## y-axis of inset
      
    }
    
    plot <-
      plot +
      geom_magnify(from = c(inset.xmin, inset.xmax, 
                            inset.ymin, inset.ymax), ## create inset
                   to = c(position.inset.xmin, 
                          position.inset.xmax, 
                          position.inset.ymin, 
                          position.inset.ymax), 
                   axes = "xy") +
      scale_x_continuous(breaks=seq(label.xmin, label.xmax, 5), ## x-axis of plot
                         labels=rev(rev(c(rbind(seq(label.xmin, label.xmax, 10), "")))[-1])) ## only label every other tick
    
    if(i %in% c("l50", "RSR")){
      
      plot <-
        plot +
        scale_y_continuous(breaks=seq(label.ymin, label.ymax, 0.05),
                           labels=sprintf("%4.2f", seq(label.ymin, label.ymax, 0.05))) ## y-axis of plot
      
    }
    
    if(i=="35e15"){
      
      plot <-
        plot +
        scale_y_continuous(breaks=seq(label.ymin, label.ymax, 0.5),
                           labels=sprintf("%4.1f", seq(label.ymin, label.ymax, 0.5))) ## y-axis of plot
      
    }
    
    plot <-
      plot +
      coord_cartesian(xlim = c(label.xmin, label.xmax), 
                      ylim = c(label.ymin, label.ymax)) +
      theme(axis.text = element_text(size = 14),
            axis.title.x = element_text(size = 16, vjust = -1),
            axis.title.y = element_text(size = 16, vjust = 2.5)) ## adjust other characteristics of plot
    
    ## save plot as .pdf and .svg file
    cairo_pdf(here::here("out",  "plots", paste0(i, "-",  peer, "-", min(decades), "-", max(decades+9), ".pdf")),
              width=10, height=7)
    
    print(plot)
    
    dev.off()
    
    svg(here::here("out",  "plots", paste0(i, "-",  peer, "-", min(decades), "-", max(decades+9), ".svg")),
        width=10, height=7)
    
    print(plot)
    
    dev.off()
    
  })
  
  ###############
  # SAVE OUTPUT #
  ###############
  
  save(CCPM.births.bl, CCPM.births.cf, CCPM.diff.births, 
       both.births.combined, diff.births.combined,
       formal_indicators.df,
       years,
       color.avg, color.peer, color.usa,
       color.additional.2, color.additional.3, color.missing.2, color.missing.3,
       file=here::here("out", "data", paste0("output-", peer, "-", min(years), "-", max(years), ".RData")))
  
  ## clean environment
  rm(CCPM.births.bl, CCPM.births.cf, CCPM.diff.births, 
     both.births.combined, diff.births.combined,
     formal_indicators.df)
  
} ## end of loop

#################################
# VALUES REPORTED IN MANUSCRIPT #
#################################

## clear environment
rm(list = ls())

## load output data set
load(here::here("out", "data", "output-OWN-1950-2049.RData"))

## probability to survive until age 50
round(
formal_indicators.df$Value[formal_indicators.df$Indicator=="l50" &
                           formal_indicators.df$Year==2019 & 
                           formal_indicators.df$Name=="United States of America"]*1000
)/1000

round(
formal_indicators.df$Value[formal_indicators.df$Indicator=="l50" &
                           formal_indicators.df$Year==2019 & 
                           formal_indicators.df$Name=="Average"]*1000
)/1000

## temporary life expectancy
formal_indicators.df$Year[formal_indicators.df$Indicator=="35e15" &
                          formal_indicators.df$Value>=34.5 & 
                          formal_indicators.df$Name=="United States of America"][1]

formal_indicators.df$Year[formal_indicators.df$Indicator=="35e15" &
                          formal_indicators.df$Value>=34.5 & 
                          formal_indicators.df$Name=="Average"][1]

round(
(formal_indicators.df$Value[formal_indicators.df$Indicator=="35e15" &
                            formal_indicators.df$Year==2019 & 
                            formal_indicators.df$Name=="Average"] -
formal_indicators.df$Value[formal_indicators.df$Indicator=="35e15" &
                           formal_indicators.df$Year==2019 & 
                           formal_indicators.df$Name=="United States of America"])*12*10
)/10

## reproduction-survival ratio
round(
formal_indicators.df$Value[formal_indicators.df$Indicator=="RSR" &
                           formal_indicators.df$Year==2019 & 
                           formal_indicators.df$Name=="United States of America"]*1000
)/1000

round(
  formal_indicators.df$Value[formal_indicators.df$Indicator=="RSR" &
                               formal_indicators.df$Year==2019 & 
                               formal_indicators.df$Name=="Average"]*1000
)/1000 

## quality check: do births in step-wise projection sum to births in standard projection? 
aux <-
  diff.births.combined %>% 
  summarize(Population = sum(Population), .by = c(Year, Indicator)) %>% 
  complete(Year, Indicator, fill = list(Population = 0)) %>%
  left_join(
    
    both.births.combined %>% 
      summarize(Population.both = sum(Population), .by = Year)
    
  ) %>% 
  mutate(Population = ifelse(Indicator == "Additional", Population.both + Population, Population.both - Population)) %>% 
  select(-Population.both) %>% 
  group_split(Indicator, .keep = FALSE)
              
all.equal(aux[[1]] %>% filter(Year %in% years) %>% data.frame, 
          CCPM.births.bl %>% filter(Year %in% years))

all.equal(aux[[2]] %>% filter(Year %in% years) %>% data.frame, 
          CCPM.births.cf %>% filter(Year %in% years))

## first year in which BL births < CF births
CCPM.diff.births %>% 
  filter(Population < 0) %>% 
  pull(Year) %>% 
  min()

## missing and additional births in 2019, second generation
1000 * round(
  diff.births.combined %>% 
  filter(Year %in% 2019,
         Indicator == "Missing",
         Generation == 2) %>% 
  summarize(Population = sum(Population)) %>% 
  pull(Population) / 1000)

## missing births in 2010-2019, second generation
1000 * round(
  diff.births.combined %>% 
  filter(Year %in% 2010:2019,
         Indicator == "Missing",
         Generation == 2) %>% 
  summarize(Population = sum(Population)) %>% 
  pull(Population) / 1000)

#####################################################
# SUPPLEMENTARY PLOT OF ADDITIONAL / MISSING BIRTHS #
#####################################################

## determine y-axis range
births.min <-
  diff.births.combined %>% 
  filter(Year %in% years,
         Population < 0) %>% 
  summarize(Population = sum(Population), .by = Year) %>% 
  pull(Population) %>% 
  min()

births.max <-
  diff.births.combined %>% 
  filter(Year %in% years,
         Population > 0) %>% 
  summarize(Population = sum(Population), .by = Year) %>% 
  pull(Population) %>% 
  max()

births.min <- plyr::round_any(births.min / 1000, f = floor, accuracy = 5)  
births.max <- plyr::round_any(births.max / 1000, f = ceiling, accuracy = 5)

## plot
diff.births.plot <-
  diff.births.combined %>% 
  filter(Year %in% years) %>% 
  summarize(Population = sum(Population), .by = c(Year, Indicator, Generation)) %>% 
  complete(Year, Indicator, Generation, fill = list(Population = 0)) %>%
  mutate(Generation = factor(Generation, levels = c("3+", "2"))) %>% 
  ggplot(aes(x = Year, y = Population / 1000, fill = interaction(Indicator, Generation))) +
  geom_area(stat = "identity", position = "stack") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +    
  geom_line(data = CCPM.diff.births %>% filter(Year %in% years), 
            aes(x = Year, y = Population / 1000, linetype = "Baseline \U2013 Counterfactual"), 
            color = "black", linewidth = 0.5, inherit.aes = FALSE) +  
  labs(x = "Year", y = "Births (in thousands)") +
  scale_x_continuous(breaks = seq(min(years), max(years + 1), 5),
                     labels = rev(rev(c(rbind(seq(min(years), max(years + 1), 10), "")))[-1])) +
  scale_y_continuous(breaks = seq(births.min, births.max, 5)) +
  scale_fill_manual("", 
                    breaks = c("Additional.2", "Additional.3+", 
                               "Missing.2", "Missing.3+"),
                    labels = c("Additional, 2nd generation", 
                               "Additional, 3rd+ generation",
                               "Missing, 2nd generation",
                               "Missing, 3rd+ generation"),
                    values = c(color.additional.2, color.additional.3, color.missing.2, color.missing.3)) +
  scale_linetype_manual("", values = "longdash") +
  guides(fill = guide_legend(nrow = 2, byrow = FALSE, order = 1)) +
  theme_bw() +
  theme(aspect.ratio = 1,
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16, vjust = -1),
        axis.title.y = element_text(size = 16, vjust = 2.5),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.text = element_text(size = 12)) +
  coord_cartesian(xlim = c(min(years), max(years + 1)),
                  ylim = c(births.min, births.max), 
                  clip = "on") 

## save plot as .pdf and .svg file
cairo_pdf(here::here("out",  "plots", paste0("supp-diff-births-OWN-", min(years), "-", max(years), ".pdf")),
          width=8, height=8)

print(diff.births.plot)

dev.off()

svg(here::here("out",  "plots", paste0("supp-diff-births-OWN-", min(years), "-", max(years), ".svg")),
    width=8, height=8)

print(diff.births.plot)

dev.off()
