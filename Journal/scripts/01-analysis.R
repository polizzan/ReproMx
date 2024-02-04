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

for(i in c("data.table", "gdata", "ggmagnify", "here", 
           "plyr", "RColorBrewer", "svglite", "tidyverse")){

if(system.file(package=i)==""){install.packages(i)}

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
years <-1950:2049

## decades shown in plots
decades <- seq(1950, 2010, 10)

## first decade to include in inset of line plots [demographic indicators]
inset.decade <- 1980

## determine color palette for bar plots [missing americans & missing births]
color.missing <- RColorBrewer::brewer.pal(n=12, "Paired")[6]
color.additional <- RColorBrewer::brewer.pal(n=12, "Paired")[2]

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

#####################
# CLEAR ENVIRONMENT #
#####################

## keep only necessary objects
gdata::keep(unwpp.combined, countries,
            peer.list, years, decades, inset.decade, 
            color.missing, color.additional,
            color.usa, color.peer, color.avg, 
            CCPM, life.table, sure = TRUE)

###########################################################
# GENERATE LOOKUP TABLE FOR PLOTS OF PROJECTION ESTIMATES #
###########################################################

## table contains necessary information for 
## - selecting R objects 
## - choosing file names
## - rounding estimates
## - choosing time period to be plotted
proj.table <-
  rbind(
    c("missing.americans.direct", "MA-direct", 50, min(decades), max(decades+9)), 
    c("missing.americans.total", "MA-total", 500, min(decades+1), max(decades+10)), ## total missing americans are counted on january 1 of following year
    c("missing.births", "MB", 5, min(decades), max(decades+9)),
    c("missing.births.yomd", "MB-YOMD", 10, min(decades), max(decades+9)), ## yomd = 'year of maternal death'
    c("missing.births.cumulative", "MB-cumulative", 100, min(years), max(years)) ## cumulative number of missing births shown for entire projection period
  ) %>% data.frame()

## assign variable names
names(proj.table) <- c("df", "file", "breaks", "year.min", "year.max")

## make variables numeric
proj.table$breaks <- as.numeric(proj.table$breaks)
proj.table$year.min <- as.numeric(proj.table$year.min)
proj.table$year.max <- as.numeric(proj.table$year.max)

###############################
# LOOP THROUGH PEER COUNTRIES #
###############################

walk(peer.list, function(peer){ ## start of loop

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

#######################################
# MISSING AMERICANS & BIRTHS [DIRECT] #
#######################################

## definition 'direct': 
## (births to) americans that 'went missing' in a given year

direct.missing <- 
 map(years, function(x){ ## loop over all years

  ## female starting population
  pop.f <-
   unwpp.df %>% 
   filter(Name=="United States of America",
          Year==x) %>% 
   pull(Population) %>% 
   as.matrix()
   
  ## male starting population: female-dominant projection 
  pop.m <- as.matrix(rep(0, dim(pop.f)[1]))
   
  ## female survival probabilities: baseline 
  Sx.f.bl <-
    unwpp.df %>% 
    filter(Name=="United States of America",
           Year==x) %>% 
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
           Year==x) %>% 
    select(Year, Age, Sx) %>% 
    pivot_wider(names_from=Year,
                values_from=Sx) %>% ## reshape to wide data set
    select(-Age) %>% 
    as.matrix() %>% 
    unname()
  
  Sx.m <- ## male survival probabilities: female-dominant projection
    mig.f <- ## female migration: closed projection
    mig.m <- ## male migration: closed projection
    matrix(0, nrow=dim(Sx.f.bl)[1], ncol=dim(Sx.f.bl)[2])
  
  ## female fertility rates
  Fx <- 
    unwpp.df %>% 
    filter(Name=="United States of America",
           Year==x) %>% 
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
           Year==x,
           Age==0) %>% 
    pull(SRB)
  
  ## baseline projection
  proj.bl <-
    CCPM(Pop_f=pop.f, 
         Pop_m=pop.m, 
         Sx_f=Sx.f.bl, 
         Sx_m=Sx.m, 
         Fx=Fx,
         SRB=SRB, 
         Migra_f=mig.f, 
         Migra_m=mig.m, 
         stochastic=FALSE,
         out="Projection") %>% ## output: population
    data.frame()
  
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
         out="Births") %>% ## output: live births
    data.frame()
  
  ## counterfactual projection
  proj.cf <-
    CCPM(Pop_f=pop.f, 
         Pop_m=pop.m, 
         Sx_f=Sx.f.cf, 
         Sx_m=Sx.m, 
         Fx=Fx,
         SRB=SRB, 
         Migra_f=mig.f, 
         Migra_m=mig.m, 
         stochastic=FALSE,
         out="Projection") %>% ## output: population
    data.frame()
  
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
         out="Births") %>% ## output: live births
    data.frame()
  
  ## assign variable names
  names(proj.bl) <- 
    names(proj.cf) <- c("Population", "Age", "Sex", "Year")
  
  names(births.bl) <- 
    names(births.cf) <- c("Population", "Sex", "Year")

  ## missing births (male and female): observed - counterfactual births
  missing.births <-
    sum(births.bl$Population) -
      sum(births.cf$Population)
  
  ## missing female americans at age 0: 
  ## subject observed female live births to two survivorship scenarios + take difference
  missing.zero <-
    births.bl$Population[births.bl$Sex==0] * Sx.f.bl[1, 1] - 
      births.bl$Population[births.bl$Sex==0] * Sx.f.cf[1, 1]
  
  ## missing female americans in all other age groups: 
  ## difference between observed and counterfactual female population at end of projection
  missing.rest <-
    proj.bl$Population[proj.bl$Age>0 & proj.bl$Sex==0 & proj.bl$Year==1] -
      proj.cf$Population[proj.cf$Age>0 & proj.cf$Sex==0 & proj.cf$Year==1]
  
  ## combine missing americans at different ages
  missing.pop <-
    c(missing.zero, missing.rest)
  
  ## combine missing births and missing americans
  df.births <- data.frame(Age=-1, Population=missing.births, Year=x, Start=x)
  df.pop <- data.frame(Age=0:85, Population=missing.pop, Year=x+1, Start=x)
  
  missing <-
    df.births %>% 
    add_row(df.pop)
  
  missing

})

direct.df <-
  data.table::rbindlist(direct.missing) %>% ## combine year-wise estimates
  mutate(Method="direct")

#########################################
# MISSING AMERICANS & BIRTHS [INDIRECT] #
#########################################

## definition 'indirect': 
## in a given year,
## (births to) americans that 'went missing' in all previous years
## and that would still be alive under peer mortality conditions

indirect.missing <-
  map(years[2]:years[length(years)], function(x){ ## loop over all years: by definition, no missing americans on january 1 of first projection year
  
    ## female starting population = missing female americans
    pop.f <-
      direct.df %>% 
      filter(Age>=0,
             Year==x) %>% 
      pull(Population) %>% 
      as.matrix()
    
    ## male starting population: female-dominant projection
    pop.m <- as.matrix(rep(0, dim(pop.f)[1]))
    
    ## female survival probabilities: counterfactual 
    Sx.f.cf <-
      unwpp.df %>% 
      filter(Name=="Average",
             Year %in% x:years[length(years)]) %>% ## missing americans projected forward until end of projection period
      select(Year, Age, Sx) %>% 
      pivot_wider(names_from=Year,
                  values_from=Sx) %>% ## reshape to wide data set
      select(-Age) %>% 
      as.matrix() %>% 
      unname()
    
    ## adjust survivorship ratios
    for(j in 1:dim(Sx.f.cf)[2]){

      i <- ifelse(j < dim(Sx.f.cf)[1], j, dim(Sx.f.cf)[1]) 
    
      ## male and female children of missing americans
      ## do not survive to have children of their own
      ## i.e., only missing americans in second generation are considered
      Sx.f.cf[c(1:i), j] <- 0
  
    }  
    
    Sx.m <- ## male survival probabilities: female-dominant projection
      mig.f <- ## female migration: closed projection
      mig.m <- ## male migration: closed projection
      matrix(0, nrow=dim(Sx.f.cf)[1], ncol=dim(Sx.f.cf)[2])
    
    ## female fertility rates
    Fx <- 
      unwpp.df %>% 
      filter(Name=="United States of America",
             Year %in% x:years[length(years)]) %>% 
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
             Year %in% x:years[length(years)],
             Age==0) %>% 
      pull(SRB)
    
    ## missing female americans
    proj.cf <-
      CCPM(Pop_f=pop.f, 
           Pop_m=pop.m, 
           Sx_f=Sx.f.cf, 
           Sx_m=Sx.m, 
           Fx=Fx,
           SRB=SRB, 
           Migra_f=mig.f, 
           Migra_m=mig.m, 
           stochastic=FALSE,
           out="Projection") %>% ## output: population
      data.frame()
    
    ## missing births
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
           out="Births") %>% ## output: live births
      data.frame()
    
    names(proj.cf) <- c("Population", "Age", "Sex", "Year")
    names(births.cf) <- c("Population", "Sex", "Year")  
     
    ## missing female americans
    proj.cf <- proj.cf[proj.cf$Year!=0 & proj.cf$Sex!=1, ] ## exclude base year = "direct", keep only females
    proj.cf <- subset(proj.cf, select=-Sex) ## drop sex variable
    
    ## adjust year variable
    proj.cf$Year <- proj.cf$Year + x
    births.cf$Year <- births.cf$Year + x
    
    ## missing births
    missing.births <-
      births.cf %>% 
      summarize(Population=sum(Population), .by=Year) 
    
    ## add age variable
    missing.births$Age <- -1
    
    ## add starting year
    missing.births$Start <- proj.cf$Start <- x - 1
    
    ## combine missing americans and missing births
    missing <- 
      missing.births %>% 
      add_row(proj.cf)
    
    missing
      
  })
  
indirect.df <-
  data.table::rbindlist(indirect.missing) %>% ## combine year-wise estimates
  mutate(Method="indirect")

##########################################
# GENERATE AND PLOT PROJECTION ESTIMATES #
##########################################

## combine direct and indirect projection estimates
missing.df <-
  direct.df %>% 
    add_row(indirect.df) 

## 1) americans that 'went missing' each year 
missing.americans.direct <-
  missing.df %>% 
  filter(Method=="direct", Age!=-1) %>% 
  summarize(Population=sum(Population), .by=c(Start)) %>% 
  arrange(Start) %>% 
  rename(Year=Start)

## 2) annual 'stock' of missing americans 
missing.americans.total <-
  missing.df %>% 
  filter(Age!=-1) %>% 
  summarize(Population=sum(Population), .by=c(Year)) %>% 
  arrange(Year)

## 3) missing births 
missing.births <-
  missing.df %>% 
  filter(Age==-1) %>% 
  summarize(Population=sum(Population), .by=c(Year)) %>% 
  arrange(Year)

## 4) missing births by year of maternal death
missing.births.yomd <-
  missing.df %>% 
  filter(Age==-1) %>% 
  summarize(Population=sum(Population), .by=c(Start, Year)) %>% 
  arrange(Start, Year)

## 5) missing births, cumulative
missing.births.cumulative <-
  missing.births %>% 
  mutate(Population=cumsum(Population))

## plot 
walk(1:dim(proj.table)[1], function(x){ ## loop over rows of lookup table

  ## pull values from correct row of lookup table
  df <- get(proj.table$df[x])
  breaks <- proj.table$breaks[x]
  period <- proj.table$year.min[x]:proj.table$year.max[x]
  label <- ifelse(str_detect(proj.table$file[x], "MA"), "Americans", "births")
  file <- proj.table$file[x]
  
  ## adjust variables for: missing births by year of maternal death
  if(proj.table$df[x]=="missing.births.yomd"){
    
    df <-
      df %>% 
      filter(Year %in% period) %>% ## consider only births within selected period
      summarize(Population=sum(Population), .by=Start) %>% ## sum by year of maternal death
      rename(Year=Start)
    
  }
  
  ## axis ranges
  values <- df$Population[df$Year<=max(period)] / 1000 ## pull values from specified period
  
  range.x.min <- plyr::round_any(min(period), f=floor, accuracy=10)  ## round years
  range.x.max <- plyr::round_any(max(period), f=ceiling, accuracy=10) 
  
  range.y.min <- plyr::round_any(min(values), f=floor, accuracy=breaks) ## round values
  range.y.max <- plyr::round_any(max(values), f=ceiling, accuracy=breaks)
  
  ## pull values for positioning labels and arrows
  label.first.x <-
    df %>% 
      filter(Year<=max(period)) %>% 
      mutate(Indicator=ifelse(Population > 0, 0, 1)) %>% 
      group_by(Indicator) %>% 
      slice_head(n=2) %>%
      slice_tail(n=1) %>% 
      pull(Year)

  label.second.x <-  
    df %>% 
      filter(Year<=max(period)) %>% 
      mutate(Indicator=ifelse(Population > 0, 0, 1)) %>% 
      group_by(Indicator) %>% 
      slice_tail(n=2) %>% 
      slice_head(n=1) %>% 
      pull(Year)
  
  label.first.y <-
    df %>% 
    filter(Year %in% label.first.x) %>% 
    mutate(Population=Population/1000) %>% 
    pull(Population)
    
  label.second.y <-
    df %>% 
    filter(Year %in% label.second.x) %>% 
    mutate(Population=Population/1000) %>% 
    pull(Population)
  
  ## create data set for plotting
  plot.df <-
    df %>% 
      filter(Year %in% period) %>% 
      mutate(fill = ifelse(Population > 0, 0, 1))
  
  ## create auxiliary data set for black outline (geom_step) 
  ## currently only works if first year has positive values and last year has negative values
  aux.df <-
    plot.df %>% 
    select(-fill) %>% 
    add_row(
      plot.df %>% 
        select(-fill) %>% 
        filter(Year == max(period)) %>% 
        mutate(Year = max(period) + 1)
    ) %>% 
    add_row(
      data.frame(Year = c(min(period), max(period) + 1),
                 Population = c(0, 0))
    ) %>% arrange(Year, Population)
    
  ## generate plot
  plot <-
    ggplot(plot.df, 
           aes(x = Year, y = Population / 1000, fill = as.factor(fill))) + ## fill according to sign
      geom_bar(stat = "identity", show.legend = FALSE, color = "transparent", width = 1) +
      geom_step(data = aux.df, aes(x = Year - 0.5, y = Population / 1000), inherit.aes = FALSE, color = "black", linewidth = 0.05) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.05) +
      scale_fill_manual(values = c(color.additional, color.missing)) +
      labs(x = "Year", y = paste(str_to_title(label), "(in thousands)")) +
      scale_y_continuous(breaks = seq(range.y.min, range.y.max, breaks)) +
      scale_x_continuous(breaks = seq(range.x.min, range.x.max, 5),
                         labels = rev(rev(c(rbind(seq(range.x.min, range.x.max, 10), "")))[-1])) + ## label only every other tick
      theme_bw() +
      theme(aspect.ratio = 1,
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.text = element_text(size = 14),
            axis.title.x = element_text(size = 16, vjust = -1),
            axis.title.y = element_text(size = 16, vjust = 2.5),
            axis.line = element_line(colour = "black")
      ) +
      coord_cartesian(xlim = c(range.x.min - 5, range.x.max + 5),
                      ylim = c(range.y.min, range.y.max), 
                      clip = "off") +
    
      ## add labels and arrows
      geom_text(aes(x = (label.first.x[1] + label.second.x[1])/2, y = range.y.min/2, 
                    label = paste0("Missing ", label, " because\nU.S. mortality > peer mortality")), 
                hjust = 0.5, vjust = 1, color = color.missing) + ## add label 1 
      
      geom_path(data = data.frame(
        x = c(label.first.x[2], label.first.x[2], (label.first.x[1] + label.second.x[1])/2, (label.first.x[1] + label.second.x[1])/2),
        y = c(label.first.y[2], (label.first.y[2] + range.y.min/2)/2, (label.first.y[2] + range.y.min/2)/2, 0.95 * range.y.min/2)
      ),
      aes(x = x, y = y), inherit.aes = FALSE,
      arrow = arrow(length = unit(0.15, "cm"), type = "open", angle = 30, ends = "last"),
      lineend = "round", linewidth = 0.5, color = "black") + ## add arrow to label 1
      
      geom_text(aes(x = (label.first.x[2] + label.second.x[2])/2, y = range.y.max/2, 
                    label = paste0("Additional ", label, " because\nU.S. mortality < peer mortality")), 
                hjust = 0.5, vjust = 0, color = color.additional) + ## add label 2
    
      geom_path(data = data.frame(
        x = c(label.second.x[1], label.second.x[1], (label.first.x[2] + label.second.x[2])/2, (label.first.x[2] + label.second.x[2])/2),
        y = c(label.second.y[1], (label.second.y[1] + range.y.max/2)/2, (label.second.y[1] + range.y.max/2)/2, 0.95 * range.y.max/2)
      ),
      aes(x = x, y = y), inherit.aes=FALSE,
      arrow = arrow(length = unit(0.15, "cm"), type = "open", angle = 30, ends = "last"),
      lineend = "round", linewidth = 0.5, color = "black") ## add arrow to label 2
    
  ## save plot as .pdf and .svg file
  cairo_pdf(here::here("out",  "plots", paste0(file, "-",  peer, "-", min(period), "-", max(period), ".pdf")),
            width=8, height=7)
  
  print(plot)
  
  dev.off()
  
  svg(here::here("out",  "plots", paste0(file, "-",  peer, "-", min(period), "-", max(period), ".svg")),
      width=8, height=7)
  
  print(plot)
  
  dev.off()

})

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

save(missing.df, 
     missing.americans.direct, missing.americans.total,
     missing.births, missing.births.yomd, missing.births.cumulative,
     formal_indicators.df,
     file=here::here("out", "data", paste0("output-", peer, "-", min(years), "-", max(years), ".RData")))

}) ## end of loop

#################################
# VALUES REPORTED IN MANUSCRIPT #
#################################

## clear environment
rm(list = ls())

## load output data set
load(here::here("out", "data", "output-OWN-1950-2049.RData"))

## missing births in 2019
1000*round(missing.births$Population[missing.births$Year==2019]/1000)

## missing births in 2010-2019
1000*round(sum(missing.births$Population[missing.births$Year %in% 2010:2019])/1000)

## missing female americans, direct, in 2019
1000*round(missing.americans.direct$Population[missing.americans.direct$Year==2019]/1000)

## missing female americans, direct + indirect, on january 1, 2020
1000*round(missing.americans.total$Population[missing.americans.total$Year==2020]/1000)

## additional births
missing.births %>% 
  filter(Population > 0) %>% 
  pull(Year)

missing.births %>% 
  filter(Population > 0) %>% 
  pull(Population) %>% 
  sum()

## missing births
missing.births %>% 
  filter(Population < 0) %>% 
  pull(Year)

missing.births %>% 
  filter(Population < 0, Year <= 2019) %>% 
  pull(Population) %>% 
  sum()

## additional births due to more favorable mortality
missing.births.yomd %>% 
  filter(Year <= 2019) %>% 
  summarize(Population = sum(Population), .by = Start) %>% 
  filter(Population > 0) %>% 
  pull(Start)

missing.births.yomd %>% 
  filter(Year <= 2019) %>% 
  summarize(Population = sum(Population), .by = Start) %>% 
  filter(Population > 0) %>% 
  pull(Population) %>% 
  sum()

## missing births due to less favorable mortality
missing.births.yomd %>% 
  filter(Year <= 2019) %>% 
  summarize(Population = sum(Population), .by = Start) %>% 
  filter(Population < 0) %>% 
  pull(Start)

missing.births.yomd %>% 
  filter(Year <= 2019) %>% 
  summarize(Population = sum(Population), .by = Start) %>% 
  filter(Population < 0) %>% 
  pull(Population) %>% 
  sum()

## year in which cumulative number of missing births reaches zero for the first time
missing.births.cumulative %>% 
  filter(Population < 0) %>% 
  slice_head(n = 1) %>% 
  pull(Year)

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
