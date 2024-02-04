####################################
#COHORT COMPONENT PROJECTION MODEL#
##################################

#US: person-years and/or (lost) births when: 
#using observed all-cause mortality + observed ASFRs
#fixing all-cause mortality at 2010 levels (Scenario 1)
#setting all-cause mortality to EU levels (Scenario 2)
#fixing ASFRs at 2010 levels + observed all-cause mortality (Scenario 3),

PY.US.12to55.2010to2019.observed <-
PY.US.12to55.2010to2019.nMx.all.cause.fixed <-
PY.US.12to55.2010to2019.nMx.EU <-
  data.frame(matrix(nrow=44, ncol=10)) ##ages 12:55 (=44), years 2010-2019 (=10)

names(PY.US.12to55.2010to2019.observed) <-
names(PY.US.12to55.2010to2019.nMx.all.cause.fixed) <-
names(PY.US.12to55.2010to2019.nMx.EU) <-
  2010:2019
  
df1 <- df2 <- df3 <-
  data.frame(matrix(nrow=44, ncol=11)) ##projected population on 1st January 2010-2020 (=11), ages 12-55 (=44)

names(df1) <- names(df2) <- names(df3) <- 2010:2020

x1 <- x2 <- x3 <- nNx.US.2to55.2010to2019 %>% filter(Year==2010) %>% pull(nNx) ##starting population on 1st January 2010, ages 2-55

df1[, "2010"] <- df2[, "2010"] <- df3[, "2010"] <- tail(x1, 44) ##starting population on 1st January 2010, ages 12-55

for (i in 2010:2019) {

y <-  nIx.US.2to55.2010to2019[, paste(i)]/2 ##migration in projection interval
z1 <- nMx.US.2to55.2010to2019 %>% filter(Year==i) %>% pull(nMx) ##mortality rates (observed & Scenario 3)
z2 <- nMx.US.2to55.2010to2019.fixed %>% filter(Year==i) %>% pull(nMx) ##mortality rates (Scenario 1)
z3 <- nMx.EU.2to55.2010to2019 %>% filter(Year==i) %>% pull(nMx) ##mortality rates (Scenario 2)

x1 <-  (lag(x1) + lag(y)) * nLx(nmx=z1)/lag(nLx(nmx=z1)) + y ##see Preston et al. (2001: 128)
x2 <-  (lag(x2) + lag(y)) * nLx(nmx=z2)/lag(nLx(nmx=z2)) + y ##population at end of projection interval = starting population for next projection interval
x3 <-  (lag(x3) + lag(y)) * nLx(nmx=z3)/lag(nLx(nmx=z3)) + y

df1[, paste(i+1)] <- tail(x1, 44) ##population aged 12-55 at end of projection interval 
df2[, paste(i+1)] <- tail(x2, 44)
df3[, paste(i+1)] <- tail(x3, 44)

PY.US.12to55.2010to2019.observed[, paste(i)] <-  
  rowMeans(cbind(df1[, paste(i)] + y[11:54], df1[, paste(i+1)])) ##= person-years lived between ages 12-55 in projection interval (see Preston et al. 2001: 128)

PY.US.12to55.2010to2019.nMx.all.cause.fixed[, paste(i)] <-
  rowMeans(cbind(df2[, paste(i)] + y[11:54], df2[, paste(i+1)]))

PY.US.12to55.2010to2019.nMx.EU[, paste(i)] <-
  rowMeans(cbind(df3[, paste(i)] + y[11:54], df3[, paste(i+1)]))

}  

#multiply person-years with ASFRs to obtain number of births between 2010-2019
Births.US.12to55.2010to2019.observed <-
  ASFR.US.12to55.2010to2019 * PY.US.12to55.2010to2019.observed ##observed

Births.US.12to55.2010to2019.nMx.all.cause.fixed <-
  ASFR.US.12to55.2010to2019 * PY.US.12to55.2010to2019.nMx.all.cause.fixed ##Scenario 1

Births.US.12to55.2010to2019.nMx.EU <-
  ASFR.US.12to55.2010to2019 * PY.US.12to55.2010to2019.nMx.EU ##Scenario 2

Births.US.12to55.2010to2019.ASFR.fixed <-
  PY.US.12to55.2010to2019.observed %>% 
  mutate(across(everything(), ~ .x * ASFR.US.12to55.2010to2019 %>% pull(`2010`))) ##Scenario 3

#calculate person-years lost in Scenario 1
PY.lost.12to55.2010to2019 <- 
  PY.US.12to55.2010to2019.observed-
  PY.US.12to55.2010to2019.nMx.all.cause.fixed

#Scenario 1: lost births by birth order
Births.lost.US.12to55.2010to2019.birth.order <- 
  array(dim=c(44, 10, 5), dimnames=list(1:44, 2010:2019, 1:5)) ##birth order 1, 2, 3, 4, 5+

for (i in 1:5) {
  Births.lost.US.12to55.2010to2019.birth.order[, , i] <-
    as.matrix(
      PY.lost.12to55.2010to2019 * 
        ASFR.bo.US.12to55.2010to2019 %>% 
        select(Year, Age, paste0("ASFR", i)) %>% 
        pivot_wider(names_from=Year,
                    values_from=paste0("ASFR", i)) %>% 
        select(-Age)
    )
}

#US: person-years and (lost) births when fixing cause-specific mortality
#(Scenario 4)
PY.US.12to55.2010to2019.nMx.cause.specific.fixed  <- 
  array(dim=c(44, 10, 13), dimnames=list(1:44, 2010:2019, unique(nMx.US.2to55.2010to2019.cause.specific$Cause)))

Births.US.12to55.2010to2019.nMx.cause.specific.fixed <- 
  array(dim=c(44, 10, 13), dimnames=list(1:44, 2010:2019, unique(nMx.US.2to55.2010to2019.cause.specific$Cause)))
    ##ages 12-55 (=44), years 2010-2019 (=10), 13 causes of death

Births.lost.US.12to55.2010to2019.cause.specific <- data.frame(matrix(nrow=13, ncol=2))
names(Births.lost.US.12to55.2010to2019.cause.specific) <- c("Cause", "Births Lost")
Births.lost.US.12to55.2010to2019.cause.specific$Cause <- unique(nMx.US.2to55.2010to2019.cause.specific$Cause)

for (j in unique(nMx.US.2to55.2010to2019.cause.specific$Cause)) {

df4 <- data.frame(matrix(nrow=44, ncol=11)) ##projected population on 1st January 2010-2020 (=11), ages 12-55 (=44)
names(df4) <- 2010:2020
  
x <- nNx.US.2to55.2010to2019 %>% filter(Year==2010) %>% pull(nNx) ##starting population on 1st January 2010, ages 2-55
  
df4[, "2010"] <- tail(x, 44) ##starting population on 1st January 2010, ages 12-55
  
for (i in 2010:2019) {
    
y <- nIx.US.2to55.2010to2019[, paste(i)]/2 ##migration in projection interval
z <- nMx.US.2to55.2010to2019.cause.specific %>% filter(Year==i & Cause==j) %>% pull(nMx) ##mortality rates for cause j (Scenario 4)

x <-  (lag(x) + lag(y)) * nLx(nmx=z)/lag(nLx(nmx=z)) + y ##see Preston et al. (2001: 128)
      ##population at end of projection interval = starting population for next projection interval

df4[, paste(i+1)] <- tail(x, 44) ##population aged 12-55 at end of projection interval 
    
PY.US.12to55.2010to2019.nMx.cause.specific.fixed[, paste(i), j] <- 
  rowMeans(cbind(df4[, paste(i)] + y[11:54], df4[, paste(i+1)])) ##= person-years lived between ages 12-55 in projection interval for each cause j
}  

#multiply person-years with ASFRs to obtain number of births between 2010-2019
Births.US.12to55.2010to2019.nMx.cause.specific.fixed[, , j] <-  
  PY.US.12to55.2010to2019.nMx.cause.specific.fixed[, , j] * as.matrix(ASFR.US.12to55.2010to2019)

#number of lost births = difference between observed and counterfactual
Births.lost.US.12to55.2010to2019.cause.specific$`Births Lost`[Births.lost.US.12to55.2010to2019.cause.specific$Cause==j] <- 
  sum(Births.US.12to55.2010to2019.observed-Births.US.12to55.2010to2019.nMx.cause.specific.fixed[, , j])

}
