##################
# FUNCTIONS FILE #
##################

# ---

######################
# SURVIVORSHIP RATIO #
######################

## derive survivorship ratios, Sx, from age-specific mortality rates
life.table <- function(nmx, radix=1, female=TRUE){
  
  ## years lived by those who die in age interval, nax  
  nax <- rep(0.5, length(nmx))  
  
  if(female==TRUE){ ## nax in age interval 0-1 dependent on nmx and sex (Andreev & Kingkade 2015)
    
    if(nmx[1] >= 0 & nmx[1] < 0.01724){
      
      nax[1] <- 0.14903 - 2.05527 * nmx[1]
      
    }else{
      
      if(nmx[1] >= 0.01724 & nmx[1] < 0.06891){
        
        nax[1] <- 0.04667 + 3.88089 * nmx[1]
        
      }else{
        
        nax[1] <- 0.31411
        
      }
    }
    
  }else{
    
    if(nmx[1] >= 0 & nmx[1] < 0.02300){
      
      nax[1] <- 0.14929 - 1.99545 * nmx[1]
      
    }else{
      
      if(nmx[1] >= 0.02300 & nmx[1] < 0.08307){
        
        nax[1] <- 0.02832 + 3.26021 * nmx[1]
        
      }else{
        
        nax[1] <- 0.29915
        
      }
    }
  }
  
  nax[length(nmx)] <- 1/nmx[length(nmx)] ## nax in open-ended age interval = inverse of nmx
  
  ## probability of death, nqx (Preston et al. 2001: 49)
  nqx <- nmx/(1+(1-nax)*nmx) 
  nqx[length(nmx)] <- 1 ## nqx in open-ended age interval = 1
  
  ## probability of survival, npx
  npx <- 1-nqx 
  
  ## number of survivors at beginning of age interval, lx
  lx <- rep(radix, length(nmx))
  
  for(i in 2:length(nmx)){
    
    lx[i] <- lx[i-1] * npx[i-1] 
    
  }
  
  ## number of deaths in age interval, ndx
  ndx <- lx * nqx 
  
  ## years lived in age interval, nLx (Preston et al. 2001: 49)
  nLx <- lx * npx + nax * ndx 
  nLx[length(nmx)] <- lx[length(nmx)]/nmx[length(nmx)] ## years lived in open-ended age interval 

  ## years lived in and above age interval, Tx
  Tx <- rev(cumsum(rev(nLx)))
    
  ## survivorship ratios, Sx (Preston et al. 2001: chapter 6.3)
  Sx <- nLx/lag(nLx) 
  Sx[1] <- nLx[1]/radix ## survivorship ratio in age interval 0-1
  Sx[length(nmx)] <- nLx[length(nmx)]/(nLx[length(nmx)] + nLx[length(nmx)-1]) ## survivorship ratio in open-ended age interval

  return(data.frame(lx, nLx, Tx, Sx))
  
}

##########################
# COHORT COMPONENT MODEL #
##########################

## cohort component projection model
CCPM <- function(Pop_f, ## column vector of age-specific starting population (female)
                 Pop_m, ## column vector of age-specific starting population (male) 
                 Sx_f, ## matrix of age-specific survivorship ratios (female) 
                 Sx_m, ## matrix of age-specific  survivorship ratios (male)
                 Fx, ## matrix of age-specific fertility rates (female)
                 SRB, ## vector of sex ratios at birth (male to female)
                 Migra_f, ## matrix of age-specific migration counts (female)
                 Migra_m, ## matrix of age-specific migration counts (male)
                 stochastic=FALSE, ## determine if projection is stochastic or deterministic
                 out="Births"){ ## determine output: 'Births' (live births) or 'Projection' (population)
  
  births_f <- NULL ## vector of female births
  births_m <- NULL ## vector of male births
  
  Projection_f <- Pop_f ## column vector of female population, to be turned into a matrix
  Projection_m <- Pop_m ## column vector of male population, to be turned into a matrix
  
  for(j in 1:dim(Sx_f)[2]){ ## for each year of the projection period
  
    aux_f <- ## column vector of female/male population at start of next time interval
      aux_m <-
      matrix(0, 
             nrow=dim(Pop_f)[1],
             ncol=1)
    
    ## STEP I: age-wise survival of population to next age group
    for (i in 1:(dim(Pop_f)[1]-1)){
      
      aux_f[i+1, 1] <- ## female
        
        if(stochastic){
          
          rbinom(n=1, size=trunc(Projection_f[i, j]), prob=Sx_f[i+1, j]) ## trunc(): binomial requires integers for 'size' argument
      
        }else{
          
          Projection_f[i, j] * Sx_f[i+1, j]
          
        }
          
      aux_m[i+1, 1] <- ## male
        
        if(stochastic){
          
          rbinom(n=1, size=trunc(Projection_m[i, j]), prob=Sx_m[i+1, j])
      
        }else{
          
          Projection_m[i, j] * Sx_m[i+1, j]
          
      }  
    } 
    
    ## survival in open-ended age interval
    aux_f[dim(Pop_f)[1], 1] <- ## female
      
      if(stochastic){
        
        aux_f[dim(Pop_f)[1], 1] + rbinom(n=1, size=trunc(Projection_f[dim(Pop_f)[1], j]), prob=Sx_f[dim(Sx_f)[1], j])
    
      }else{
        
        aux_f[dim(Pop_f)[1], 1] + Projection_f[dim(Pop_f)[1], j] * Sx_f[dim(Sx_f)[1], j]
        
      }  
        
    aux_m[dim(Pop_m)[1], 1] <- ## male
      
      if(stochastic){
        
        aux_m[dim(Pop_m)[1], 1] + rbinom(n=1, size=trunc(Projection_m[dim(Pop_m)[1], j]), prob=Sx_m[dim(Sx_m)[1], j])
    
      }else{
        
        aux_m[dim(Pop_m)[1], 1] + Projection_m[dim(Pop_m)[1], j] * Sx_m[dim(Sx_m)[1], j]
        
      }  

    ## STEP II: fertility
    ## calculate total number of births
    births <- 1/2 * (Fx[, j] %*% Projection_f[, j] + 
                       Fx[, j] %*% aux_f)
    
    if(stochastic){
      
    aux0 <- rpois(n=1, lambda=c(births))
    
    }else{
      
    aux0 <- c(births)  
      
    }
    
    ## split into female and male births
    aux0_f <- aux0 * (1/(1+SRB[j]))
    aux0_m <- aux0 * (SRB[j]/(1+SRB[j]))
    
    ## STEP III: survival of newborns
    if(stochastic){
    
    aux_f[1, 1] <- rbinom(n=1, size=trunc(aux0_f), prob=Sx_f[1, j])
    aux_m[1, 1] <- rbinom(n=1, size=trunc(aux0_m), prob=Sx_m[1, j])
    
    }else{
      
    aux_f[1, 1] <- aux0_f * Sx_f[1, j]
    aux_m[1, 1] <- aux0_m * Sx_m[1, j]      
      
    }

    ## STEP IV: add migration
    aux_f <- aux_f + Migra_f[, j]
    aux_m <- aux_m + Migra_m[, j]
    
    ## STEP V: store projection results
    Projection_f <-
      cbind(Projection_f, aux_f) ## add vector of projected female population as new column to existing matrix
    
    Projection_m <-
      cbind(Projection_m, aux_m) ## add vector of projected male population as new column to existing matrix
    
    births_f <- c(births_f, aux0_f) ## add projected female births to existing vector
    births_m <- c(births_m, aux0_m) ## add projected male births to existing vector
    
  }
  
  ## STEP VI: generate matrix with projection output
  Projection <- 
    cbind(
      
      c(rbind(Projection_f, Projection_m)), ## population
      
      rep(0:(dim(Projection_f)[1]-1), 2 * dim(Projection_f)[2]), ## age
      
      rep(rep(c(0, 1), each = dim(Projection_f)[1]), dim(Projection_f)[2]), ## sex
      
      rep(0:(dim(Projection_f)[2]-1), each = 2 * dim(Projection_f)[1]) ## year
    
    )

  Births <-
    cbind(
      
      c(births_f, births_m), ## births
      
      rep(c(0, 1), each=length(births_f)), ## sex
      
      rep(0:(length(births_f)-1), 2) ## year
  
    )  
  
  ## return data set of interest: 'Projection' or 'Births'    
  return(get(out))
  
}
