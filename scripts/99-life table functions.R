############
#FUNCTIONS#
##########

#derive nLx (single-age life table without age interval 0-1)
nLx <- function (nmx, radix=1) {
   
nax <- 0.5  ##average number of years lived in each age interval
nqx <- nmx/(1+(1-nax)*nmx) ##probability of death in each age interval (Preston et al. 2001: 47)
npx <- 1-nqx ##probability of survival in each age interval

lx <- rep(radix, length(nmx))

for (i in 2:length(nmx)) {

lx[i] <- lx[i-1] * npx[i-1] ##number of survivors at the beginning of each age interval

}

ndx <- lx * nqx ##number of deaths in each age interval
nLx <- lx * npx + nax * ndx   ##years lived in each age interval

return(nLx)

}

#derive temporary life expectancy from nmx/nMx
ex <- function(nmx, radix=1) {

nLx <- nLx(nmx=nmx, radix=radix) ##calculate number of years lived in each age interval
Tx <- rev(cumsum(rev(nLx))) ##calculate number of years lived above age x

ex <- Tx[1]/radix ##calculate temporary life expectancy

return(ex)

}