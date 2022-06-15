############
#MAIN FILE#
##########

#clear environment
rm(list=ls())

#install and load packages

##install.packages("cowplot")
##devtools::install_github("NicolasH2/ggbrace")
##install.packages("ggh4x")
##install.packages("ggprism")
##install.packages("here")
##install.packages("HMDHFDplus")
##install.packages("strex")
##install.packages("svglite")
##install.packages("tidyverse")
##install.packages("tidyr")

library(cowplot)
library(ggbrace)
library(ggh4x)
library(ggprism)
library(here)
library(HMDHFDplus)
library(strex)
library(svglite)
library(tidyverse)
library(tidyr)

#load life table functions
source(here("scripts", "99-life table functions.R"))

#set options
options(digits=10)

#data preparation
source(here("scripts", "01-preparation.R"))

#descriptive analyses
source(here("scripts", "02-descriptives.R"))

#main projections
source(here("scripts", "03-projections.R"))

#sensitivity analyses
source(here("scripts", "04-sensitivity analyses.R"))

#plots
source(here("scripts", "05-plots.R"))

#appendix
source(here("scripts", "06-appendix.R"))
