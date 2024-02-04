#######################
#SENSITIVITY ANALYSES#
#####################

#adjustment factors
round(2.791/1.705, 3) ##2019 TFR ratio of 12th grade to average
round(1.284/1.705, 3) ##2019 TFR ratio of BA to average
round(2.350/1.931, 3) ##2010 TFR ratio of Hispanic to average
round(1.939/1.705, 3) ##2019 TFR ratio of Hispanic to average 
round(1.972/1.931, 3) ##2010 TFR ratio of non-Hispanic Black to average
round(1.774/1.705, 3) ##2019 TFR ratio of non-Hispanic Black to average
round(1.791/1.931, 3) ##2010 TFR ratio of non-Hispanic White to average
round(1.610/1.705, 3) ##2019 TFR ratio of non-Hispanic White to average
round(1.543/1.921, 3) ##TFR ratio of women w/ disease to average

#adjusted number of births lost in Scenario 1
round(sum(PY.lost.12to55.2010to2019*ASFR.US.12to55.2010to2019), 0) ##identical TFR
round(sum(PY.lost.12to55.2010to2019*(ASFR.US.12to55.2010to2019*2.791/1.705)), 0) ##12th grade TFR (2019)
round(sum(PY.lost.12to55.2010to2019*(ASFR.US.12to55.2010to2019*1.284/1.705)), 0) ##BA degree TFR (2019)
round(sum(PY.lost.12to55.2010to2019*(ASFR.US.12to55.2010to2019*2.350/1.931)), 0) ##Hispanic TFR (2010) 
round(sum(PY.lost.12to55.2010to2019*(ASFR.US.12to55.2010to2019*1.939/1.705)), 0) ##Hispanic TFR (2019) 
round(sum(PY.lost.12to55.2010to2019*(ASFR.US.12to55.2010to2019*1.972/1.931)), 0) ##non-Hispanic Black TFR (2010)
round(sum(PY.lost.12to55.2010to2019*(ASFR.US.12to55.2010to2019*1.774/1.705)), 0) ##non-Hispanic Black TFR (2019)
round(sum(PY.lost.12to55.2010to2019*(ASFR.US.12to55.2010to2019*1.791/1.931)), 0) ##non-Hispanic White TFR (2010)
round(sum(PY.lost.12to55.2010to2019*(ASFR.US.12to55.2010to2019*1.610/1.705)), 0) ##non-Hispanic White TFR (2019)
round(sum(PY.lost.12to55.2010to2019*(ASFR.US.12to55.2010to2019*1.543/1.921)), 0) ##women w/ disease TFR
