# Load packages to run NS models
library(RMark) #to run Program MARK
library(tidyverse) #helps to handle and transform data
library(data.table) #for data manipulation and visuals
library(readr) #read in excel files that are not .csv
library(msm) #to use delta method to estimate variance components
library(ggplot2) #create figures using ggplot
library(plotrix) #compute simple statistics

# Add path to MARK.exe
Sys.setenv(MarkPath = "C:/Program Files (x86)/MARK")
mark.path <- "C:/Program Files (x86)/MARK"
options(mark.path = mark.path)
data("dipper")


# Load the SPEI data INP file
# spei_data<- read.csv("nest_survival/data/INP_20220921.csv", header = TRUE)
spei_data <- read_csv("C:/Users/shoepfner/Desktop/nest survival/Randall's work/INP_20220921.csv")
spei_data <- read_csv("C:/Users/shoepfner/Desktop/nest survival/Randall's work/INP_2026_Sarah.csv")

#keep just the 205 rows of data, millions of blank rows at teh end of the Excel file
spei_data <- head(spei_data, 200)  #last nest should be THK012

# Check the headers. Sometimes column name changes when reading csv files. 
head(spei_data)
summary(spei_data)

# Make year a factor variable
is.factor(spei_data$Year)
spei_data$Year<-as.factor(spei_data$Year)
is.factor(spei_data$Year) #it is now a factor variable

# # Make Site a factor variable
# is.factor(spei_data$Site)
# spei_data$Site<-as.factor(spei_data$Site)
# is.factor(spei_data$Site) #now a factor variable
# 
# # Square the sea ice variables. (Quadratic term and effect). This is adding another column.
# spei_data$Win_Hi2<-spei_data$Win_Hi^2
# #spei_data$Win_Lo2<-spei_data$Win_Lo^2 didn't use
# spei_data$Spr_Hi2<-spei_data$Spr_Hi^2
# #spei_data$Spr_Lo2<-spei_data$Spr_Lo^2 didn't use
# head(spei_data) #these two columns are now in the dataframe


###############################################################################

# Fit nest survival models by taking away the intercept and use initial values

# STAGE 1 - fit NEST SURVIVAL models at the nesting level

# Information: From year 1994 to 2026 the earliest julian date is 134 and latest is 209.
# To get the Number of Occasions (NOCC), a.k.a duration of the study, the season length
# will be the difference of the latest and earliest. 209-134=75

# Data from 2022-2025 has earliest date of 142 and oldest as 196 
# NOCC is 55 (really 141 to 196)

# Stage 1 models #

###############################################################################

run.models_1 = function()
{
  # 1. constant daily survival rate model (null)
  S.dot = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.dot",
               model.parameters = list(S=list(formula = ~ 1)))
  
  # 2. year
  S.sy = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.sy", groups = c("Year"),
              model.parameters = list(S=list(formula = ~ Year))) 
  
  # 3. year + init
  S.syi = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.syi", groups = c("Year"),
               model.parameters = list(S=list(formula = ~ Year + Init)),
               initial = S.sy)

  # 4. year + nestage
  S.syn = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.syn", groups = c("Year"),
               model.parameters = list(S=list(formula = ~ Year + NestAge)),
               initial = S.sy)
  
  # 5. year + init + nestage
  S.syin = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.syin", groups = c("Year"),
                model.parameters = list(S=list(formula = ~ Year + Init + NestAge)),
                initial = S.syi)
  
  # # 6. site + year + init + site * init
  # S.syi.sxi = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.syi.sxi", groups = c("Site","Year"),
  #                  model.parameters = list(S=list(formula = ~ Site + Year + Init + Site*Init)),
  #                  initial = S.syi)
  # 
  # # 7. site + year + nestage + site * nestage
  # S.syn.sxn = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.syn.sxn", groups = c("Site","Year"),
  #                  model.parameters = list(S=list(formula = ~ Site + Year + NestAge + Site*NestAge)),
  #                  initial = S.syn)
  # 
  # # 8. site + year + init + nestage + site * init
  # S.syin.sxi = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.syin.sxi", groups = c("Site","Year"),
  #                   model.parameters = list(S=list(formula = ~ Site + Year + Init + NestAge + Site*Init)),
  #                   initial = S.syin)
  # 
  # # 9. site + year + init + nestage + site * nestage
  # S.syin.sxn = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.syin.sxn", groups = c("Site","Year"),
  #                   model.parameters = list(S=list(formula = ~ Site + Year + Init + NestAge + Site*NestAge)),
  #                   initial = S.syin)
  # 
  # # 10. site + year + init + nestage + site * init + site * nestage
  # S.syin.sxi.sxn = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.syina.sxi.sxna", groups = c("Site","Year"),
  #                       model.parameters = list(S=list(formula = ~ Site + Year + Init + NestAge + Site*Init + Site*NestAge)),
  #                       initial = S.syin.sxi)
  # 
  # # 11. site * year
  # S.sxy = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.sxy", groups = c("Site","Year"),
  #              model.parameters = list(S=list(formula = ~ -1 + Site:Year)))
  # 
  # # 12. site * year + init
  # S.sxy.i = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.sxy.i", groups = c("Site","Year"),
  #                model.parameters = list(S=list(formula = ~ -1 + Site:Year + Init)),
  #                initial = S.sxy)
  # 
  # 13. site * year + nestage
  # S.sxy.n = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.sxy.n", groups = c("Site","Year"),
  #                model.parameters = list(S=list(formula = ~ -1 + Site:Year + NestAge)),
  #                initial = S.sxy)
  # 
  # # 14. site * year + site * init
  # S.sxy.sxi = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.sxy.sxi", groups = c("Site","Year"),
  #                  model.parameters = list(S=list(formula = ~ -1 + Site:Year + Site:Init)),
  #                  initial = S.sxy)
  # 
  # # 15. site * year + site * nestage
  # S.sxy.sxn = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.sxy.sxn", groups = c("Site","Year"),
  #                  model.parameters = list(S=list(formula = ~ -1 + Site:Year + Site:NestAge)),
  #                  initial = S.sxy)
  # 
  # # 16. site * year + init + nestge
  # S.sxy.in = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.sxy.in", groups = c("Site","Year"),
  #                 model.parameters = list(S=list(formula = ~ -1 + Site:Year + Init + NestAge)),
  #                 initial = S.sxy.i)
  # 
  # # 17. site * year + init + nestage + site * init
  # S.sxy.ina.sxi = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.sxy.ina.sxi", groups = c("Site","Year"),
  #                      model.parameters = list(S=list(formula = ~ -1 + Site:Year + Init + NestAge + Site:Init)),
  #                      initial = S.sxy.in)
  # 
  # # 18. site * year + init + nestage + site * nestage
  # S.sxy.ina.sxn = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.sxy.ina.sxn", groups = c("Site","Year"),
  #                      model.parameters = list(S=list(formula = ~ -1 + Site:Year + Init + NestAge + Site:NestAge)),
  #                      initial = S.sxy.in)
  # 
  # # 19. site * year + init + nestage + site*init + site*nestage
  # S.sxy.in.sxi.sxn = mark(spei_data, nocc = 55, model = "Nest", model.name = "S.sxy.in.sxi.sxn", groups = c("Site","Year"),
  #                         model.parameters = list(S=list(formula = ~ -1 + Site:Year + Site:Init + Site:NestAge)),
  #                         initial = S.sxy.sxi)
  
  
  
  # Return model table and list of models
  
  return(collect.models())
}

# Fit models
model.results_1=run.models_1()

# Look at model results
model.results_1
output_1 <- model.results_1

# to look at specific model output
# write the model in the console
model.results_1$S.dot #1
model.results_1$S.sy #2
model.results_1$S.syi #3
model.results_1$S.syn #4
model.results_1$S.syin #5
# model.results_1$S.syi.sxi #6
# model.results_1$S.syn.sxn #7
# model.results_1$S.syin.sxi #8
# model.results_1$S.syin.sxn #9
# model.results_1$S.syin.sxi.sxn #10
# model.results_1$S.sxy #11
# model.results_1$S.sxy.i #12
# model.results_1$S.sxy.n #13
# model.results_1$S.sxy.sxi #14
# model.results_1$S.sxy.sxn #15
# model.results_1$S.sxy.in #16 top model w=0.42
# model.results_1$S.sxy.ina.sxi #17
# model.results_1$S.sxy.ina.sxn #18 2nd top model w=0.20
# model.results_1$S.sxy.in.sxi.sxn #19

# Note: all the models ran completely. Every model output was checked to look
# for any errors or off numbers.


# Write the results output into excel (csv)
model_output_1 <- model.results_1$model.table
write.csv(model_output_1, "nest_survival/output/model_ouput_1.csv")

model_output_yn <- model.results_1$S.syn$results$beta
model_output_yin <- model.results_1$S.syin$results$beta

##############################################################################

# Work with stage 1 top model to get 24-day cumulative nest success estimates.
# This is going to create a figure of the annual nest survival estimates for each
# year and site, while accounting for the factors of nest initiation date and
# nestage.

##############################################################################


# Use find.covariates to get dataframe for mean individual covariates
  #look at the top two models within 2 DeltaAICC
top_model_1 <- model.results_1$S.syi
top_model_2 <- model.results_1$S.syin   #only really works for this model, other just has 4 variables
ffc <- find.covariates(top_model_2, spei_data)

# Build a design matrix for nestages 1:24, which is my assumed incubation period until
# success. The following code will assign nest ages 1:24 for nestage for each year/site.
# When doing this, be careful and make sure the year and site are corresponding to on another
# the design matrix. Or else your estimates will be mixed.
# ffc$value[2517:2540] <- 1:24 #1994 
# ffc$value[2591:2614] <- 1:24 #1995 
# ffc$value[2665:2688] <- 1:24 #1996
# ffc$value[2739:2762] <- 1:24 #1997
# ffc$value[2813:2836] <- 1:24 #1998
# ffc$value[2887:2910] <- 1:24 #1999
# ffc$value[2961:2984] <- 1:24 #2000
# ffc$value[3035:3058] <- 1:24 #2001
# ffc$value[3109:3132] <- 1:24 #2002
# ffc$value[3183:3206] <- 1:24 #2003
# ffc$value[3257:3280] <- 1:24 #2004
# ffc$value[3331:3354] <- 1:24 #2005
# ffc$value[3405:3428] <- 1:24 #2006
# ffc$value[3479:3502] <- 1:24 #2007
# ffc$value[3553:3576] <- 1:24 #2008
# ffc$value[3627:3650] <- 1:24 #2009
# ffc$value[3701:3724] <- 1:24 #2010
# ffc$value[3775:3798] <- 1:24 #2010-u the '-u' stands for site "utq"
# ffc$value[3849:3872] <- 1:24 #2011
# ffc$value[3923:3946] <- 1:24 #2011-u
# ffc$value[3997:4020] <- 1:24 #2012
# ffc$value[4071:4094] <- 1:24 #2012-u
# ffc$value[4145:4168] <- 1:24 #2013
# ffc$value[4219:4242] <- 1:24 #2013-u
# ffc$value[4293:4316] <- 1:24 #2014 
# ffc$value[4367:4390] <- 1:24 #2014-u
# ffc$value[4441:4464] <- 1:24 #2015
# ffc$value[4515:4538] <- 1:24 #2015-u
# ffc$value[4589:4612] <- 1:24 #2016-u
# ffc$value[4663:4686] <- 1:24 #2017-u
# ffc$value[4737:4760] <- 1:24 #2018-u
# ffc$value[4811:4834] <- 1:24 #2019
# ffc$value[4885:4908] <- 1:24 #2019-u
# ffc$value[4959:4982] <- 1:24 #2021

ffc$value[4959:4982] <- 1:24 #2022
ffc$value[4959:4982] <- 1:24 #2023
ffc$value[4959:4982] <- 1:24 #2024
ffc$value[4959:4982] <- 1:24 #2025
ffc$value[4959:4982] <- 1:24 #2026

# Assign appropriate initiation date values for each site
# ffc$value[1:74] <- seq(128, 177, length = 74) #k1994   1
# ffc$value[75:148] <- seq(128, 177, length = 74) #k1995   2
# ffc$value[149:222] <- seq(128, 177, length = 74) #k1996   3
# ffc$value[223:296] <- seq(128, 177, length = 74) #k1997   4
# ffc$value[297:370] <- seq(128, 177, length = 74) #k1998   5
# ffc$value[371:444] <- seq(128, 177, length = 74) #k1999   6
# ffc$value[445:518] <- seq(128, 177, length = 74) #k2000   7
# ffc$value[519:592] <- seq(128, 177, length = 74) #k2001   8
# ffc$value[593:666] <- seq(128, 177, length = 74) #k2002   9
# ffc$value[667:740] <- seq(128, 177, length = 74) #k2003   10
# ffc$value[741:814] <- seq(128, 177, length = 74) #k2004   11
# ffc$value[815:888] <- seq(128, 177, length = 74) #k2005   12
# ffc$value[889:962] <- seq(128, 177, length = 74) #k2006   13
# ffc$value[963:1036] <- seq(128, 177, length = 74) #k2007   14
# ffc$value[1037:1110] <- seq(128, 177, length = 74) #k2008   15
# ffc$value[1111:1184] <- seq(128, 177, length = 74) #k2009   16
# ffc$value[1185:1258] <- seq(128, 177, length = 74) #k2010   17
# ffc$value[1259:1332] <- seq(150, 187, length = 74) #u2010   18
# ffc$value[1333:1406] <- seq(128, 177, length = 74) #k2011   19
# ffc$value[1407:1480] <- seq(150, 187, length = 74) #u2011   20
# ffc$value[1481:1554] <- seq(128, 177, length = 74) #k2012   21
# ffc$value[1555:1628] <- seq(150, 187, length = 74) #u2012   22
# ffc$value[1629:1702] <- seq(128, 177, length = 74) #k2013   23
# ffc$value[1703:1776] <- seq(150, 187, length = 74) #u2013   24
# ffc$value[1777:1850] <- seq(128, 177, length = 74) #k2014   25
# ffc$value[1851:1924] <- seq(150, 187, length = 74) #u2014   26
# ffc$value[1925:1998] <- seq(128, 177, length = 74) #k2015   27
# ffc$value[1999:2072] <- seq(150, 187, length = 74) #u2015   28
# ffc$value[2073:2146] <- seq(150, 187, length = 74) #u2016   29
# ffc$value[2147:2220] <- seq(150, 187, length = 74) #u2017   30
# ffc$value[2221:2294] <- seq(150, 187, length = 74) #u2018   31
# ffc$value[2295:2368] <- seq(128, 177, length = 74) #k2019   32
# ffc$value[2369:2442] <- seq(150, 187, length = 74) #u2019   33
# ffc$value[2443:2516] <- seq(128, 177, length = 74) #k2021   34

ffc$value[2443:2516] <- seq(128, 177, length = 74) #k2022   35
ffc$value[2443:2516] <- seq(128, 177, length = 74) #k2023   36
ffc$value[2443:2516] <- seq(128, 177, length = 74) #k2024   37
ffc$value[2443:2516] <- seq(128, 177, length = 74) #k2025   38
ffc$value[2443:2516] <- seq(128, 177, length = 74) #k2026   39


# Fill the design matrix with values of interest that we just did above.
fdesign <- fill.covariates(top_model_1, ffc)

# Extract the first 24 nestages for each from design matrix
ffull.survival <- compute.real(top_model_1, design = fdesign, vcv = TRUE) #vcv = TRUE, returns vcv instead of se
freal <- ffull.survival$real
vcv <- ffull.survival$vcv.real

# DELTA-METHOD
# The function deltamethod.special computes delta-method standard errors.
# After you run the function, it will return the standard errors.
# To get the variance, you square that value.
# The codes below will now compute to get the standard eroror.
s1994 <- deltamethod.special("prod",ffull.survival$real[1:24],ffull.survival$vcv.real[1:24,1:24]) #1
s1995 <- deltamethod.special("prod",ffull.survival$real[75:98],ffull.survival$vcv.real[75:98,75:98]) #2                                                                                
s1996 <- deltamethod.special("prod",ffull.survival$real[149:172],ffull.survival$vcv.real[149:172,149:172]) #3
s1997 <- deltamethod.special("prod",ffull.survival$real[223:246],ffull.survival$vcv.real[223:246,223:246]) #4
s1998 <- deltamethod.special("prod",ffull.survival$real[297:320],ffull.survival$vcv.real[297:320,297:320]) #5
s1999 <- deltamethod.special("prod",ffull.survival$real[371:394],ffull.survival$vcv.real[371:394,371:394]) #6
s2000 <- deltamethod.special("prod",ffull.survival$real[445:468],ffull.survival$vcv.real[445:468,445:468]) #7
s2001 <- deltamethod.special("prod",ffull.survival$real[519:542],ffull.survival$vcv.real[519:542,519:542]) #8
s2002 <- deltamethod.special("prod",ffull.survival$real[593:616],ffull.survival$vcv.real[593:616,593:616]) #9
s2003 <- deltamethod.special("prod",ffull.survival$real[667:690],ffull.survival$vcv.real[667:690,667:690]) #10
s2004 <- deltamethod.special("prod",ffull.survival$real[741:764],ffull.survival$vcv.real[741:764,741:764]) #11
s2005 <- deltamethod.special("prod",ffull.survival$real[815:838],ffull.survival$vcv.real[815:838,815:838]) #12
s2006 <- deltamethod.special("prod",ffull.survival$real[889:912],ffull.survival$vcv.real[889:912,889:912]) #13
s2007 <- deltamethod.special("prod",ffull.survival$real[963:986],ffull.survival$vcv.real[963:986,963:986]) #14
s2008 <- deltamethod.special("prod",ffull.survival$real[1037:1060],ffull.survival$vcv.real[1037:1060,1037:1060]) #15
s2009 <- deltamethod.special("prod",ffull.survival$real[1111:1134],ffull.survival$vcv.real[1111:1134,1111:1134]) #16
s2010 <- deltamethod.special("prod",ffull.survival$real[1185:1208],ffull.survival$vcv.real[1185:1208,1185:1208]) #17
s2010u <- deltamethod.special("prod",ffull.survival$real[1259:1282],ffull.survival$vcv.real[1259:1282,1259:1282]) #18
s2011 <- deltamethod.special("prod",ffull.survival$real[1333:1356],ffull.survival$vcv.real[1333:1356,1333:1356]) #19
s2011u <- deltamethod.special("prod",ffull.survival$real[1407:1430],ffull.survival$vcv.real[1407:1430,1407:1430]) #20
s2012 <- deltamethod.special("prod",ffull.survival$real[1481:1504],ffull.survival$vcv.real[1481:1504,1481:1504]) #21
s2012u <- deltamethod.special("prod",ffull.survival$real[1555:1578],ffull.survival$vcv.real[1555:1578,1555:1578]) #22
s2013 <- deltamethod.special("prod",ffull.survival$real[1629:1652],ffull.survival$vcv.real[1629:1652,1629:1652]) #23
s2013u <- deltamethod.special("prod",ffull.survival$real[1703:1726],ffull.survival$vcv.real[1703:1726,1703:1726]) #24
s2014 <- deltamethod.special("prod",ffull.survival$real[1777:1800],ffull.survival$vcv.real[1777:1800,1777:1800]) #25
s2014u <- deltamethod.special("prod",ffull.survival$real[1851:1874],ffull.survival$vcv.real[1851:1874,1851:1874]) #26
s2015 <- deltamethod.special("prod",ffull.survival$real[1925:1948],ffull.survival$vcv.real[1925:1948,1925:1948]) #27
s2015u <- deltamethod.special("prod",ffull.survival$real[1999:2022],ffull.survival$vcv.real[1999:2022,1999:2022]) #28
s2016u <- deltamethod.special("prod",ffull.survival$real[2073:2096],ffull.survival$vcv.real[2073:2096,2073:2096]) #29
s2017u <- deltamethod.special("prod",ffull.survival$real[2147:2170],ffull.survival$vcv.real[2147:2170,2147:2170]) #30
s2018u <- deltamethod.special("prod",ffull.survival$real[2221:2244],ffull.survival$vcv.real[2221:2244,2221:2244]) #31
s2019 <- deltamethod.special("prod",ffull.survival$real[2295:2318],ffull.survival$vcv.real[2295:2318,2295:2318]) #32
s2019u <- deltamethod.special("prod",ffull.survival$real[2369:2392],ffull.survival$vcv.real[2369:2392,2369:2392]) #33
s2021 <- deltamethod.special("prod",ffull.survival$real[2443:2466],ffull.survival$vcv.real[2443:2466,2443:2466]) #34
#add in mroe recent years' values
s2022 <- deltamethod.special("prod",ffull.survival$real[2443:2466],ffull.survival$vcv.real[2443:2466,2443:2466]) #35
s2023 <- deltamethod.special("prod",ffull.survival$real[2443:2466],ffull.survival$vcv.real[2443:2466,2443:2466]) #36
s2024 <- deltamethod.special("prod",ffull.survival$real[2443:2466],ffull.survival$vcv.real[2443:2466,2443:2466]) #37
s2025 <- deltamethod.special("prod",ffull.survival$real[2443:2466],ffull.survival$vcv.real[2443:2466,2443:2466]) #38
s2026 <- deltamethod.special("prod",ffull.survival$real[2443:2466],ffull.survival$vcv.real[2443:2466,2443:2466]) #39

# Obtain the DSR estimates for nest ages 1:24
survival.24 <- compute.real(top_model_1, design = fdesign)[c(1:24,
                                                             75:98,
                                                             149:172,
                                                             223:246,
                                                             297:320,
                                                             371:394,
                                                             445:468,
                                                             519:542,
                                                             593:616,
                                                             667:690,
                                                             741:764,
                                                             815:838,
                                                             889:912,
                                                             963:986,
                                                             1037:1060,
                                                             1111:1134,
                                                             1185:1208,
                                                             1259:1282,
                                                             1333:1356,
                                                             1407:1430,
                                                             1481:1504,
                                                             1555:1578,
                                                             1629:1652,
                                                             1703:1726,
                                                             1777:1800,
                                                             1851:1874,
                                                             1925:1948,
                                                             1999:2022,
                                                             2073:2096,
                                                             2147:2170,
                                                             2221:2244,
                                                             2295:2318,
                                                             2369:2392,
                                                             2443:2466), ]


# Get the product of the 24 nestage DSR estimates to achieve cumulative nest success probability.
# There should be 34 estimates total. 24 for Kigigak Island and 10 for Utqiagvik.
product1994 <- prod(survival.24$estimate[1:24]) #1
product1995 <- prod(survival.24$estimate[25:48]) #2
product1996 <- prod(survival.24$estimate[49:72]) #3
product1997 <- prod(survival.24$estimate[73:96]) #4
product1998 <- prod(survival.24$estimate[97:120]) #5
product1999 <- prod(survival.24$estimate[121:144]) #6
product2000 <- prod(survival.24$estimate[145:168]) #7
product2001 <- prod(survival.24$estimate[169:192]) #8
product2002 <- prod(survival.24$estimate[193:216]) #9
product2003 <- prod(survival.24$estimate[217:240]) #10
product2004 <- prod(survival.24$estimate[241:264]) #11
product2005 <- prod(survival.24$estimate[265:288]) #12
product2006 <- prod(survival.24$estimate[289:312]) #13
product2007 <- prod(survival.24$estimate[313:336]) #14
product2008 <- prod(survival.24$estimate[337:360]) #15
product2009 <- prod(survival.24$estimate[361:384]) #16
product2010 <- prod(survival.24$estimate[385:408]) #17
product2010u <- prod(survival.24$estimate[409:432]) #18
product2011 <- prod(survival.24$estimate[433:456]) #19
product2011u <- prod(survival.24$estimate[457:480]) #20
product2012 <- prod(survival.24$estimate[481:504]) #21
product2012u <- prod(survival.24$estimate[505:528]) #22
product2013 <- prod(survival.24$estimate[529:552]) #23
product2013u <- prod(survival.24$estimate[553:576]) #24
product2014 <- prod(survival.24$estimate[577:600]) #25
product2014u <- prod(survival.24$estimate[601:624]) #26
product2015 <- prod(survival.24$estimate[625:648]) #27
product2015u <- prod(survival.24$estimate[649:672]) #28
product2016u <- prod(survival.24$estimate[673:696]) #29
product2017u <- prod(survival.24$estimate[697:720]) #30
product2018u <- prod(survival.24$estimate[721:744]) #31
product2019 <- prod(survival.24$estimate[745:768]) #32
product2019u <- prod(survival.24$estimate[769:792]) #33
product2021 <- prod(survival.24$estimate[793:816]) #34
#add in more recent years' values
product2022 <- prod(survival.24$estimate[793:816]) #35
product2023 <- prod(survival.24$estimate[793:816]) #36
product2024 <- prod(survival.24$estimate[793:816]) #37
product2025 <- prod(survival.24$estimate[793:816]) #38
product2026 <- prod(survival.24$estimate[793:816]) #39

# Create a dataframe from the products of the 24 nestage estimates above.
probsuccess <- c(product1994,product1995,product1996,product1997,product1998,product1999,product2000,
                 product2001,product2002,product2003,product2004,product2005,product2006,product2007,
                 product2008,product2009,product2010,product2011,product2012,product2013,product2014,
                 product2015,product2019,product2021,product2010u,product2011u,product2012u,product2013u,
                 product2014u,product2015u,product2016u,product2017u,product2018u,product2019u,
                 product2022,product2023,product2024,product2025,product2026)
probsuccess <- as.data.frame(probsuccess)

# Create a dataframe for the standard errors for year and site.
se <- c(s1994,s1995,s1996,s1997,s1998,s1999,s2000,s2001,
        s2002,s2003,s2004,s2005,s2006,s2007,s2008,s2009,s2010,
        s2011,s2012,s2013,s2014,s2015,s2019,s2021,s2010u,s2011u,
        s2012u,s2013u,s2014u,s2015u,s2016u,s2017u,s2018u,s2019u,
        s2022,s2023,s2024,s2025,s2026)
se <- as.data.frame(se)

# Create dataframe for the 95% confidence intervals
lci <- probsuccess - 1.96*(se)
lci <- as.data.frame(lci)
colnames(lci)[colnames(lci) == "probsuccess"] <- "lci"

uci <- probsuccess + 1.96*(se)
uci <- as.data.frame(uci)
colnames(uci)[colnames(uci) == "probsuccess"] <- "uci"

# Combine 'probsuccess' and 'lci' and 'uci'.
data <- cbind(probsuccess,lci,uci)
#data$lci <- data$probsuccess - 1.96*(se)
#data$uci <- data$probsuccess + 1.96*(se)


# Create a figure of the annual nest survival estimates for both sites.

# Add site and year to dataframe we just created.
data$Site <- c("Kigigak Island","Kigigak Island","Kigigak Island","Kigigak Island","Kigigak Island","Kigigak Island",
               "Kigigak Island","Kigigak Island","Kigigak Island","Kigigak Island","Kigigak Island","Kigigak Island",
               "Kigigak Island","Kigigak Island","Kigigak Island","Kigigak Island","Kigigak Island","Kigigak Island",
               "Kigigak Island","Kigigak Island","Kigigak Island","Kigigak Island","Kigigak Island","Kigigak Island",
               "Utqiaġvik","Utqiaġvik","Utqiaġvik","Utqiaġvik","Utqiaġvik",
               "Utqiaġvik","Utqiaġvik","Utqiaġvik","Utqiaġvik","Utqiaġvik", 
               "Kigigak Island","Kigigak Island","Kigigak Island","Kigigak Island","Kigigak Island")

data$year <- c("1994","1995","1996","1997","1998","1999",
               "2000","2001","2002","2003","2004","2005",
               "2006","2007","2008","2009","2010","2011",
               "2012","2013","2014","2015","2019","2021",
               "2010","2011","2012","2013","2014",
               "2015","2016","2017","2018","2019",
               "2022","2023","2024","2025","2026")

data$probsuccess<-as.numeric(data$probsuccess) #in case it is not recognized as numerical values

# save new data frame as csv to not need to run model set 1 again
write.csv(data, "nest_survival/output/annual_ns_estimates.csv")
annual_ns_estimates <- read.csv("nest_survival/output/annual_ns_estimates.csv", header = TRUE)
summary(annual_ns_estimates)
annual_ns_estimates$Site <- as.factor(annual_ns_estimates$Site)
annual_ns_estimates$year <- as.factor(annual_ns_estimates$year)


# Plot
ggplot(data, aes(x = year, y = probsuccess, color = Site)) +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.13) +
  geom_errorbar(aes(ymin = lci, ymax = uci, width = 0.4)) +
  geom_point() +
  geom_hline(yintercept = mean(data$probsuccess[1:24]), color = "red", lty = "dashed") +
  geom_hline(yintercept = mean(data$probsuccess[25:34]), color = "blue", lty = "dashed") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.75, 0.3)) +
  xlab("Year") + ylab("Estimated Nest Survival") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  theme(axis.title.y = element_text(
    size = 15
  )) +
  theme(axis.title.x = element_text(
    size = 15
  )) +
  theme(legend.position = "bottom")
ggsave("nest_survival/output/nest_success_prob_vs_year.jpg",width = 7, height = 5, dpi = 600)

# Plot 2
ggplot(annual_ns_estimates, aes(x = year, y = probsuccess, color = Site)) +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.13) +
  geom_errorbar(aes(ymin = lci, ymax = uci, width = 0.4),
                position = position_dodge(0.5)) +
  geom_point(position = position_dodge(0.5)) +
  geom_hline(yintercept = mean(data$probsuccess[1:24]), color = "red", lty = "dashed") +
  geom_hline(yintercept = mean(data$probsuccess[25:34]), color = "blue", lty = "dashed") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.75, 0.3)) +
  xlab("Year") + ylab("Estimated Nest Survival") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  theme(axis.title.y = element_text(
    size = 15
  )) +
  theme(axis.title.x = element_text(
    size = 15
  )) +
  theme(legend.position = "bottom")
ggsave("nest_survival/output/nest_success_prob_vs_year_dodge.jpg",width = 7, height = 5, dpi = 600)

# Get simple statistics
# Average nest success 
std.error(data$probsuccess[1:24]) #0.03950135
std.error(data$probsuccess[25:34]) #0.04592819 
k.m.uci <- 0.788046467291436 + 1.96*0.03950135
k.m.lci <- 0.788046467291436 - 1.96*0.03950135
u.m.uci <- 0.67859130986094 + 1.96*0.04592819
u.m.lci <- 0.67859130986094 - 1.96*0.04592819




################################################################################

# STAGE 2 - fit NEST SURVIVAL models accounting for environmental variables
# including supported additive variables from stage 1 (Init and NestAge).
# NOCC will remain the same (75).

# Stage 2 models #

################################################################################

run.models_2 = function()
{
  # 1. constant 
  S.dot = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.dot",
               model.parameters = list(S=list(formula = ~ 1)))
  
  # 2. sxt + sxw + winhi + init + nestage + site (Peterson and Douglas 2004, nest plot index)
  S.stswwhins = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.stswwhins", groups = "Site",
                     model.parameters = list(S=list(formula = ~ sxt + sxw + Win_Hi + Init + NestAge + Site)))
  
  # 3. sxt + sxw + winhi + winhi2 + init + nestage + site (peterson and douglas 20014, nest plot index but with squared term)
  S.stswwhwh2ins = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.stswwhwh2ins", groups = "Site",
                        model.parameters = list(S=list(formula = ~ sxt + sxw + Win_Hi + Win_Hi2 + Init + NestAge + Site)))
  
  # 4. ice.severity.index + init + nestage + site (Paul Flint 2016, apparent female survival)
  S.wsiins = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.wsiins", groups = "Site",
                  model.parameters = list(S=list(formula = ~ wsi + Init + NestAge + Site)))
  
  # 5. winhi + winhi2 + init + nestage + site (Christie et al 2018 and Dunham et al 2021, apparent female survival)
  S.whwh2ins = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.whwh2ins", groups = "Site",
                    model.parameters = list(S=list(formula = ~ Win_Hi + Win_Hi2 + Init + NestAge + Site)))
  
  # 6. winhi + init + nestage + site  (Flint et al 2016)
  S.whins = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.whins", groups = "Site",
                 model.parameters = list(S=list(formula = ~ Win_Hi + Init + NestAge + Site)))
  # Return model table and list of models
  
  return(collect.models())
}

# Fit models
model.results_2=run.models_2()

# Look at model results
model.results_2



##to look at specific model output
#write the model in the console
model.results_2$S.dot
model.results_2$S.stswwhins # 2nd top model w=.1
model.results_2$S.stswwhwh2ins # top model w=0.899
model.results_2$S.wsiins
model.results_2$S.whwh2ins
model.results_2$S.whins


# Write the results output into excel (csv)
model_output_2 <- model.results_2$model.table
write.csv(model_output_2, "nest_survival/output/model_output_2.csv")




###############################################################################

# Work with stage 2 top model to obtain DSR estimates that will be used to plot
# DSR as function explanatory variables from the stage 2 top model.

###############################################################################




# Add top model to the environment with simple name
topmodel_2 <- model.results_2$S.stswwhwh2ins


# Look at data statistics summary to see what the ice values are.
summary(spei_data)
### Win_Hi: min=0   max=79
### Win_Hi2: min=0   max=6241
### sxt: min=0   max=7
### sxw: min=0   max=5



###############################################################################
# Plot the quadratic effect of high ice conditions (including mean variables)
w2fc <- find.covariates(topmodel_2, spei_data)

# Assign appropriate nest ages
#w2fc$value[c(741:764, 815:838)] <- 1:24 #wont include, parameter estimates for CI overlap 0

# Assign high ice days range (0:79) to winter days
w2fc$value[c(297:370, 371:444)] <- seq(0, 79, length = 74)

# Create a vector of 0:79 and then create another vector
# of those squared. (both 74 values)
highdays <- seq(0, 79, length = 74)
highdays2 <- highdays^2

# Assign high ice days range (0:79) to winter days
w2fc$value[c(445:518, 519:592)] <- highdays2

# Assign spring temp variables
w2fc$value[c(1:74, 75:148)] <- seq(0, 7, length = 74)

# Assign spring wind variables
w2fc$value[c(149:222, 223:296)] <- seq(0, 5, length = 74)

# Create a design matrix with the values you just changed above
w2design <- fill.covariates(topmodel_2, w2fc)

# Obtain real estimates
w2.survival <- compute.real(topmodel_2, design = w2design)

# Insert number of high ice days
w2.survival$high_ice_days[c(1:74, 75:148)] <- seq(0, 79, length = 74)

# Add site for group variable
w2.survival$Site[1:74] <- ("Kigigak Island")
w2.survival$Site[75:148] <- ("Utqiaġvik")

# Change column names
colnames(w2.survival) <- c("dsr", "se", "lci", "uci","fixed", "Win_Hi2", "Site")

# Make site a factor variable
is.factor(w2.survival$Site)
w2.survival$Site<-as.factor(w2.survival$Site)
is.factor(w2.survival$Site) #now a factor variable


# Graph the best supported model for high ice days

# Plot
ggplot(w2.survival, aes(x = Win_Hi2, y = dsr, color = Site, fill = Site)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.13) +
  geom_line() +
  scale_color_manual(values = c("red","blue")) +
  xlab("Winter Ice Days") + ylab("Daily Nest Survival Probability") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(legend.position = "bottom")
ggsave("nest_survival/output/high_ice2_all.jpg",width = 7, height = 5, dpi = 600)



# Give nest survival estimates for the zero ice days (min), max dsr (moderate, 62 days), and 79 ice days (max ice days)
cum_w2.survival <- w2.survival
cum_w2.survival$cum24 <- w2.survival$dsr^24
cum_w2.survival$cum24lci <- w2.survival$lci^24
cum_w2.survival$cum24uci <- w2.survival$uci^24
### true nest survival @ 0 days
### utq: 0.6445964 (0.4768480, 0.7712619)
### kig: 0.4100728 (0.2601819, 0.5552637)

### true nest survival at max dsr (62 days)
### utq: 0.8155944 (0.6654779, 0.9032013)
### kig: 0.6597629 (0.4811497, 0.7899871)

### true nest survival at max ice days (79)
### utq: 0.8058119 (0.5891536, 0.9159740)
### kig: 0.6437923 (0.3777375, 0.8203782)


###############################################################################
# Plot spring extreme temp
# Summary
### sxt: min=0 max=7
### sxw: min=0 max=5

spfc <- find.covariates(topmodel_2, spei_data)
#spfc$value[c(741:764, 815:838)] <- 1:24

# Assign sxt values 0:7
spfc$value[c(1:74, 75:148)] <- seq(0, 7, length = 74)

# Create a design matrix with the values you just changed above
spdesign <- fill.covariates(topmodel_2, spfc)

# Obtain real estimates
sp.survival <- compute.real(topmodel_2, design = spdesign)

# Insert number spring extremes days
sp.survival$spring_extreme[c(1:74, 75:148)] <- seq(0, 7, length = 74)

# Add site for group variable
sp.survival$Site[1:74] <- ("Kigigak Island")
sp.survival$Site[75:148] <- ("Utqiaġvik")

# Change column names
colnames(sp.survival) <- c("dsr", "se", "lci", "uci","fixed", "spring_extreme_temp", "Site")

# Make site a factor variable
is.factor(sp.survival$Site)
sp.survival$Site<-as.factor(sp.survival$Site)
is.factor(sp.survival$Site) #now a factor variable


# Plot
ggplot(sp.survival, aes(x = spring_extreme_temp, y = dsr, color = Site, fill = Site)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.13) +
  geom_line() +
  scale_color_manual(values = c("red","blue")) +
  xlab("Spring Extreme Temperature Days") + ylab("Daily Nest Survival Probability") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(legend.position = "bottom")
ggsave("nest_survival/output/spring_extreme_temp.jpg",width = 7, height = 5, dpi = 600)

# Give nest survival estimates from the min and max dsr value
cum24_st <- sp.survival
cum24_st$dsr24 <- cum24_st$dsr^24
cum24_st$lci24 <- cum24_st$lci^24
cum24_st$uci24 <- cum24_st$uci^24
### true nest survival at 0 extreme temperature days in April
### utq: 0.8552867 (0.7777957, 0.907404)
### kig: 0.7266923 (0.6634702,0.7801093)

### true nest survival at 7 (max) temperature days in April
### utq: 0.7893091 (0.5854893, 0.9010368)
### kig: 0.6173185 (0.3701762, 0.7922688)


###############################################################################
# Spring extreme wind
spwfc <- find.covariates(topmodel_2, spei_data)

# Assign sxw values 0:5
spwfc$value[c(149:222, 223:296)] <- seq(0, 5, length = 74)

# Create a design matrix with the values you just changed above
spwdesign <- fill.covariates(topmodel_2, spwfc)

# Obtain real estimates
spw.survival <- compute.real(topmodel_2, design = spwdesign)

# Insert number spring extremes
spw.survival$spring_extreme[c(1:74, 75:148)] <- seq(0, 5, length = 74)

# Add site for group variable
spw.survival$Site[1:74] <- ("Kigigak Island")
spw.survival$Site[75:148] <- ("Utqiaġvik")

# Change column names
colnames(spw.survival) <- c("dsr", "se", "lci", "uci","fixed", "spring_extreme_wind", "Site")

# Make site a factor variable
is.factor(spw.survival$Site)
spw.survival$Site<-as.factor(spw.survival$Site)
is.factor(spw.survival$Site) #now a factor variable


# Plot
ggplot(spw.survival, aes(x = spring_extreme_wind, y = dsr, color = Site, fill = Site)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.13) +
  geom_line() +
  scale_color_manual(values = c("red","blue")) +
  xlab("Spring Extreme Wind Days") + ylab("Daily Nest Survival Probability") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(legend.position = "bottom")
ggsave("nest_survival/output/spring_extreme_wind.jpg",width = 7, height = 5, dpi = 600)

# Give nest survival estimates for the min and max wind values
cum24_sw <- spw.survival
cum24_sw$dsr24 <- cum24_sw$dsr^24
cum24_sw$lci24 <- cum24_sw$lci^24
cum24_sw$uci24 <- cum24_sw$uci^24
### true nest survival at 0 extreme wind days in April
### utq: 0.8624892 (0.7851417, 0.9135639)
### kig: 0.7392048 (0.6727289, 0.7943423)

### true nest survival at 5 (max) temperature days in April
### utq: 0.7947505 (0.5929911, 0.9042551)
### kig: 0.6259828 (0.3756097, 0.8002388)



###############################################################################
# Nest Initiation Date

# Create a new dataframe of the nest initiation date values prior to making a graph
ifc <- find.covariates(topmodel_2, spei_data)

# Assign nest initiation date range
### kig init min = 128
### kig init max = 177
### utq init min = 150
### utq init max = 187
ifc$value[593:666] <- seq(128, 177, length = 74)
ifc$value[667:740] <- seq(150, 187, length = 74)


# Create a design matrix with the values you just changed above
idesign <- fill.covariates(topmodel_2, ifc)

# Obtain real estimates
init.survival <- compute.real(topmodel_2, design = idesign)

# Insert nest initiation date ranges
init.survival$init[1:74] <- seq(128, 177, length = 74)
init.survival$init[75:148] <- seq(150, 187, length = 74)

# Add site for group variable
init.survival$site[1:74] <- ("Kigigak Island")
init.survival$site[75:148] <- ("Utqiaġvik")

# Change column names
colnames(init.survival) <- c("dsr", "se", "lci", "uci","fixed", "init", "Site")

# Make site a factor variable
is.factor(init.survival$Site)
init.survival$Site<-as.factor(init.survival$Site)
is.factor(init.survival$Site) #now a factor variable


# Plot
ggplot(init.survival, aes(x = as.Date(init, origin = as.Date("1994-01-01")), y = dsr, color = Site, fill = Site)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.13) +
  geom_line() +
  scale_color_manual(values = c("red","blue")) +
  xlab("Nest Initiation Date") + ylab("Daily Nest Survival Probability") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.title.y = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(legend.position = "bottom")
ggsave("nest_survival/output/nest_initiation_date.jpg",width = 7, height = 5, dpi = 600)

# Give nest survival estimates for the min and max initiation date values
cum24_i <- init.survival
cum24_i$dsr24 <- cum24_i$dsr^24
cum24_i$lci24 <- cum24_i$lci^24
cum24_i$uci24 <- cum24_i$uci^24
### true nest survival at earliest nest initiation date
### utq 150: 0.8355991 (0.75047281, 0.8937901)
### kig 128: 0.8735808 (0.81574741, 0.9142478)

### true nest survival at latest nest initiation date
### utq 187: 0.4366802 (0.14132988, 0.7074405)
### kig 177: 0.3426890 (0.08590462, 0.6314921)







################################################################################
################################################################################
### This is important! You dont want to have a messy directory of Program MARK



# If you want to clean up the mark*.inp, .vcv, .res and .out
#  and .tmp files created by RMark in the working directory,
#  execute 'rm(list = ls(all = TRUE))' - see 2 lines below.
# NOTE: this will delete all objects in the R session.
rm(list = ls(all=TRUE))
# Then, execute 'cleanup(ask = FALSE)' to delete orphaned output
#  files from MARK. Execute '?cleanup' to learn more
cleanup(ask = FALSE)
