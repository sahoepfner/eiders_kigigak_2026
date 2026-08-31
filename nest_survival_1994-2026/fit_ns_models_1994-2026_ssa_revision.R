#Summary of Reproductive Output of Spectacled Eiders at Kigigak Island, 1994-2026
#code and analysis by Sarah Hoepfner August 2026
  #for nest survival analyses most of the code is modified from Randall Friendly's code in Friendly et al. (2025)
    #first modeled 2019-2026, and then adjusted to include all years 1994-2026 to model year+init+nestage
    #and only use 2022-2025 for the SSA review
  #end of the script has summary stat code for clutch sizes and membranes found in the nest 2022-2026

# Load packages to run NS models
library(RMark) #to run Program MARK
library(tidyverse) #helps to handle and transform data
library(data.table) #for data manipulation and visuals
library(readr) #read in excel files that are not .csv
library(msm) #to use delta method to estimate variance components
library(ggplot2) #create figures using ggplot
library(plotrix) #compute simple statistics
library(RODBC) #read in files from Access
library(dplyr) #data management and piping functions
options(scipen = 999)  #show more decimal digits

#data manipulation in R through Access and not making edits only in Excel
  #or skip to line 155 and load in the cleaned file
#start by prepping the more recent nest data that is in Access
db <- "C:/Users/shoepfner/Desktop/nest survival/db_kig_nesting_waterfowl_2019-2026_appended_20260821.accdb"
con_db<-odbcConnectAccess2007(db)
sqlTables(con_db,tableType="TABLE")$TABLE_NAME
nest <- sqlFetch(con_db, "tbl_Nest")
hatch <- sqlFetch(con_db, "qry_estHatch")

nests <- nest
nests$Year <- as.numeric(format(as.Date(nests$dt_found, format="%m/%d/%Y"), "%Y"))
nests$Year <- as.factor(nests$Year)
#remove a COEI nest
nests <- nests %>% filter(cat_species == "SPEI")

#remove unneeded columns
names(nests)
nests <- nests %>% select(-id_site, -is_onPlot, - id_plot, -cat_species, -cat_nestSite, -cat_plasticBandStatus,
                          -id_plasticBand, -cat_plasticColor, -cat_resightMethod, -is_nasalDisc, -id_nasalDisc,
                          -val_lon_nest, -val_lat_nest, -is_flagged, -cat_hatchDate, -cat_trapStatus, -is_featherCollected,
                          -n_feathersCollected, -is_flagCollected)
#change names to match RMark format
nests <- nests %>% rename(Found = dt_found, LastPres = dt_lastALive, LastCheck = dt_lastCheck, Fate = cat_fate)

#condense fates to hatch=0, failed=1
nests %>%
  distinct(Fate) %>%   # Keep only unique names
  pull(Fate)    
nests <- nests %>%
  mutate(Fate = case_when(
    Fate == "eggs_missing" ~ "1",
    Fate == "eggs_destroyed" ~ "1",
    Fate == "eggs_inviable" ~ "1",
    Fate == "eggs_abandoned" ~ "1",
    Fate == "active_lastcheck" ~ "0",
    Fate == "active" ~ "0",
    Fate == "hatched" ~ "0",
    TRUE ~ Fate )) #keep other values as they are 
#remove nests that were not monitored
nests <- nests %>% filter(!Fate == "not_monitored")
str(nests)

#convert nesting dates to Julian dates
nests_julian <- nests
nests_julian$Found <- as.POSIXlt(nests$Found)$yday
nests_julian$LastPres <- as.POSIXlt(nests$LastPres)$yday
nests_julian$LastCheck <- as.POSIXlt(nests$LastCheck)$yday
nests_julian$dt_Hatch <- as.POSIXlt(nests$dt_Hatch)$yday
str(nests_julian)

#merge in the hatch date files=
hatched <- nests_julian
#merge in qry_estHatch table to get estimated hatch dates from latest float
hatched <- merge(hatched, hatch, by = "id_nest", all = TRUE)  #gives 20 more nests than there should be...
#clean up a little
hatched <- hatched %>% select(-cat_species, -cat_plasticBandStatus)
hatched$estHatch <- as.POSIXlt(hatched$estHatch)$yday

#make some corrections for errors in the Access file
hatched$Fate[hatched$id_nest == "LLK201"] <- 0
hatched$LastPres[hatched$id_nest == "AFM209"] <- 167
hatched$LastCheck[hatched$id_nest == "MAM043"] <- 177
hatched$LastCheck[hatched$id_nest == "LLK201"] <- 174
hatched$LastCheck[hatched$id_nest == "AFM011"] <- 174
hatched$LastCheck[hatched$id_nest == "MAM045"] <- 177
hatched$LastCheck[hatched$id_nest == "RJF038"] <- 161  #inviable nest incubated <24 days so last alive and last checked should be different days
hatched$LastCheck[hatched$id_nest == "RJF250"] <- 168      #inviable nest but female incubated for full incubation period
hatched <- subset(hatched, !(id_nest %in% c("AGB060", "JMT247", "JMT249")))  #has minimal data for nest entries?
hatched <- hatched %>% filter(!(id_nest %in% c("AFM016", "AFM017", "LAH001", "LAH004", "LLK002", "TDM003", "TDM004")))  #nests found hatching or not monitored

hatched <- hatched %>% mutate(
  LastPres = if_else(Fate == "0", LastCheck, LastPres))   #some dt_Hatch are missing and wrong
hatched <- hatched %>% mutate(
  dt_Hatch = if_else(Fate == "0", LastCheck, 0))   #some dt_Hatch are missing and wrong


#fill in hatch dates, first from known hatched nests
hatched <- hatched %>% mutate(
  Hatch_Date = if_else(Fate == "0", dt_Hatch, #first pull hatch date from known hatched nests
                       estHatch))   #rest of column is filled in from float/candle estimated hatch date

#fix weird estimated hatch dates
hatched$Hatch_Date[hatched$id_nest == "AFM011"] <- 174
hatched$Hatch_Date[hatched$id_yrNest == "19DJR001"] <- 166
hatched$Hatch_Date[hatched$id_nest == "AFM015"] <- 178
hatched$Hatch_Date[hatched$id_nest == "LLK201"] <- 174
hatched$Hatch_Date[hatched$id_yrNest == "2019DJR001"] <- 168
hatched$Hatch_Date[hatched$id_nest == "JMT308"] <- 169
hatched$Hatch_Date[hatched$id_nest == "JMT258"] <- 169
hatched$Hatch_Date[hatched$id_nest == "TCD019"] <- 169
  
#add in initiation date by subtracting 24 days from hatch date
str(hatched)
hatched <- hatched %>%
  mutate(init = Hatch_Date - 24)

#fix wrong initiation dates due to weird or missing estimations
hatched$init[hatched$id_nest == "LLK201"] <- 149
hatched$init[hatched$id_nest == "AFM011"] <- 149
hatched$init[hatched$id_nest == "AGB048"] <- 149
hatched$init[hatched$id_nest == "AGB053"] <- 144
hatched$init[hatched$id_nest == "JAM003"] <- 152
hatched$init[hatched$id_nest == "JCA308"] <- 154
hatched$init[hatched$id_nest == "LLK001"] <- 145
hatched$init[hatched$id_nest == "LRB201"] <- 149
hatched$init[hatched$id_nest == "LRB202"] <- 149
hatched$init[hatched$id_nest == "LRB203"] <- 152
hatched$init[hatched$id_nest == "RJF630"] <- 155
hatched$init[hatched$id_nest == "TCD021"] <- 150
hatched$init[hatched$id_nest == "TCD029"] <- 157

#DJR nests have repeats, remove them
hatched <- hatched %>% distinct(id_yrNest, .keep_all = TRUE)     #DJR001 has nests in 2019, 2023, and 2024

INP_2019_2026 <- hatched
write.csv(INP_2019_2026, "C:/Users/shoepfner/Desktop/nest survival/nest_survival_2022-2025/INP_2019-2026.csv")



##at this step now must combine with Randall's INP file for 1994-2021
  #only use 2022-2026 from the above work - use Randall's work for 2019 and 2022
nest_inp_RJF <- read_csv("C:/Users/shoepfner/Desktop/nest survival/Randall's work/INP_20220921.csv")
INP_2019_2026 <- read_csv("C:/Users/shoepfner/Desktop/nest survival/nest_survival_2022-2025/INP_2019-2026.csv")
names(nest_inp_RJF)
names(INP_2019_2026)
str(nest_inp_RJF)
str(INP_2019_2026)

nest_inp_RJF <- nest_inp_RJF %>% filter(Site=="kig") %>% 
  mutate(id_yrNest = paste(Year, Nest, sep = "")) %>%
  select(-Win_Hi, -Win_Lo, -Spr_Hi, -Spr_Lo, -wxt, -sxt, -wxw, - sxw, -wsi, -wi, -Nest, -Site) %>%
  rename(init=Init)
nest_inp_RJF$Year <- as.numeric(nest_inp_RJF$Year)

INP_2019_2026 <- INP_2019_2026 %>% 
  select(-ID_tblNest, -id_nest, -dt_Hatch, -notes_nest, -dt_visit, -cat_nestStatus, -maxAge, - estHatch, -daysToHatch, -Hatch_Date, -"...1") %>%
  rename(FirstFound=Found, LastPresent=LastPres, LastChecked=LastCheck) %>% 
  mutate(Freq = 1)
INP_2019_2026$FirstFound <- as.numeric(INP_2019_2026$FirstFound)
INP_2019_2026$Fate <- as.numeric(INP_2019_2026$Fate)

#From year 1994 to 2021 the earliest julian date is 134 and latest is 209. Don't change Randall's input file!!
#from 2019-2026 the earliest day was 137, so set the more recent years to 134 as the start
  #subtract 133 from all dates (not Init yet though)
INP_2019_2026 <- INP_2019_2026 %>%
  mutate(across(c(FirstFound, LastPresent, LastChecked), ~ . - 133))

#add column for AgeDay1 = age of the nest on study day 1, not the age of the nest when first found
  #day 1 (133) and subtract initiation date 
INP_2019_2026 <- INP_2019_2026 %>%
  mutate(AgeDay1 = 133 - init)


#both INP files have the years 2019 and 2021 so remove these nests from my file from the more recent years
INP_2019_2026 <- INP_2019_2026 %>% filter(!(Year == 2019 & 2021))


inp <- bind_rows(nest_inp_RJF, INP_2019_2026)
names(inp)





##CENTER initiation date
  #centering Init and NestAge (subtract the mean) before modeling, mean per year
    #doesn't change inference but helps convergence and keeps the intercept interpretable 
      #DSR at the mean date/age rather than at Init=0, which is meaningless here
init_c <- inp %>%
  group_by(Year) %>%
  summarise(Init_year = mean(init, na.rm = TRUE)) 
init_c

inp <- inp %>%
  left_join(init_c, by = "Year")

inp <- inp %>% 
  mutate(Init_center = Init_year-init)


#save the final INP file so don't have to always run the above code
write.csv(inp, "C:/Users/shoepfner/Desktop/nest survival/nest_survival_2022-2025/inp_ns_models_1994-2026_ssa_revision.csv")




####start modeling####
# Add path to MARK.exe
Sys.setenv(MarkPath = "C:/Program Files (x86)/MARK")
mark.path <- "C:/Program Files (x86)/MARK"
options(mark.path = mark.path)
data("dipper")


# Load the SPEI data INP file
# spei_data<- read.csv("nest_survival/data/INP_20220921.csv", header = TRUE)
spei_data <- read_csv("C:/Users/shoepfner/Desktop/nest survival/nest_survival_2022-2025/inp_ns_models_1994-2026_ssa_revision.csv")


# Check the headers. Sometimes column name changes when reading csv files. 
head(spei_data)
summary(spei_data)
names(spei_data)

# Make year a factor variable
is.factor(spei_data$Year)
spei_data$Year<-as.factor(spei_data$Year)
is.factor(spei_data$Year) #it is now a factor variable
str(spei_data)
spei_data <- as.data.frame(spei_data)

setwd("C:/Users/shoepfner/Desktop/nest survival/nest_survival_2022-2025")

spei_data %>% filter (Year == 2019) %>%
  summarise(n=n()) 
#164 nests that I included in 2019 nest survival, Randall included 165, should be 165!!


###############################################################################

# Fit nest survival models by taking away the intercept and use initial values
# Information: From year 1994 to 2021 the earliest julian date is 134 and latest is 195
# To get the Number of Occasions (NOCC), a.k.a duration of the study, the season length
# will be the difference of the latest and earliest. 195-134= 61
    #but the code didn't like that so ran 62 so it counts both the first found and last checked days
# code from Friendly et al. (2025)

###############################################################################

run.models_1 = function()
{
  # 1. constant daily survival rate model (null)
  S.dot = mark(spei_data, nocc = 62, model = "Nest", model.name = "S.dot",
               # ddl= nest.ddl,
               model.parameters = list(S=list(formula = ~ 1)))

  # 2. year
  S.sy = mark(spei_data, nocc = 62, model = "Nest", model.name = "S.sy", groups = c("Year"),
              # ddl= nest.ddl,
              model.parameters = list(S=list(formula = ~ Year)))

  # 3. year + init
  S.syi = mark(spei_data, nocc = 62, model = "Nest", model.name = "S.syi", groups = c("Year"),
               # ddl= nest.ddl,
               model.parameters = list(S=list(formula = ~ Year + init)),
               initial = S.sy)

  # 4. year + nestage
  S.syn = mark(spei_data, nocc = 62, model = "Nest", model.name = "S.syn", groups = c("Year"),
               # ddl= nest.ddl,
               model.parameters = list(S=list(formula = ~ Year + NestAge)),
               initial = S.sy)

  # 5. year + init + nestage
  S.syin = mark(spei_data, nocc = 62, model = "Nest", model.name = "S.syin", groups = c("Year"),
                # ddl= nest.ddl,
                model.parameters = list(S=list(formula = ~ Year + init + NestAge)),
                initial = S.syi)

  # Return model table and list of models

  return(collect.models())
}

# Fit models
model.results_1=run.models_1()

# Look at model results
model.results_1

# Write the results output into excel (csv)
model_output_1 <- model.results_1$model.table
# write.csv(model_output_1, "nest_survival/output/model_ouput_1.csv")

model.results_1$S.sy$results$beta
model.results_1$S.syn$results$beta
model.results_1$S.syin$results$beta
model.results_1$S.syi$results$beta


##############################################################################

# Work with stage 1 top model to get 24-day cumulative nest success estimates.
# This is going to create a figure of the annual nest survival estimates for each
# year and site, while accounting for the factors of nest initiation date and nestage.

##############################################################################


# Use find.covariates to get dataframe for mean individual covariates
  #look at the top two models within 2 DeltaAICC
  #focus on top_model_2, init barely overlaps 0 and should be included
top_model_1 <- model.results_1$S.syn
top_model_2 <- model.results_1$S.syin   #USE this one!top model and includes covariates we care about
top_model_3 <- model.results_1$S.sy
ffc2 <- find.covariates(top_model_2, spei_data)



# Build a design matrix for nestages 1:24, which is my assumed incubation period until
# success. The following code will assign nest ages 1:24 for nestage for each year/site.
# When doing this, be careful and make sure the year and site are corresponding to on another
# the design matrix. Or else your estimates will be mixed.

ffc2$value[1770:1793] <- 1:24 #1994
ffc2$value[1831:1854] <- 1:24 #1995
ffc2$value[1892:1915] <- 1:24 #1996
ffc2$value[1953:1976] <- 1:24 #1997
ffc2$value[2014:2037] <- 1:24 #1998
ffc2$value[2075:2098] <- 1:24 #1999
ffc2$value[2136:2159] <- 1:24 #2000
ffc2$value[2197:2220] <- 1:24 #2001
ffc2$value[2258:2281] <- 1:24 #2002
ffc2$value[2319:2342] <- 1:24 #2003
ffc2$value[2380:2403] <- 1:24 #2004
ffc2$value[2441:2464] <- 1:24 #2005
ffc2$value[2502:2525] <- 1:24 #2006
ffc2$value[2563:2586] <- 1:24 #2007
ffc2$value[2624:2647] <- 1:24 #2008
ffc2$value[2685:2708] <- 1:24 #2009
ffc2$value[2746:2769] <- 1:24 #2010
ffc2$value[2807:2830] <- 1:24 #2011
ffc2$value[2868:2891] <- 1:24 #2012
ffc2$value[2929:2952] <- 1:24 #2013
ffc2$value[2990:3013] <- 1:24 #2014
ffc2$value[3051:3074] <- 1:24 #2015
ffc2$value[3112:3135] <- 1:24 #2019
ffc2$value[3173:3196] <- 1:24 #2021
ffc2$value[3234:3257] <- 1:24 #2022
ffc2$value[3295:3318] <- 1:24 #2023
ffc2$value[3356:3379] <- 1:24 #2024
ffc2$value[3417:3440] <- 1:24 #2025
ffc2$value[3478:3501] <- 1:24 #2026



##use the Julian date of the annual mean
ffc2 <- ffc2 %>%
  left_join(annual_init, by = "Year") %>%
  mutate(value = InitDate) %>%
  select(-InitDate, -Year)

ffc2$value[1:61] <- seq(145.9231, length = 1) #1994
ffc2$value[62:122] <- seq(147.6907, length = 1) #1995
ffc2$value[123:183] <- seq(143.4035, length = 1) #1996
ffc2$value[184:244] <- seq(143.6014, length = 1) #1997
ffc2$value[245:305] <- seq(154.1308, length = 1) #1998
ffc2$value[306:366] <- seq(156.3071, length = 1) #1999
ffc2$value[367:427] <- seq(154.3534, length = 1) #2000
ffc2$value[428:488] <- seq(155.8571, length = 1) #2001
ffc2$value[489:549] <- seq(145.8333, length = 1) #2002
ffc2$value[550:610] <- seq(147.5902, length = 1) #2003
ffc2$value[611:671] <- seq(143.7887, length = 1) #2004
ffc2$value[672:732] <- seq(144.0146, length = 1) #2005
ffc2$value[733:793] <- seq(154.9940, length = 1) #2006
ffc2$value[794:854] <- seq(148.0578, length = 1) #2007
ffc2$value[855:915] <- seq(151.0299, length = 1) #2008
ffc2$value[916:976] <- seq(149.4946, length = 1) #2009
ffc2$value[977:1037] <- seq(150.7353, length = 1) #2010
ffc2$value[1038:1098] <- seq(149.1852, length = 1) #2011
ffc2$value[1099:1159] <- seq(160.5364, length = 1) #2012
ffc2$value[1160:1220] <- seq(156.9444, length = 1) #2013
ffc2$value[1221:1281] <- seq(142.9643, length = 1) #2014
ffc2$value[1282:1342] <- seq(149.7647, length = 1) #2015
ffc2$value[1343:1403] <- seq(141.8788, length = 1) #2019
ffc2$value[1404:1464] <- seq(146.9510, length = 1) #2021
ffc2$value[1465:1525] <- seq(148.8421, length = 1) #2022
ffc2$value[1526:1586] <- seq(148.4444, length = 1) #2023
ffc2$value[1587:1647] <- seq(154.0517, length = 1) #2024
ffc2$value[1648:1708] <- seq(145.0000, length = 1) #2025
ffc2$value[1709:1769] <- seq(146.8125, length = 1) #2026


fdesign2 <- fill.covariates(top_model_2, ffc2)

# Extract the first 24 nestages for each from design matrix
  #this code just pulls out all 1769 values, later the exact columns by year get pulled out
ffull.survival <- compute.real(top_model_2, design = fdesign2, vcv = TRUE) #vcv = TRUE, returns vcv instead of se
freal <- ffull.survival$real
vcv <- ffull.survival$vcv.real 


# DELTA-METHOD
# The function deltamethod.special computes delta-method standard errors.
  #uses Delta method special function and then the product of the estimates and vcv matrix for the same rows
# After you run the function, it will return the standard errors.
# To get the variance, you square that value.
# The codes below will now compute to get the standard error.
s1994 <- deltamethod.special("prod",ffull.survival$real[1:24],ffull.survival$vcv.real[1:24,1:24])
s1995 <- deltamethod.special("prod",ffull.survival$real[62:85],ffull.survival$vcv.real[62:85,62:85])
s1996 <- deltamethod.special("prod",ffull.survival$real[123:146],ffull.survival$vcv.real[123:146,123:146])
s1997 <- deltamethod.special("prod",ffull.survival$real[184:207],ffull.survival$vcv.real[184:207,184:207])
s1998 <- deltamethod.special("prod",ffull.survival$real[245:268],ffull.survival$vcv.real[245:268,245:268])
s1999 <- deltamethod.special("prod",ffull.survival$real[306:329],ffull.survival$vcv.real[306:329,306:329])
s2000 <- deltamethod.special("prod",ffull.survival$real[367:390],ffull.survival$vcv.real[367:390,367:390])
s2001 <- deltamethod.special("prod",ffull.survival$real[428:451],ffull.survival$vcv.real[428:451,428:451])
s2002 <- deltamethod.special("prod",ffull.survival$real[489:512],ffull.survival$vcv.real[489:512,489:512])
s2003 <- deltamethod.special("prod",ffull.survival$real[550:573],ffull.survival$vcv.real[550:573,550:573])
s2004 <- deltamethod.special("prod",ffull.survival$real[611:634],ffull.survival$vcv.real[611:634,611:634])
s2005 <- deltamethod.special("prod",ffull.survival$real[672:695],ffull.survival$vcv.real[672:695,672:695])
s2006 <- deltamethod.special("prod",ffull.survival$real[733:756],ffull.survival$vcv.real[733:756,733:756])
s2007 <- deltamethod.special("prod",ffull.survival$real[794:817],ffull.survival$vcv.real[794:817,794:817])
s2008 <- deltamethod.special("prod",ffull.survival$real[855:878],ffull.survival$vcv.real[855:878,855:878])
s2009 <- deltamethod.special("prod",ffull.survival$real[916:939],ffull.survival$vcv.real[916:939,916:939])
s2010 <- deltamethod.special("prod",ffull.survival$real[977:1000],ffull.survival$vcv.real[977:1000,977:1000])
s2011 <- deltamethod.special("prod",ffull.survival$real[1038:1061],ffull.survival$vcv.real[1038:1061,1038:1061])
s2012 <- deltamethod.special("prod",ffull.survival$real[1099:1122],ffull.survival$vcv.real[1099:1122,1099:1122])
s2013 <- deltamethod.special("prod",ffull.survival$real[1160:1183],ffull.survival$vcv.real[1160:1183,1160:1183])
s2014 <- deltamethod.special("prod",ffull.survival$real[1221:1244],ffull.survival$vcv.real[1221:1244,1221:1244])
s2015 <- deltamethod.special("prod",ffull.survival$real[1282:1305],ffull.survival$vcv.real[1282:1305,1282:1305])
s2019 <- deltamethod.special("prod",ffull.survival$real[1343:1366],ffull.survival$vcv.real[1343:1366,1343:1366])
s2021 <- deltamethod.special("prod",ffull.survival$real[1404:1427],ffull.survival$vcv.real[1404:1427,1404:1427])
s2022 <- deltamethod.special("prod",ffull.survival$real[1465:1488],ffull.survival$vcv.real[1465:1488,1465:1488])
s2023 <- deltamethod.special("prod",ffull.survival$real[1526:1549],ffull.survival$vcv.real[1526:1549,1526:1549])
s2024 <- deltamethod.special("prod",ffull.survival$real[1587:1610],ffull.survival$vcv.real[1587:1610,1587:1610])
s2025 <- deltamethod.special("prod",ffull.survival$real[1648:1671],ffull.survival$vcv.real[1648:1671,1648:1671])
s2026 <- deltamethod.special("prod",ffull.survival$real[1709:1732],ffull.survival$vcv.real[1709:1732,1709:1732])
                             

# Obtain the DSR estimates
survival.24 <- compute.real(top_model_2, design = fdesign2)[c(1:24,
                                                             62:85,
                                                             123:146,
                                                             184:207,
                                                             245:268,
                                                             306:329,
                                                             367:390,
                                                             428:451,
                                                             489:512,
                                                             550:573,
                                                             611:634,
                                                             672:695,
                                                             733:756,
                                                             794:817,
                                                             855:878,
                                                             916:939,
                                                             977:1000,
                                                             1038:1061,
                                                             1099:1122,
                                                             1160:1183,
                                                             1221:1244,
                                                             1282:1305,
                                                             1343:1366,
                                                             1404:1427,
                                                             1465:1488,
                                                             1526:1549,
                                                             1587:1610,
                                                             1648:1671,
                                                             1709:1732), ]

# Get the product of the 24 nestage DSR estimates to achieve cumulative nest success probability.
product1994 <- prod(survival.24$estimate[1:24])
product1995 <- prod(survival.24$estimate[25:48])
product1996 <- prod(survival.24$estimate[49:72])
product1997 <- prod(survival.24$estimate[73:96])
product1998 <- prod(survival.24$estimate[97:120])
product1999 <- prod(survival.24$estimate[121:144])
product2000 <- prod(survival.24$estimate[145:168])
product2001 <- prod(survival.24$estimate[169:192])
product2002 <- prod(survival.24$estimate[193:216])
product2003 <- prod(survival.24$estimate[217:240]) 
product2004 <- prod(survival.24$estimate[241:264]) 
product2005 <- prod(survival.24$estimate[265:288])
product2006 <- prod(survival.24$estimate[289:312])
product2007 <- prod(survival.24$estimate[313:336])
product2008 <- prod(survival.24$estimate[337:360])
product2009 <- prod(survival.24$estimate[361:384])
product2010 <- prod(survival.24$estimate[385:408]) 
product2011 <- prod(survival.24$estimate[406:432]) 
product2012 <- prod(survival.24$estimate[433:456]) 
product2013 <- prod(survival.24$estimate[457:480]) 
product2014 <- prod(survival.24$estimate[481:504])
product2015 <- prod(survival.24$estimate[505:528])
product2019 <- prod(survival.24$estimate[529:552]) 
product2021 <- prod(survival.24$estimate[553:576]) 
product2022 <- prod(survival.24$estimate[577:600]) 
product2023 <- prod(survival.24$estimate[601:624]) 
product2024 <- prod(survival.24$estimate[625:648]) 
product2025 <- prod(survival.24$estimate[649:672])
product2026 <- prod(survival.24$estimate[673:696])


# Create a dataframe from the products of the 24 nestage estimates above.
probsuccess <- c(product1994,product1995,product1996,product1997,product1998,product1999,product2000,
                 product2001,product2002,product2003,product2004,product2005,product2006,product2007,
                 product2008,product2009,product2010,product2011,product2012,product2013,product2014,product2015,
                 product2019,product2021,product2022,product2023,product2024,product2025,product2026)
probsuccess <- as.data.frame(probsuccess)

# Create a dataframe for the standard errors for year and site.
#se not using the delta method, just the compute.real function
se <- c(s1994,s1995,s1996,s1997,s1998,s1999,s2000,s2001,s2002,s2003,
        s2004,s2005,s2006,s2007,s2008,s2009,s2010,s2011,s2012,s2013,
        s2014,s2015,s2019,s2021,s2022,s2023,s2024,s2025,s2026)
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

data$year <- c("1994","1995","1996","1997","1998","1999","2000","2001","2002","2003",
               "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013",
               "2014","2015","2019","2021","2022","2023","2024","2025","2026")

data$probsuccess<-as.numeric(data$probsuccess) #in case it is not recognized as numerical values

# save new data frame as csv to not need to run model set 1 again
annual_ns_estimates <- data
annual_ns_estimates


write.csv(data, "C:/Users/shoepfner/Desktop/nest survival/nest_survival_2022-2025/annual_NS_1994-2026.csv")
annual_ns_estimates <- read.csv("C:/Users/shoepfner/Desktop/nest survival/nest_survival_2022-2025/annual_NS_1994-2026.csv", header = TRUE)
summary(annual_ns_estimates)
annual_ns_estimates$year <- as.factor(annual_ns_estimates$year)


#pull out just 2022-2026 for the memo
short <- annual_ns_estimates %>% filter(year %in% c(2022, 2023, 2024, 2025, 2026))


# Plot
ggplot(short, aes(x = year, y = probsuccess)) +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.13) +
  geom_errorbar(aes(ymin = lci, ymax = uci, width = 0.4)) +
  geom_point() +
  # geom_hline(yintercept = mean(data$probsuccess[1:24]), color = "red", lty = "dashed") +
  # geom_hline(yintercept = mean(data$probsuccess[25:34]), color = "blue", lty = "dashed") +
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
# ggsave("nest_survival/output/nest_success_prob_vs_year.jpg",width = 7, height = 5, dpi = 600)



####compare to Randall's work####
randall <- read_csv("C:/Users/shoepfner/Desktop/nest survival/Randall's work/Randall_output.csv")
names(randall)
names(annual_ns_estimates)

annual_ns_estimates <- annual_ns_estimates %>% 
  mutate(source = "Sarah")

randall <- randall %>% filter(!(Site== "Utqiaġvik")) %>%
  mutate(source = "Randall") %>% 
  select(-Site)
randall$year <- as.factor(randall$year)

combined <- bind_rows(annual_ns_estimates, randall)

ggplot(combined, aes(x = (year), y = probsuccess, color = source)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(
    aes(ymin = lci, ymax = uci),
    position = position_dodge(width = 0.5),
    width = 0.3 ) +
  labs(title = "comparison of Randall and Sarah's cumulative nest survival estimates 1994-2026",
    x = "Year",
    y = "Cumulative nest survival",
    color = "Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# If you want to clean up the mark*.inp, .vcv, .res and .out
#  and .tmp files created by RMark in the working directory,
#  execute 'rm(list = ls(all = TRUE))' - see 2 lines below.
# NOTE: this will delete all objects in the R session.
rm(list = ls(all=TRUE))
# Then, execute 'cleanup(ask = FALSE)' to delete orphaned output
#  files from MARK. Execute '?cleanup' to learn more
cleanup(ask = FALSE)






####clutch size and membrane summary stats####
  #look at clutch size of nests from 2022-2025 for Erik Osnas' IPM
  #nests in 2021 and 12 in 2024 did not have columns n_eggsTotal and n_eggsViable not entered so I entered them 
      #but no longer relevant now since those years aren't being included

db <- "C:/Users/shoepfner/Desktop/nest survival/db_kig_nesting_waterfowl_2019-2026_compiled_20250724_clutch.accdb"
con_db<-odbcConnectAccess2007(db)
sqlTables(con_db,tableType="TABLE")$TABLE_NAME
nest <- sqlFetch(con_db, "tbl_Nest")
visit <- sqlFetch(con_db, "tbl_Visit")

#clean up
nest <- nest %>% filter(cat_species == "SPEI")
visit <- visit %>% mutate(year = lubridate::year(dt_visit))
visit$year <- as.factor(visit$year)
visit$n_eggsTotal <- as.numeric(visit$n_eggsTotal)
visit$n_eggMembrane <- as.numeric(visit$n_eggMembrane)
visit$n_ducklingAlive <- as.numeric(visit$n_ducklingAlive)
visit$n_ducklingDead <- as.numeric(visit$n_ducklingDead)

visit %>% summarise(na_count = sum(is.na(n_eggsTotal)))
#254/1664 have no egg  values = 15.3%, I filled in at least one row for each nest
#years 2021, and some of 2024, had total egg numbers missing


##clutch counts##

#find most "true" clutch size
  #need to find the visit that has the most eggs per nest
unique(visit$id_nest)   #508 nests
eggs <- visit %>% group_by(id_nest) %>% 
  slice_max(order_by = n_eggsTotal, n = 1, with_ties = FALSE)

#exclude nests that had less than three eggs and an early float suggesting still in lay
  #removed 45 nests
eggs_3 <- eggs %>% filter(!(n_eggsTotal <= 3 & cat_float1 == "SF"))
#subset just to 2022-2026
eggs_3 <- eggs_3 %>% filter(year %in% c(2022, 2023, 2024, 2025, 2026))

#number of eggs in a clutch per year
eggs_3 %>% group_by(year) %>%
  summarise(n = n())
#overall average
mean(eggs_3$n_eggsTotal)   #5.009
#annual average
eggs_3 %>% group_by(year) %>%
  summarise(mean_value = mean(n_eggsTotal))
#annual median
eggs_3 %>% group_by(year) %>%
  summarise(median_value = median(n_eggsTotal))
#annual mode
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]}
eggs_3 %>% group_by(year) %>%
  summarise(mode_value = find_mode(n_eggsTotal))


#plots
plot_clutch <- eggs_3 %>% group_by(year, n_eggsTotal) %>%
  summarise(n = n())
plot_clutch$n_eggsTotal <- as.factor(plot_clutch$n_eggsTotal)
ggplot(plot_clutch, aes(x = year, y = n, fill = n_eggsTotal)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    # title = "SPEI clutch sizes 2019-2025",
    x = "Year",
    y = "Count",
    fill = "clutch size" ) +
  theme_minimal(base_size = 14) 



#same thing as above but a box and whisker plot
#not the greatest plot
ggplot(eggs_3, aes(x = year, y = n_eggsTotal)) +
  geom_boxplot(notch = TRUE, outlier.colour = "red", outlier.shape = 8) +
  labs(title = "SPEI clutch sizes 2019-2025",
       x = "Year",
       y = "Clutch size") +
  theme_minimal()




##membrane count##
#Access shows 236 nest (visits) with membranes observed
membrane <- visit %>% group_by(id_nest) %>% 
  slice_max(order_by = n_eggMembrane, n = 1, with_ties = FALSE) %>%
  filter(!(n_eggMembrane == 0))      #get 256 here, there may have been repeat visits to nests where membranes were counted

#subset just to 2022-2026
membrane <- membrane %>% filter(year %in% c(2022, 2023, 2024, 2025, 2026))

#number of nests per year with membranes
mean(membrane$n_eggMembrane)   #3.836
membrane %>% group_by(year) %>%
  summarise(n = n())
membrane %>% group_by(year) %>%
  summarise(mean_value = mean(n_eggMembrane))
membrane %>% group_by(year) %>%
  summarise(median_value = median(n_eggMembrane))
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]}
membrane %>% group_by(year) %>%
  summarise(mode_value = find_mode(n_eggMembrane))
str(membrane)



#plots
plot_membrane <- membrane %>% group_by(year, n_eggMembrane) %>%
  summarise(n = n())
plot_membrane$n_eggMembrane <- as.factor(plot_membrane$n_eggMembrane)

ggplot(plot_membrane, aes(x = year, y = n, fill = n_eggMembrane)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    # title = "SPEI membranes present in nests 2019-2025",
    x = "Year",
    y = "Count",
    fill = "membrane count" ) +
  theme_minimal(base_size = 14)


