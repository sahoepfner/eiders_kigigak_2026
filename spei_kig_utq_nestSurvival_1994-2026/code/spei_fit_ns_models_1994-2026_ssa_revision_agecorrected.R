#Summary of Reproductive Output of Spectacled Eiders at Kigigak Island and Utqiagvik, 1994-2026
#code and analysis by Sarah Hoepfner October 2026
#analysis used for memo for SPEI SSA 5-year review in 2026
  #for nest survival analyses most of the code is modified from Randall Friendly's code in Friendly et al. (2025)
    #used Randall's INP file from 1994-2021
      #this input file has the dates standardized to the earliest found nest date for EACH YEAR
      #this means that NO time trends can be used
    #added in more recent compiled data from Kiggiak and Utqiagvik, 2022-2026
      #similar to the INP files from 1994-2021, the dates are standardized per year

#future analyses should standardize the whole dataset to the earliest date of any nest found 1994-XXXX

#end of the script has summary stat code for clutch sizes and membranes found in nests 2022-2026

# Load packages to run NS models
library(RMark) #to run Program MARK
library(tidyverse) #helps to handle and transform data
library(data.table) #for data manipulation and visuals
library(readr) #read in excel files that are not .csv
library(readxl) #read in excel files
library(msm) #to use delta method to estimate variance components
library(ggplot2) #create figures using ggplot
library(plotrix) #compute simple statistics
library(RODBC) #read in files from Access
library(dplyr) #data management and piping functions
options(scipen = 999)  #show more decimal digits

#clean up workspace
rm(list = ls())

#skip to line 373 for combining the separate INP files
#skip to line 460 for reading in the final INP file and run the models
#skip to line 706 to see final estimates and update graphs without re-running the models

#functions to write csv files are commented out currently to not overwrite shared files

#set wd to project folder "spei_kig_utq_nestSurvival_1994-2026"
setwd("C:/Users/shoepfner/Documents/spei_kig_utq_nestSurvival_1994-2026")

####Kig data 2022-2026####

#data manipulation in R through Access and not making edits only in Excel

#start by prepping the more recent Kig nest data that is in Access
db <- "data/KIG/db_kig_nesting_waterfowl_2019-2026_appended_20260821.accdb"
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

#add in site name 
hatched$Site <- "kig"

hatched


##add AgeDay1
  #has to be specific per year/site so add in here
age <- hatched
age %>% group_by(Year) %>% 
  summarise(min_Found = min(Found, na.rm = TRUE))
age <- age %>% group_by(Year) %>% 
  mutate(AgeDay1 = min(Found, na.rm = TRUE) - init) %>%
  ungroup()



INP_2019_2026 <- age
# write.csv(INP_2019_2026, "data/KIG/spei_kig_INP_2019-2026.csv")



####UTQ SPEI####
visit_UTQ <- read_csv("data/UTQ/spei_utq_attributes_compiled_2026_09_03.csv")

names(visit_UTQ)
visit_UTQ <- visit_UTQ %>% select(c(nest, species, year, date, time_visit, nest_status,eggs_method, eggs_age_1, eggs_age_2, 
                                    eggs_starred, eggs_pipped, young_dry, young_wet))
str(visit_UTQ)
visit_UTQ$year <- as.factor(visit_UTQ$year)
visit_UTQ <- visit_UTQ %>% filter(species=="SPEI") 
visit_UTQ <- visit_UTQ %>% filter(year %in% c(2022,2023,2024,2025,2026)) #243 visits
names(visit_UTQ)

#remove nests that only have one visit, aka contribute nothing to exposure days
  #found already hatched or depredated
visit <- visit_UTQ %>%
  group_by(nest) %>%
  filter(n() > 1) %>%
  ungroup() 
visit <- visit %>% 
  filter(!(nest %in% c("26-JAN086", "22-EJB077")))
unique(visit$nest) 
#leaves behind 212 visits and 67 nests

#date found, last active, and last alive
visit <- visit %>%  group_by(nest) %>%
  mutate(dt_found = min(date, na.rm = TRUE)) %>%
  mutate(dt_lastAlive = max(date[nest_status == "active"], na.rm = TRUE)) %>%
  mutate(dt_lastCheck = min(date[nest_status %in% c("destroyed", "hatched", "abandoned")], na.rm = TRUE)) %>%
  mutate(fate = nest_status[which.max(date)]) %>%
  ungroup() 

visit <- visit %>% mutate(float_age1 = case_when(
  eggs_age_1 == "SF"   ~ "2",
  eggs_age_1 == "SA"  ~ "5",
  eggs_age_1 == "SO"   ~ "8",
  eggs_age_1 == "SP"   ~ "11",
  eggs_age_1 == "FN"  ~ "13",
  eggs_age_1 == "FS"   ~ "15",
  eggs_age_1 == "FB"   ~ "18",
  eggs_age_1 == "FO"  ~ "21",
  eggs_age_1 == "FA"   ~ "22",
  eggs_age_1 == "PS"   ~ "23",
  eggs_age_1 == "PP"  ~ "24",
  TRUE ~ "NA"))

visit <- visit %>% mutate(float_age2 = case_when(
  eggs_age_2 == "SF"   ~ "2",
  eggs_age_2 == "SA"  ~ "5",
  eggs_age_2 == "SO"   ~ "8",
  eggs_age_2 == "SP"   ~ "11",
  eggs_age_2 == "FN"  ~ "13",
  eggs_age_2 == "FS"   ~ "15",
  eggs_age_2 == "FB"   ~ "18",
  eggs_age_2 == "FO"  ~ "21",
  eggs_age_2 == "FA"   ~ "22",
  eggs_age_2 == "PS"   ~ "23",
  eggs_age_2 == "PP"  ~ "24",
  TRUE ~ "NA"))

#add in the candle age for one nest
visit$float_age1[visit$nest == "22-MWM001"] <- "0"


visit$float_age1 <- as.numeric(visit$float_age1)
visit$float_age2 <- as.numeric(visit$float_age2)


#choose the largest value for age and estimated hatch date
age <- visit %>%  group_by(nest) %>%
  mutate(age = pmax(float_age1, float_age2, na.rm = TRUE)) %>% 
  ungroup()
age <- age %>% mutate(init=date-age) %>%
  mutate(hatch=init+24)

#change these dates for nests with ducklings seen in the nest 
age$hatch[age$nest == "24-MLE104"] <- "2024-07-11"
age$hatch[age$nest == "26-JAN086"] <- "2026-07-16"
age$init[age$nest == "24-MLE104"] <- "2024-06-17"
age$init[age$nest == "26-JAN086"] <- "2026-06-23"


#pull out just one row per nest so no longer multiple visits
  #should get 69 nests like above
age <- age %>% 
  group_by(nest) %>%
  slice_min(rowSums(is.na(across(everything()))), with_ties = FALSE) %>%
  ungroup()



#make sure hatched nests have last alive and last checked dates match hatch date
names(age)
nests <- age
#if nest hatched and estimated hatch date is between the dt_last alive and dt_last checked then make that the new dates
  #take midpoint of the dt_last alive and dt_last check
nests <- nests %>% mutate(
  NewColumn1 = case_when(
    fate == "hatched" & hatch >= dt_lastAlive & hatch <= dt_lastCheck ~ hatch,
    fate == "hatched" ~ dt_lastAlive + (dt_lastCheck - dt_lastAlive) / 2,
    TRUE ~ as.Date(NA)),
  NewColumn2 = case_when(
    fate == "hatched" & hatch >= dt_lastAlive & hatch <= dt_lastCheck ~ hatch,
    fate == "hatched" ~ dt_lastAlive + (dt_lastCheck - dt_lastAlive) / 2,
    TRUE ~ as.Date(NA)))

nests$NewColumn1[nests$nest == "24-MLE104"] <- "2024-07-11"
nests$NewColumn2[nests$nest == "24-MLE104"] <- "2024-07-11"
nests$NewColumn1[nests$nest == "26-JAN086"] <- "2026-07-16"
nests$NewColumn2[nests$nest == "26-JAN086"] <- "2026-07-16"


#use NewColumn1 and NewColumn2 for dt_lastAlive and dt_lastCheck
nests <- nests %>% mutate(
  dt_lastAlive = if_else(!is.na(NewColumn1), NewColumn1, dt_lastAlive),
  dt_lastCheck = if_else(!is.na(NewColumn2), NewColumn2, dt_lastCheck))

#condense fates
nests <- nests %>%
  mutate(Fate = case_when(
    fate == "abandoned" ~ "1",
    fate == "destroyed" ~ "1",
    fate == "hatched" ~ "0",
    TRUE ~ fate ))

#checks of dates for nest survival models
#hatched nests have the same date for 
checks <- nests %>%
  mutate(
    dates_match = if_else(
      Fate == 0,
      dt_lastAlive == dt_lastCheck,
      NA))  ## no falses in the dates_match column
#dt_found is before lastAlive, or equal to
checks$check <- checks$dt_lastAlive - checks$dt_found
#dt_lastCheck is greater than dt_lastAlive, or equal to
checks$check2 <- checks$dt_lastCheck - checks$dt_lastAlive



#looks good, rename the columns and get rid of columns so ready for combining as an INP
names(checks)
checks <- checks %>% select(nest, year, dt_found, dt_lastAlive, dt_lastCheck, Fate, age, init)
checks <- checks %>% rename(id_yrNest=nest, Year=year, FirstFound=dt_found,
                            LastPresent=dt_lastAlive, LastChecked=dt_lastCheck)
#add column for site
checks$Site <- "utq"
checks

checks %>% summarise(across(everything(), ~ sum(is.na(.))))
#14 nests do not have an age and no init date
  #Randall removed these, also they won't contribute to the model

checks <- checks %>% filter(!is.na(age))

#convert dates to Julian dates
julian <- checks
julian$FirstFound <- as.POSIXlt(julian$FirstFound)$yday
julian$LastPresent <- as.POSIXlt(julian$LastPresent)$yday
julian$LastChecked <- as.POSIXlt(julian$LastChecked)$yday
julian$init <- as.POSIXlt(julian$init)$yday
str(julian)


##add AgeDay1
#has to be specific per year/site so easier to add in here
age1 <- julian
age1 %>% group_by(Year) %>% 
  summarise(min_Found = min(FirstFound, na.rm = TRUE))
age1 <- age1 %>% group_by(Year) %>% 
  mutate(AgeDay1 = min(FirstFound, na.rm = TRUE) - init + 1) %>%
  ungroup()


# write.csv(age1, "data/UTQ/INP_UTQ_2022-2026.csv")




####combine all three INP files####
  #at this step now must combine with Randall's INP file for 1994-2021
  #only use 2022-2026 Kig from the above work - use Randall's work for 2019 and 2022
  #use 2022-2026 for Utq also, those are the only new years since Randall
nest_inp_RJF <- read_csv("data/UTQ/Randall/spei_utq_INP_20220921_Randall.csv")
kig <- read_csv("data/KIG/spei_kig_INP_2019-2026.csv")
utq <- read_csv("data/UTQ/spei_utq_INP_2022-2026.csv")
names(nest_inp_RJF)
names(kig)
names(utq)
str(nest_inp_RJF)
str(kig)
str(utq)

nest_inp_RJF <- nest_inp_RJF %>%
  mutate(id_yrNest = paste(Year, Nest, sep = "")) %>%
  select(-Win_Hi, -Win_Lo, -Spr_Hi, -Spr_Lo, -wxt, -sxt, -wxw, - sxw, -wsi, -wi, -Nest) %>%
  rename(init=Init)
nest_inp_RJF$Year <- as.numeric(nest_inp_RJF$Year)
#NOTE: these input values are all standardized on the annual scale to the earliest date a nest found THAT YEAR
  #future work should standardize to the whole dataset
dates <- nest_inp_RJF %>% group_by (Year) %>% 
  summarise(sum=mean(AgeDay1)) 


#double check the AgeDay1 based on the earliest found nest date
  #values for each year are based off values in Randall's individual and annual R code files
RJF <- nest_inp_RJF %>%
  filter(Year %in% 2009:2021) %>% 
  filter(Site=="kig") %>% 
  mutate(
    FF = case_when(
      Year == 1994 ~ 143,
      Year == 1995 ~ 145,
      Year == 1996 ~ 135,
      Year == 1997 ~ 140,
      Year == 1998 ~ 149,
      Year == 1999 ~ 151,
      Year == 2000 ~ 152,
      Year == 2001 ~ 152,
      Year == 2002 ~ 145,
      Year == 2003 ~ 140,
      Year == 2004 ~ 134,
      Year == 2005 ~ 148,
      Year == 2006 ~ 151,
      Year == 2007 ~ 144,
      Year == 2008 ~ 147,
      Year == 2009 ~ 144,
      Year == 2010 ~ 147,
      Year == 2011 ~ 146,
      Year == 2012 ~ 156,
      Year == 2013 ~ 164,
      Year == 2014 ~ 153,
      Year == 2015 ~ 153,
      Year == 2019 ~ 153,   #should really be 138, error in Randall's code file "2019_INP_Init_NestAge.csv", value copied from 2015
      Year == 2021 ~ 153,   #should really be 143, error in Randall's code file "2021_INP_Init_NestAge.csv", value copied from 2015
      TRUE ~ NA_real_
    ),
    AgeDaysss = FF - init)
RJF$diff <- RJF$AgeDaysss - RJF$AgeDay1 #all equal zero, but need to fix 2019 and 2021
  #the annual code files say earliest day for 2019 is 138, and 2021 is 143

#have to subtract 15 days to 2019's AgeDay1 and 10 days to 2021 for Kig
#UTQ values are fine
nest_inp_RJF <- nest_inp_RJF %>% 
  mutate(
    AgeDay1 = case_when(
      Year == 2019 & Site == "kig" ~ AgeDay1 - 15,    #change by 15 days
      Year == 2021 & Site == "kig" ~ AgeDay1 - 10,    #change by 10 days
      TRUE ~ AgeDay1                  # leave all other years unchanged
    ))

RJF1 <- nest_inp_RJF %>%
  filter(Year %in% 2010:2019) %>% 
  filter(Site=="utq") %>% 
  mutate(
    FF = case_when(
      Year == 2010 ~ 169,
      Year == 2011 ~ 163,
      Year == 2012 ~ 166,
      Year == 2013 ~ 163,
      Year == 2014 ~ 169,
      Year == 2015 ~ 167,
      Year == 2016 ~ 162,
      Year == 2017 ~ 171,
      Year == 2018 ~ 179,
      Year == 2019 ~ 170,
      TRUE ~ NA_real_
    ),
    AgeDaysss = FF - init
  )
RJF1$diff <- RJF1$AgeDaysss - RJF1$AgeDay1
  #one nest is off by one day but all is fine






kig <- kig %>% 
  select(-ID_tblNest, -id_nest, -dt_Hatch, -notes_nest, -dt_visit, -cat_nestStatus, 
         -maxAge, - estHatch, -daysToHatch, -Hatch_Date, -"...1") %>%
  rename(FirstFound=Found, LastPresent=LastPres, LastChecked=LastCheck) %>% 
  mutate(Freq = 1)
kig$FirstFound <- as.numeric(kig$FirstFound)
kig$Fate <- as.numeric(kig$Fate)

#Kig- standardize for each year to the earliest date, to match INP file 1994-2021
  #AgeDay1 already done above
kig <- kig %>% group_by(Year) %>%
  group_by(Year) %>%
  mutate(across(c(FirstFound, LastPresent, LastChecked), 
                ~ . - min(FirstFound-1, na.rm = TRUE))) %>%
  ungroup()



#both Randall's file and my Kig file have the years 2019 and 2021, remove mine
kig <- kig %>% filter(!(Year %in% c(2019,2021)))


#UTQ- standardize for each year to the earliest date, to match INP file 1994-2021
  #AgeDay1 already done above
utq <- utq %>% select(-1)  %>% 
  mutate(Freq = 1)
utq <- utq %>% group_by(Year) %>%
  group_by(Year) %>%
  mutate(across(c(FirstFound, LastPresent, LastChecked), 
                ~ . - min(FirstFound-1, na.rm = TRUE))) %>%
  ungroup()

#nest_inp_RJF=2870, kig later=231, utq later=53 nests


###combine the three files together 
combined <- bind_rows(nest_inp_RJF, kig, utq)  #3154 nests total




#save the final INP file so don't have to always run the above code
# write.csv(combined, "data/spei_kig_utq_INPnestSurvival_1994-2026_ssa_revision_final.csv")




####start modeling####
# Add path to MARK.exe
Sys.setenv(MarkPath = "C:/Program Files (x86)/MARK")
mark.path <- "C:/Program Files (x86)/MARK"
options(mark.path = mark.path)
data("mallard")


# Load the SPEI data INP file
#check the name of this file!
spei_data <- read_csv("data/spei_kig_utq_INPnestSurvival_1994-2026_ssa_revision_final.csv")



# Check the headers. Sometimes column name changes when reading csv files. 
head(spei_data)
summary(spei_data)
names(spei_data)

# Make year and site factor variables
spei_data$Year <-as.factor(spei_data$Year)
spei_data$Site <-as.factor(spei_data$Site)
str(spei_data)
spei_data <- as.data.frame(spei_data)

#set wd to store the mark files
setwd("data/mark")

#just a double check there is the correct number of nests per year
spei_data %>% filter (Year == 2019) %>%
  filter(Site == "kig") %>% 
  summarise(n=n()) 
#164 nests that I included in 2019 nest survival, Randall included 165, should be 165!!



###############################################################################

# Fit nest survival models by taking away the intercept and use initial values
  #how it should be done with dates standardized to earliest date of wholedataset:
    #from year 1994 to 2026 the earliest julian date is 134 and latest is 206
# To get the Number of Occasions (NOCC), a.k.a duration of the study, the season length
# will be the difference of the latest and earliest. 206-134= 72
    #but can really just look at latest last LastChecked number = 50
    #NOCC = 50
# code from Friendly et al. (2025)
  #there are more models commented out that were run by Friendly but we only cared about the full model



###############################################################################

run.models_1 = function()
{
  # 1. constant daily survival rate model (null)
  S.dot = mark(spei_data, nocc = 50, model = "Nest", model.name = "S.dot",
               # ddl= nest.ddl,
               model.parameters = list(S=list(formula = ~ 1)))
  # 
  # # 2. year + site
  # S.sy = mark(spei_data, nocc = 50, model = "Nest", model.name = "S.sy", groups = c("Site", "Year"),
  #             # ddl= nest.ddl,
  #             model.parameters = list(S=list(formula = ~ Year + Site)))
  # 
  # # 3. year + site + init
  # S.syi = mark(spei_data, nocc = 50, model = "Nest", model.name = "S.syi", groups = c("Site", "Year"),
  #              # ddl= nest.ddl,
  #              model.parameters = list(S=list(formula = ~ Year + Site + init)),
  #              initial = S.sy)
  # 
  # # 4. year + site + nestage
  # S.syn = mark(spei_data, nocc = 50, model = "Nest", model.name = "S.syn", groups = c("Site", "Year"),
  #              # ddl= nest.ddl,
  #              model.parameters = list(S=list(formula = ~ Year + Site + NestAge)),
  #              initial = S.sy)
  # 
  # 5. year + site + init + nestage
  S.syin = mark(spei_data, nocc = 50, model = "Nest", model.name = "S.syin", groups = c("Site", "Year"),
                # ddl= nest.ddl,
                model.parameters = list(S=list(formula = ~ Year  + Site + init + NestAge)))
  
  # 6. year * site + init + nestage
  S.syin.inter = mark(spei_data, nocc = 50, model = "Nest", model.name = "S.syin.inter",
                       groups = c("Site", "Year"),
                       model.parameters = list(S = list(formula = ~ -1 + Site:Year + init + NestAge)),  #must be Year and then SIte
                       initial = S.syin)
  # Return model table and list of models

  return(collect.models())
}

# Fit models
model.results_1=run.models_1()

# Look at model results
model.results_1


model.results_1$S.sy$results$beta
model.results_1$S.syin.inter$results$beta


# Write the results output into a csv file
setwd("C:/Users/shoepfner/Documents/spei_kig_utq_nestSurvival_1994-2026")
model_output_1 <- model.results_1$model.table
# write.csv(model_output_1, "output/mark/spei_ns_model_estimates_final_nointer.csv")
model_output_1 <- read.csv("output/mark/spei_ns_model_estimates_final_nointer.csv")






##############################################################################

# Work with stage 1 top model to get 24-day cumulative nest success estimates.
# This is going to create a figure of the annual nest survival estimates for each
# year and site, while accounting for the factors of nest initiation date and nestage.

##############################################################################


# Use find.covariates to get dataframe for mean individual covariates
  #look at the top two models within 2 DeltaAICC
  #focus on top_model_2, init barely overlaps 0 and should be included
top_model_1 <- model.results_1$S.sy
top_model_2 <- model.results_1$S.syin.inter   #USE this one!top model and includes covariates we care about
ffc2 <- find.covariates(top_model_2, spei_data)

#save it to load in for future use without running hte model
# write.csv(top_model_2$results$beta, "output/mark/spei_ns_model_estimates_beta_final_nointer.csv", row.names = TRUE)
top_model_2$results$beta <- read.csv("output/mark/spei_ns_model_estimates_beta_final_nointer.csv")


##Adapted code from Friendly here, used to have to hard code the whole matrix and susceptible to many errors, and loooots of time
  #used Claude to re-vamp the code so it is faster to go through when things change
#nest age values
ffc2 <- ffc2 %>%
  mutate(
    nestage_num = as.numeric(str_extract(var, "(?<=NestAge)\\d+")),
    value = ifelse(!is.na(nestage_num), nestage_num, value),
    group = str_extract(rnames, "(?<=g)[a-z]+\\d+"),
    site  = str_extract(group, "^[a-z]+"),
    year  = as.numeric(str_extract(group, "\\d+")),
    age   = as.numeric(str_extract(rnames, "(?<=a)\\d+")))

#init values
  #use the Julian date of the annual mean
spei_data 
inits <- spei_data %>% 
  group_by(Site, Year) %>% 
  summarise(mean = mean(init), .groups = "drop")
print(inits, n=Inf)
inits$Year <- as.numeric(as.character(inits$Year))
inits <- inits %>% rename(init_mean=mean, site=Site, year=Year)

ffc2 <- ffc2 %>%
  left_join(inits, by = c("site","year")) %>%
  mutate(value = ifelse(var == "init", init_mean, value))

ffc2$value <- as.numeric(ffc2$value)

fdesign2 <- fill.covariates(top_model_2, ffc2)


ffull.survival <- compute.real(top_model_2, design = fdesign2, vcv = TRUE) #vcv = TRUE, returns vcv instead of se
freal <- ffull.survival$real
vcv <- ffull.survival$vcv.real 



# DELTA-METHOD
# The function deltamethod.special computes delta-method standard errors.
  #uses Delta method special function and then the product of the estimates and vcv matrix for the same rows
# After you run the function, it will return the standard errors.
# To get the variance, you square that value.
# The codes below will now compute to get the standard error.

ffc2 <- ffc2 %>%
  mutate(age = as.numeric(str_extract(rnames, "(?<=a)\\d+")))

# Confirm row order matches before trusting this
ffc2_init_only <- ffc2 %>% filter(var == "init")

years <- sort(unique(ffc2_init_only$year))
sites <- unique(ffc2_init_only$site)

#loop: calculate cumulative DSR product per year and site
product_results <- list()

for (yr in years) {
  for (st in sites) {
    rows <- which(ffc2_init_only$year == yr & 
                    ffc2_init_only$site == st & 
                    ffc2_init_only$age %in% 0:23)
    if (length(rows) != 24) next
    key <- paste(yr, st, sep = "_")
    product_results[[key]] <- prod(ffull.survival$real[rows])
  }
}
product_results <- unlist(product_results)


#loop: calculate delta-method SE per year and site
se_results <- list()

for (yr in years) {
  for (st in sites) {
    rows <- which(ffc2_init_only$year == yr & 
                    ffc2_init_only$site == st & 
                    ffc2_init_only$age %in% 0:23)
    if (length(rows) != 24) next
    key <- paste(yr, st, sep = "_")
    se_results[[key]] <- deltamethod.special(
      "prod", ffull.survival$real[rows], ffull.survival$vcv.real[rows, rows]
    )
  }
}
se_results <- unlist(se_results)
   


# Combine product_results and se_results into one data frame
final_results <- data.frame(
  group = names(product_results),
  probsuccess = product_results
) %>%
  left_join(
    data.frame(group = names(se_results), se = se_results),
    by = "group"
  ) %>%
  separate(group, into = c("year", "site"), sep = "_") %>%
  mutate(year = as.numeric(year))

print(final_results)


#add in logit-scale 95% CIs
final_results <- final_results %>%
  mutate(
    logit_est = qlogis(probsuccess),
    logit_se  = se / (probsuccess * (1 - probsuccess)),
    lci = plogis(logit_est - 1.96 * logit_se),
    uci = plogis(logit_est + 1.96 * logit_se))




# save new data frame as csv to not need to run model set 1 again
annual_ns_estimates <- final_results
annual_ns_estimates
# write.csv(annual_ns_estimates, "output/spei_ns_estimates_1994-2026_ssa_revision_final_nointer.csv")


#save results so you don't have to run the model each time
  #this also gets used in the nest abundance analysis
save(top_model_2, fdesign2, ffull.survival, 
     file = "output/mark/spei_nest_survival_top_model_2_results_final_nointer.RData")
ffull.survival2 <- compute.real(top_model_2, design = fdesign2, vcv = TRUE)
saveRDS(ffull.survival2, file = "output/mark/spei_ns_ffull_survival2_final_nointer.rds")
saveRDS(top_model_2, file = "output/mark/spei_ns_top_model_2_final_nointer.rds")






####plot estimates####
#can skip to this line to redo figures and not re-run the model
load("output/mark/spei_nest_survival_top_model_2_results_final_nointer.RData")

annual_ns_estimates <- read.csv("output/spei_ns_estimates_1994-2026_ssa_revision_final_nointer.csv", header = TRUE)
summary(annual_ns_estimates)
annual_ns_estimates$Year <- as.factor(annual_ns_estimates$year)


sample_sizes <- spei_data %>%
  group_by(Year, Site) %>%
  summarise(n = n(), .groups = "drop") %>%
  rename(year=Year, site=Site)
sample_sizes$year <- as.numeric(sample_sizes$year)

annual_ns_estimates <- annual_ns_estimates %>%
  left_join(sample_sizes, by = c("year", "site"))

#hard enter nest numbers
df <- data.frame(
  year = c(1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,
           2008,2009,2010,2011,2012,2013,2014,2015,2019,2021,2022,2023,2024,2025,2026),
  n = c(78,97,114,138,107,127,116,28,138,122,142,137,168,173,134,93,102,108,110,54,
        84,68,165,102,38,81,58,22,32))
annual_ns_estimates <- annual_ns_estimates %>%
  left_join(df, by = "year")


#get annual estimates 2022-2026 for each site
  #this was for the SPEI SSA review Hoepfner 2026
ffull.survival

# Pull the row labels straight from the model's own design data (guaranteed aligned)
row_labels <- top_model_2$design.data$S

years_wanted <- 2021:2026
sites_wanted <- c("kig", "utq")
target_groups <- expand.grid(Year = as.character(years_wanted), 
                             Site = sites_wanted, 
                             stringsAsFactors = FALSE)


#get 2022-2026 model averaged estimates
  #undoes the logit function, takes the mean and  then re-scales to logit scale
mean_2022_2026 <- annual_ns_estimates %>%
  filter(year %in% 2022:2026) %>%      #comment out this to get the overall 1994-2026 mean for each site
  mutate(logit_est = qlogis(probsuccess),
    logit_se  = se / (probsuccess * (1 - probsuccess))) %>%
  group_by(site) %>%
  summarise(
    weighted_logit_mean = sum(logit_est / logit_se^2) / sum(1 / logit_se^2),
    pooled_se = sqrt(1 / sum(1 / logit_se^2))) %>%
  mutate(mean_survival = plogis(weighted_logit_mean),
    lci = plogis(weighted_logit_mean - 1.96 * pooled_se),
    uci = plogis(weighted_logit_mean + 1.96 * pooled_se))

print(mean_2022_2026)


#mean of whole study
#get 2022-2026 model averaged estimates
#undoes the logit function, takes the mean and  then re-scales to logit scale
mean <- annual_ns_estimates %>%
  filter(year %in% 1994:2026) %>%      #comment out this to get the overall 1994-2026 mean for each site
  mutate(logit_est = qlogis(probsuccess),
         logit_se  = se / (probsuccess * (1 - probsuccess))) %>%
  group_by(site) %>%
  summarise(
    weighted_logit_mean = sum(logit_est / logit_se^2) / sum(1 / logit_se^2),
    pooled_se = sqrt(1 / sum(1 / logit_se^2))) %>%
  mutate(mean_survival = plogis(weighted_logit_mean),
         lci = plogis(weighted_logit_mean - 1.96 * pooled_se),
         uci = plogis(weighted_logit_mean + 1.96 * pooled_se))

print(mean)


#pull out just 2022-2026 for the memo graph
short <- annual_ns_estimates %>% filter(year %in% c(2022, 2023, 2024, 2025, 2026))
short
short$year <- as.factor(short$year)
short$site <- as.factor(short$site)
str(short)


annual_ns_estimates$year <- as.factor(annual_ns_estimates$year)
annual_ns_estimates$site <- as.factor(annual_ns_estimates$site)


# Plot
pd <- position_dodge(width = 0.5)
kig_mean <- mean_2022_2026 %>% filter(site == "kig") %>% pull(mean_survival)
utq_mean <- mean_2022_2026 %>% filter(site == "utq") %>% pull(mean_survival)
ggplot(short, aes(x = year, y = probsuccess, color=site, group=site)) +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.13) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4, position = pd) +
  geom_point(position = pd) +
  geom_text(aes(y = uci, label = n.y),
            position = pd,
            vjust = -0.5,
            size = 4,
            show.legend = FALSE, color = "black") +
  geom_hline(yintercept = kig_mean, color = "red", lty = "dashed") +
  geom_hline(yintercept = utq_mean, color = "blue", lty = "dashed") +
  # geom_hline(yintercept = 0.542, color = "red", lty = "dashed") +  #2022-2026 mean
  # geom_hline(yintercept = 0.385, color = "blue", lty = "dashed") +  #2022-2026 mean
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.75, 0.3)) +
  labs(y="Estimated Nest Survival", x="Year", color="Site") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 13)) +
  theme(axis.text.y = element_text(size = 13)) +
  theme(axis.title.y = element_text(
    size = 15)) +
  theme(axis.title.x = element_text(
    size = 15)) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  guides(color = guide_legend(override.aes = list(size = 2, linewidth = 1.2))) + 
  scale_color_brewer(palette = "Set1", labels = c("kig" = "Kigigak", "utq" = "Utqiagvik", size = 15))





#full dataset 1994-2026
pd <- position_dodge(width = 0.5)
kig_mean <- mean %>% filter(site == "kig") %>% pull(mean_survival)
utq_mean <- mean %>% filter(site == "utq") %>% pull(mean_survival)
ggplot(annual_ns_estimates, aes(x = year, y = probsuccess, color=site, group=site)) +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.13) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4, position = pd) +
  geom_point(position = pd) +
  geom_text(aes(y = uci, label = n.y),
            position = pd,
            vjust = -0.5,
            size = 4,
            show.legend = FALSE, color = "black") +
  geom_hline(yintercept = kig_mean, color = "red", lty = "dashed") +
  geom_hline(yintercept = utq_mean, color = "blue", lty = "dashed") +
  # geom_hline(yintercept = 0.542, color = "red", lty = "dashed") +  #2022-2026 mean
  # geom_hline(yintercept = 0.385, color = "blue", lty = "dashed") +  #2022-2026 mean
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.75, 0.3)) +
  labs(y="Estimated Nest Survival", x="Year", color="Site") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 13)) +
  theme(axis.text.y = element_text(size = 13)) +
  theme(axis.title.y = element_text(
    size = 15)) +
  theme(axis.title.x = element_text(
    size = 15)) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 12)) +
  guides(color = guide_legend(override.aes = list(size = 2, linewidth = 1.2))) + 
  scale_color_brewer(palette = "Set1", labels = c("kig" = "Kigigak", "utq" = "Utqiagvik", size = 15))



#code below is for making a variety of other graphs, by site and different years
  #used for 2021-2026 Kigigak SPEI field work report

#plot just Kig data
kig_mean <- mean %>% filter(site == "kig") %>% pull(mean_survival)
ggplot(annual_ns_estimates %>% filter(site == "kig"), aes(x = Year, y = probsuccess, color=site, group=site)) +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.13) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4, position = pd, color="black") +
  geom_point(position = pd, color="black") +
  geom_text(aes(y = uci, label = n.y), 
            position = pd, 
            vjust = -0.5, 
            size = 4,
            show.legend = FALSE, color = "black") +
  # geom_hline(yintercept = kig_mean, color = "black", lty = "dashed") +
  geom_segment(aes(x = 24, xend = 30, y = 0.63), lty = "dashed", color = "red")+
  # geom_hline(yintercept = 0.63, color = "red", lty = "dashed") +
  geom_hline(yintercept = 0.70, color = "black", lty = "dashed") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.75, 0.3)) +
  labs(y="Estimated Nest Survival", x="Year", color="Site", title="SPEI estimated nest survival at Kigigak Island 1994-2026") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 13)) +
  theme(axis.text.y = element_text(size = 13)) +
  theme(axis.title.y = element_text(
    size = 15)) +
  theme(axis.title.x = element_text(
    size = 15)) +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1", labels = c("kig" = "Kigigak", "utq" = "Utqiagvik"))



#plot just UTQ data
utq_mean <- mean %>% filter(site == "utq") %>% pull(mean_survival)
ggplot(annual_ns_estimates %>% filter(site == "utq"), aes(x = Year, y = probsuccess, color=site, group=site)) +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.13) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4, position = pd, color = "black") +
  geom_point(position = pd, color="black") +
  geom_text(aes(y = uci, label = n), 
            position = pd, 
            vjust = -0.5, 
            size = 3,
            show.legend = FALSE, color = "black") +
  # geom_segment(
  #   x = "1994", xend = "2021",   #show just the estimates from Randall's work
  #   y = mean(short$probsuccess[1:24]), yend = mean(short$probsuccess[1:24]),
  #   color = "red", linetype = "dashed") +
  geom_hline(yintercept = utq_mean, color = "black", lty = "dashed") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.75, 0.3)) +
  labs(y="Estimated Nest Survival", x="Year", color="Site", title="SPEI estimated nest survival at Utqiagvik 2010-2026") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 13)) +
  theme(axis.text.y = element_text(size = 13)) +
  theme(axis.title.y = element_text(
    size = 15)) +
  theme(axis.title.x = element_text(
    size = 15)) +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1", labels = c("kig" = "Kigigak", "utq" = "Utqiagvik"))







####clutch size and membrane summary stats####
  #nests in 2021 and 12 in 2024 did not have columns n_eggsTotal and n_eggsViable not entered so I entered them 
      #but no longer relevant now since those years aren't being included
#first is Kig data and then UTQ data below

db <- "data/KIG/db_kig_nesting_waterfowl_2019-2026_compiled_20250724_clutch.accdb"
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


####clutch counts####

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

#change a couple outliers
  #nests had dump eggs
eggs_3 <- eggs_3 %>%
  mutate(n_eggsTotal = ifelse(id_nest == "CIC048", 4, n_eggsTotal))

#number of eggs in a clutch per year
eggs_3 %>% group_by(year) %>%
  summarise(n = n())
#overall average
mean(eggs_3$n_eggsTotal)   #4.99
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


#get 95%CI on the mean
sd(eggs_3$n_eggsTotal) #1.101307
se <- 1.101307 / sqrt(217)  #SE= 0.07476
estimate <- 4.99      # your point estimate        
conf_level <- 0.95   # desired confidence level
# Compute z critical value
alpha <- 1 - conf_level
z_crit <- qnorm(1 - alpha/2)
# Confidence interval
lower <- estimate - z_crit * se
upper <- estimate + z_crit * se
ci <- c(lower, upper)
names(ci) <- c("Lower", "Upper")
print(ci)


#plots
plot_clutch <- eggs_3 %>%
  group_by(year, n_eggsTotal) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(percentage = count / sum(count) * 100,
         total_n = sum(count))

label_data <- plot_clutch %>%
  group_by(year) %>%
  summarise(max_pct = max(percentage), total_n = first(total_n))

ggplot(plot_clutch, aes(x = factor(year), y = percentage, fill = factor(n_eggsTotal))) +
  geom_col(position = "dodge", color = "black", linewidth = 0.3) +
  geom_text(data = label_data, 
            aes(x = factor(year), y = max_pct + 1, label = paste0("n = ", total_n)),
            inherit.aes = FALSE, size = 3.5) +
  labs(x = "Year", y = "Percentage (%)", fill = "Clutch size") +
  theme_bw() 







###UTQ data
utq_eggs <- read_csv("data/UTQ/sepi_utq_attributes_compiled_2026_09_03.csv")

names(utq_eggs)
utq_eggs <- utq_eggs %>% select(c(nest, species, year, date, time_visit, nest_status, eggs_whole, eggs_mems,
                                    eggs_starred, eggs_pipped, young_dry, young_wet, eggs_age_1, eggs_age_2))
str(utq_eggs)
utq_eggs$year <- as.factor(utq_eggs$year)
utq_eggs <- utq_eggs %>% filter(species=="SPEI") 
utq_eggs <- utq_eggs %>% filter(year %in% c(2022,2023,2024,2025,2026)) #243 visits
names(utq_eggs)

#find most "true" clutch size
  #need to find the visit that has the most eggs per nest
unique(utq_eggs$nest)   #94 nests
eggs <- utq_eggs %>% group_by(nest) %>% 
  slice_max(order_by = eggs_whole, n = 1, with_ties = FALSE)

#exclude nests that had less than three eggs and an early float suggesting still in lay
#removed 45 nests
eggs_3 <- eggs %>% filter(!(eggs_whole <= 3 & eggs_age_1 == "SF" & eggs_age_2 == "SF"))
#subset just to 2022-2026
eggs_3 <- eggs_3 %>% filter(year %in% c(2022, 2023, 2024, 2025, 2026))   #50 nests

#number of eggs in a clutch per year
eggs_3 %>% group_by(year) %>%
  summarise(n = n())
#overall average
mean(eggs_3$eggs_whole)   #4.56
#annual average
eggs_3 %>% group_by(year) %>%
  summarise(mean_value = mean(eggs_whole))
#annual median
eggs_3 %>% group_by(year) %>%
  summarise(median_value = median(eggs_whole))
#annual mode
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]}
eggs_3 %>% group_by(year) %>%
  summarise(mode_value = find_mode(eggs_whole)[1])


#get 95%CI on the mean
sd(eggs_3$eggs_whole) #1.180799
se <- 1.180799 / sqrt(50)  #SE= 0.1669901
estimate <- 4.56      # your point estimate        
conf_level <- 0.95   # desired confidence level
# Compute z critical value
alpha <- 1 - conf_level
z_crit <- qnorm(1 - alpha/2)
# Confidence interval
lower <- estimate - z_crit * se
upper <- estimate + z_crit * se
ci <- c(lower, upper)
names(ci) <- c("Lower", "Upper")
print(ci)


#plots
plot_clutch <- eggs_3 %>%
  group_by(year, eggs_whole) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(percentage = count / sum(count) * 100,
         total_n = sum(count))

label_data <- plot_clutch %>%
  group_by(year) %>%
  summarise(max_pct = max(percentage), total_n = first(total_n))

ggplot(plot_clutch, aes(x = factor(year), y = percentage, fill = factor(eggs_whole))) +
  geom_col(position = "dodge", color = "black", linewidth = 0.3) +
  geom_text(data = label_data, 
            aes(x = factor(year), y = max_pct + 5, label = paste0("n = ", total_n)),
            inherit.aes = FALSE, size = 3.5) +
  labs(x = "Year", y = "Percentage (%)", fill = "Clutch size") +
  theme_bw() 








####membrane count####
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


#get 95%CI on the mean
sd(membrane$n_eggMembrane) #1.411144
se <- 1.411144 / sqrt(110)  #SE= 0.1345473
estimate <- 3.836      # your point estimate        
conf_level <- 0.95   # desired confidence level
# Compute z critical value
alpha <- 1 - conf_level
z_crit <- qnorm(1 - alpha/2)
# Confidence interval
lower <- estimate - z_crit * se
upper <- estimate + z_crit * se
ci <- c(lower, upper)
names(ci) <- c("Lower", "Upper")
print(ci)


plot_clutch <- membrane %>%
  group_by(year, n_eggMembrane) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(percentage = count / sum(count) * 100,
         total_n = sum(count))

label_data <- plot_clutch %>%
  group_by(year) %>%
  summarise(max_pct = max(percentage), total_n = first(total_n))

ggplot(plot_clutch, aes(x = factor(year), y = percentage, fill = factor(n_eggMembrane))) +
  geom_col(position = "dodge", color = "black", linewidth = 0.3) +
  geom_text(data = label_data, 
            aes(x = factor(year), y = max_pct + 1, label = paste0("n = ", total_n)),
            inherit.aes = FALSE, size = 3.5) +
  labs(x = "Year", y = "Percentage (%)", fill = "membrane count") +
  theme_bw() 





