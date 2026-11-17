#Summary of Reproductive Output of Spectacled Eiders at Kigigak Island and Utqiagvik, 1994-2026
#code and analysis by Sarah Hoepfner August 2026
  #for nest survival analyses most of the code is modified from Randall Friendly's code in Friendly et al. (2025)
    #used Randall's INP file from 1994-2019
    #added in more recent compiled data from Kiggiak and Utqiagvik, 2022-2026
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

#skip to line 371 for combining the separate INP files
#skip to line 467 for reading in the final INP file and run the models



####Kig data 2022-2026####

#data manipulation in R through Access and not making edits only in Excel

#start by prepping the more recent Kig nest data that is in Access
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

#add in site name 
hatched$Site <- "kig"

hatched


##add AgeDay1
  #has to be specific per year/site so easier to add in here
age <- hatched
age %>% group_by(Year) %>% 
  summarise(min_Found = min(Found, na.rm = TRUE))
age <- age %>% group_by(Year) %>% 
  mutate(AgeDay1 = min(Found, na.rm = TRUE) - init) %>%
  ungroup()


INP_2019_2026 <- age
write.csv(INP_2019_2026, "C:/Users/shoepfner/Desktop/nest survival/nest_survival_2022-2025/INP_2019-2026.csv")



####UTQ SPEI####
visit_UTQ <- read_csv("C:/Users/shoepfner/Documents/eiders_kigigak_2026/nest_survival_1994-2026/UTQ/attributes_compiled_2026_09_03.csv")

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
  mutate(age = pmax(float_age1, float_age2, na.rm = TRUE))
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


#nests that failed take the midpoint between visits
  #but some visits were after the estimate hatch date




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


#compare fate dates to the 22 camera nests during this period
  #but not using the actual dates from the cameras, just see if really off
camera <- checks
camera <- camera %>% 
  mutate(check.fate.date = as.Date(rowMeans(across(c(dt_lastCheck, dt_lastAlive), as.numeric)), origin = "1970-01-01"))
cams <- read_excel("~/eiders_kigigak_2026/nest_survival_1994-2026/UTQ/Copy of camera nest tables for Sarah 20260924.xlsx")
cams <- cams %>%  filter(grepl("^2", Nest_ID)) #only keep cameras from years 22-26
cams <- cams %>%  filter(Species=="SPEI") #23 but really 22 nests because one camera didn't work well
cams <- cams %>% rename(nest=Nest_ID)
cams$DateTime_Fate <- as.Date(cams$DateTime_Fate)

comparison <- camera %>%
  inner_join(cams, by = "nest", suffix = c("_camera", "_cams")) %>%
  mutate(date_diff = check.fate.date - DateTime_Fate)
#remove two nests that had camera errors
comparison <- comparison %>% filter(!nest %in% c("23-DRS009","26-HMG041"))
mean(comparison$date_diff, na.rm = TRUE) # -1 days
min(comparison$date_diff, na.rm = TRUE) # -5.5
max(comparison$date_diff, na.rm = TRUE) # 3.5
sd(comparison$date_diff, na.rm = TRUE) # 2.486

comparison$date_diff <- as.numeric(comparison$date_diff)
hist(comparison$date_diff)



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
  mutate(AgeDay1 = min(FirstFound, na.rm = TRUE) - init) %>%
  ungroup()


write.csv(age1, "C:/Users/shoepfner/Documents/eiders_kigigak_2026/nest_survival_1994-2026/UTQ/INP_UTQ_2022-2026.csv")




####combine all three INP files####
  #at this step now must combine with Randall's INP file for 1994-2021
  #only use 2022-2026 Kig from the above work - use Randall's work for 2019 and 2022
  #use 2022-2026 for Utq also, those are the only new years since Randall
nest_inp_RJF <- read_csv("C:/Users/shoepfner/Desktop/nest survival/Randall's work/INP_20220921.csv")
kig <- read_csv("C:/Users/shoepfner/Desktop/nest survival/nest_survival_2022-2025/INP_2019-2026.csv")
utq <- read_csv("C:/Users/shoepfner/Documents/eiders_kigigak_2026/nest_survival_1994-2026/UTQ/INP_UTQ_2022-2026.csv")
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
#convert date back to Julian so all three files can have the same julian dates and convert to earliest
  #earliest Julian day was 134 so had to subtract 133 to get day 1, add 133 back now
# nest_inp_RJF$LastChecked <- nest_inp_RJF$LastChecked + 133
# nest_inp_RJF$LastPresent <- nest_inp_RJF$LastPresent + 133
# nest_inp_RJF$FirstFound <- nest_inp_RJF$FirstFound + 133

#try and correct it so not the annual value but earliest for the whole study, probably still 133?
  #earliest first found dates for each year, have to add back to all four date columns
    #1994=, 1995=, 1996=, 1997=, 1998=, 1999=, 2000=, 2001=, 2002=, 2003=, 2004=,
    #2005=, 2006=, 
test <- nest_inp_RJF %>%
  if_else(ht_data$Year==2019, 48,
          if_else(ht_data$Year==2009,31,
                  if_else(ht_data$Year==2010,34,
                          if_else(ht_data$Year==2011,33,
                                  if_else(ht_data$Year==2012,48,
                                          if_else(ht_data$Year==2013,35,
                                                  if_else(ht_data$Year==2014,42)))))))


kig <- kig %>% 
  select(-ID_tblNest, -id_nest, -dt_Hatch, -notes_nest, -dt_visit, -cat_nestStatus, 
         -maxAge, - estHatch, -daysToHatch, -Hatch_Date, -"...1") %>%
  rename(FirstFound=Found, LastPresent=LastPres, LastChecked=LastCheck) %>% 
  mutate(Freq = 1)
kig$FirstFound <- as.numeric(kig$FirstFound)
kig$Fate <- as.numeric(kig$Fate)

#standardize for each year to the earliest date, to correct for different methods!!!
kig <- kig %>% group_by(Year) %>%
  group_by(Year) %>%
  mutate(across(c(FirstFound, LastPresent, LastChecked), 
                ~ . - min(FirstFound-1, na.rm = TRUE))) %>%
  ungroup()



  

#both Randall's file and my Kig file have the years 2019 and 2021, remove mine
kig <- kig %>% filter(!(Year %in% c(2019,2021)))



utq <- utq %>% select(-1)  %>% 
  mutate(Freq = 1)
utq <- utq %>% group_by(Year) %>%
  group_by(Year) %>%
  mutate(across(c(FirstFound, LastPresent, LastChecked), 
                ~ . - min(FirstFound-1, na.rm = TRUE))) %>%
  ungroup()

#Randall= 2870, kig later=231, utq later=53 nests


###combine the three files together 
combined <- bind_rows(nest_inp_RJF, kig, utq)  #3154 nests total

#earliest day FirstFound is 134, subtract 133 from columns
  #so same as what I had done previously and had undone to Randall's file
# combined$FirstFound <- combined$FirstFound - 133
# combined$LastPresent <- combined$LastPresent - 133
# combined$LastChecked <- combined$LastChecked - 133



## AgeDay1 !!! ##
  #age of the nest on day 1 of the study
  #Randall's code did this on the annual/site scale
    #his INP file already has these calculated at this level
    #I also did this for the UTG and Kig data and this is what I used in the memo report
      #the differences are minimal, the year*site interaction accounts for the consistent year-site variation/error
#BUT AgeDay1 should really be this line of code below, init date - 133 (first day of whole study)
  #this "real" AgeDay1 will need to be run for the nest density analysis
# combined$AgeDay1 <- combined$init - 133


##CENTER initiation date
  #centering Init before modeling, mean per year
  #doesn't change inference but helps convergence and keeps the intercept interpretable 
  #DSR at the mean date/age rather than at Init=0, which is meaningless here
init_c <- combined %>%
  group_by(Year, Site) %>%
  summarise(Init_year_site = mean(init, na.rm = TRUE)) 
init_c

inp <- combined %>%
  left_join(init_c, by = c("Year","Site"))

inp <- inp %>% 
  mutate(Init_center = Init_year_site-init)


#save the final INP file so don't have to always run the above code
write.csv(inp, "C:/Users/shoepfner/Documents/eiders_kigigak_2026/nest_survival_1994-2026/inp_ns_models_1994-2026_ssa_revision_3.csv")



checks <- inp %>% group_by(Year, Site) %>%
  summarise(mean=mean(AgeDay1))
checks1 <- inp %>% group_by(Year, Site) %>%
  summarise(mean=mean(init))




####start modeling####
# Add path to MARK.exe
Sys.setenv(MarkPath = "C:/Program Files (x86)/MARK")
mark.path <- "C:/Program Files (x86)/MARK"
options(mark.path = mark.path)
data("mallard")


# Load the SPEI data INP file
# spei_data<- read.csv("nest_survival/data/INP_20220921.csv", header = TRUE)
spei_data <- inp
spei_data <- read_csv("C:/Users/shoepfner/Documents/eiders_kigigak_2026/nest_survival_1994-2026/inp_ns_models_1994-2026_ssa_revision_2.csv")



# Check the headers. Sometimes column name changes when reading csv files. 
head(spei_data)
summary(spei_data)
names(spei_data)

# Make year and site factor variables
spei_data$Year <-as.factor(spei_data$Year)
spei_data$Site <-as.factor(spei_data$Site)
str(spei_data)
spei_data <- as.data.frame(spei_data)

setwd("C:/Users/shoepfner/Documents/eiders_kigigak_2026/nest_survival_1994-2026")

spei_data %>% filter (Year == 2019) %>%
  filter(Site == "kig") %>% 
  summarise(n=n()) 
#164 nests that I included in 2019 nest survival, Randall included 165, should be 165!!


###############################################################################

# Fit nest survival models by taking away the intercept and use initial values
# Information: From year 1994 to 2021 the earliest julian date is 134 and latest is 206
# To get the Number of Occasions (NOCC), a.k.a duration of the study, the season length
# will be the difference of the latest and earliest. 206-134= 72
    #but the code didn't like that so ran 73 so it counts both the first found and last checked days
# code from Friendly et al. (2025)

###############################################################################

run.models_1 = function()
{
  # 1. constant daily survival rate model (null)
  # S.dot = mark(spei_data, nocc = 73, model = "Nest", model.name = "S.dot",
  #              # ddl= nest.ddl,
  #              model.parameters = list(S=list(formula = ~ 1)))
  # 
  # # 2. year + site
  # S.sy = mark(spei_data, nocc = 73, model = "Nest", model.name = "S.sy", groups = c("Site", "Year"),
  #             # ddl= nest.ddl,
  #             model.parameters = list(S=list(formula = ~ Year + Site)))
  # 
  # # 3. year + site + init
  # S.syi = mark(spei_data, nocc = 73, model = "Nest", model.name = "S.syi", groups = c("Site", "Year"),
  #              # ddl= nest.ddl,
  #              model.parameters = list(S=list(formula = ~ Year + Site + init)),
  #              initial = S.sy)
  # 
  # # 4. year + site + nestage
  # S.syn = mark(spei_data, nocc = 73, model = "Nest", model.name = "S.syn", groups = c("Site", "Year"),
  #              # ddl= nest.ddl,
  #              model.parameters = list(S=list(formula = ~ Year + Site + NestAge)),
  #              initial = S.sy)
  # 
  # # 5. year + site + init + nestage
  # S.syin = mark(spei_data, nocc = 73, model = "Nest", model.name = "S.syin", groups = c("Site", "Year"),
  #               # ddl= nest.ddl,
  #               model.parameters = list(S=list(formula = ~ Year + Site + init + NestAge)))
  
  # 6. year * site + init + nestage
  S.syin.inter = mark(spei_data, nocc = 73, model = "Nest", model.name = "S.syin.inter", 
                      groups = c("Site", "Year"),
                      model.parameters = list(S=list(formula = ~ Site*Year)))

  # Return model table and list of models

  return(collect.models())
}

# Fit models
model.results_2=run.models_1()

# Look at model results
model.results_2


# Write the results output into a csv file
model_output_1 <- model.results_1$model.table
write.csv(model_output_1, "C:/Users/shoepfner/Documents/eiders_kigigak_2026/nest_survival_1994-2026/model_estimates.csv")
write.csv(top_model_2$results$beta, "C:/Users/shoepfner/Documents/eiders_kigigak_2026/nest_survival_1994-2026/model_estimates.csv", row.names = TRUE)
model_output_1 <- read.csv("C:/Users/shoepfner/Documents/eiders_kigigak_2026/nest_survival_1994-2026/model_estimates.csv")
top_model_2$results$beta <- read.csv("C:/Users/shoepfner/Documents/eiders_kigigak_2026/nest_survival_1994-2026/model_estimates.csv")


model.results_1$S.sy$results$beta
model.results_1$S.syn$results$beta
model.results_2$S.syin.inter$results$beta
model.results_1$S.syin.inter$results$beta
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
top_model_2 <- model.results_1$S.syin.inter   #USE this one!top model and includes covariates we care about
top_model_3 <- model.results_1$S.sy
ffc2 <- find.covariates(top_model_2, spei_data)


# Build a design matrix for nestages 1:24, which is my assumed incubation period until
# success. The following code will assign nest ages 1:24 for nestage for each year/site.
# When doing this, be careful and make sure the year and site are corresponding to on another
# the design matrix. Or else your estimates will be mixed.

ffc2$value[3169:3192] <- 1:24 #1994
ffc2$value[3241:3264] <- 1:24 #1995
ffc2$value[3313:3336] <- 1:24 #1996
ffc2$value[3385:3408] <- 1:24 #1997
ffc2$value[3457:3480] <- 1:24 #1998
ffc2$value[3529:3552] <- 1:24 #1999
ffc2$value[3601:3624] <- 1:24 #2000
ffc2$value[3673:3696] <- 1:24 #2001
ffc2$value[3745:3768] <- 1:24 #2002
ffc2$value[3817:3840] <- 1:24 #2003
ffc2$value[3889:3912] <- 1:24 #2004
ffc2$value[3961:3984] <- 1:24 #2005
ffc2$value[4033:4056] <- 1:24 #2006
ffc2$value[4105:4128] <- 1:24 #2007
ffc2$value[4177:4200] <- 1:24 #2008
ffc2$value[4249:4272] <- 1:24 #2009
ffc2$value[4321:4344] <- 1:24 #2010
ffc2$value[4393:4416] <- 1:24 #2010 - U
ffc2$value[4465:4488] <- 1:24 #2011
ffc2$value[4537:4560] <- 1:24 #2011 - U
ffc2$value[4609:4632] <- 1:24 #2012
ffc2$value[4681:4704] <- 1:24 #2012 - U
ffc2$value[4753:4776] <- 1:24 #2013
ffc2$value[4825:4848] <- 1:24 #2013 - U
ffc2$value[4897:4920] <- 1:24 #2014
ffc2$value[4969:4992] <- 1:24 #2014 - U
ffc2$value[5041:5064] <- 1:24 #2015
ffc2$value[5113:5136] <- 1:24 #2015 - U
ffc2$value[5185:5208] <- 1:24 #2016 - U
ffc2$value[5257:5280] <- 1:24 #2017 - U
ffc2$value[5329:5352] <- 1:24 #2018 - U
ffc2$value[5401:5424] <- 1:24 #2019
ffc2$value[5473:5496] <- 1:24 #2019 - U
ffc2$value[5545:5568] <- 1:24 #2021
ffc2$value[5617:5640] <- 1:24 #2022
ffc2$value[5689:5712] <- 1:24 #2022 - U
ffc2$value[5761:5784] <- 1:24 #2023
ffc2$value[5833:5856] <- 1:24 #2023 - U
ffc2$value[5905:5928] <- 1:24 #2024
ffc2$value[5977:6000] <- 1:24 #2024 - U
ffc2$value[6049:6072] <- 1:24 #2025
ffc2$value[6121:6144] <- 1:24 #2025 - U
ffc2$value[6193:6216] <- 1:24 #2026
ffc2$value[6265:6288] <- 1:24 #2026 - U



##use the Julian date of the annual mean
spei_data <- read_csv("C:/Users/shoepfner/Documents/eiders_kigigak_2026/nest_survival_1994-2026/inp_ns_models_1994-2026_ssa_revision_2.csv")
inits <- spei_data %>% group_by(Site, Year) %>% 
  summarise(mean=mean(init))
inits$mean <- format(round(inits$mean, 4), nsmall = 4)
print(inits, n=Inf)

ffc2$value[1:72] <- seq(145.9231, length = 1) #1994
ffc2$value[73:144] <- seq(147.6907, length = 1) #1995
ffc2$value[145:216] <- seq(143.4035, length = 1) #1996
ffc2$value[217:288] <- seq(143.6014, length = 1) #1997
ffc2$value[289:360] <- seq(154.1308, length = 1) #1998
ffc2$value[361:432] <- seq(156.3071, length = 1) #1999
ffc2$value[433:504] <- seq(154.3534, length = 1) #2000
ffc2$value[505:576] <- seq(155.8571, length = 1) #2001
ffc2$value[577:648] <- seq(145.8333, length = 1) #2002
ffc2$value[649:720] <- seq(147.5902, length = 1) #2003
ffc2$value[721:792] <- seq(143.7887, length = 1) #2004
ffc2$value[793:864] <- seq(144.0146, length = 1) #2005
ffc2$value[865:936] <- seq(154.9940, length = 1) #2006
ffc2$value[937:1008] <- seq(148.0578, length = 1) #2007
ffc2$value[1009:1080] <- seq(151.0299, length = 1) #2008
ffc2$value[1081:1152] <- seq(149.4946, length = 1) #2009
ffc2$value[1153:1224] <- seq(150.7353, length = 1) #2010
ffc2$value[1225:1296] <- seq(172.1333, length = 1) #2010 - U
ffc2$value[1297:1368] <- seq(149.1852, length = 1) #2011
ffc2$value[1369:1440] <- seq(169.3125, length = 1) #2011 - U
ffc2$value[1441:1512] <- seq(160.5364, length = 1) #2012
ffc2$value[1513:1584] <- seq(168.8000, length = 1) #2012 - U
ffc2$value[1585:1656] <- seq(156.9444, length = 1) #2013
ffc2$value[1657:1728] <- seq(167.3333, length = 1) #2013 - U
ffc2$value[1729:1800] <- seq(142.9643, length = 1) #2014 
ffc2$value[1801:1872] <- seq(169.9375, length = 1) #2014 - U
ffc2$value[1873:1944] <- seq(149.7647, length = 1) #2015
ffc2$value[1945:2016] <- seq(162.5000, length = 1) #2015 - U
ffc2$value[2017:2088] <- seq(163.1667, length = 1) #2016 - U
ffc2$value[2089:2160] <- seq(175.5000, length = 1) #2017 - U 
ffc2$value[2161:2232] <- seq(180.8667, length = 1) #2018 - U
ffc2$value[2233:2304] <- seq(141.8788, length = 1) #2019
ffc2$value[2305:2376] <- seq(168.7273, length = 1) #2019 - U
ffc2$value[2377:2448] <- seq(146.9216, length = 1) #2021
ffc2$value[2449:2520] <- seq(148.8421, length = 1) #2022
ffc2$value[2521:2592] <- seq(176.6250, length = 1) #2022 - U
ffc2$value[2593:2664] <- seq(148.4444, length = 1) #2023
ffc2$value[2665:2736] <- seq(165.5882, length = 1) #2023 - U
ffc2$value[2737:2808] <- seq(154.0517, length = 1) #2024
ffc2$value[2809:2880] <- seq(173.3333, length = 1) #2024 - U
ffc2$value[2881:2952] <- seq(145.0000, length = 1) #2025
ffc2$value[2953:3024] <- seq(174.0000, length = 1) #2025 - U
ffc2$value[3025:3096] <- seq(146.8125, length = 1) #2026 
ffc2$value[3097:3168] <- seq(175.5714, length = 1) #2026 - U

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
s1995 <- deltamethod.special("prod",ffull.survival$real[73:96],ffull.survival$vcv.real[73:96,73:96])
s1996 <- deltamethod.special("prod",ffull.survival$real[145:168],ffull.survival$vcv.real[145:168,145:168])
s1997 <- deltamethod.special("prod",ffull.survival$real[217:240],ffull.survival$vcv.real[217:240,217:240])
s1998 <- deltamethod.special("prod",ffull.survival$real[289:312],ffull.survival$vcv.real[289:312,289:312])
s1999 <- deltamethod.special("prod",ffull.survival$real[361:384],ffull.survival$vcv.real[361:384,361:384])
s2000 <- deltamethod.special("prod",ffull.survival$real[433:456],ffull.survival$vcv.real[433:456,433:456])
s2001 <- deltamethod.special("prod",ffull.survival$real[505:528],ffull.survival$vcv.real[505:528,505:528])
s2002 <- deltamethod.special("prod",ffull.survival$real[577:600],ffull.survival$vcv.real[577:600,577:600])
s2003 <- deltamethod.special("prod",ffull.survival$real[649:672],ffull.survival$vcv.real[649:672,649:672])
s2004 <- deltamethod.special("prod",ffull.survival$real[721:744],ffull.survival$vcv.real[721:744,721:744])
s2005 <- deltamethod.special("prod",ffull.survival$real[793:816],ffull.survival$vcv.real[793:816,793:816])
s2006 <- deltamethod.special("prod",ffull.survival$real[865:888],ffull.survival$vcv.real[865:888,865:888])
s2007 <- deltamethod.special("prod",ffull.survival$real[937:960],ffull.survival$vcv.real[937:960,937:960])
s2008 <- deltamethod.special("prod",ffull.survival$real[1009:1032],ffull.survival$vcv.real[1009:1032,1009:1032])
s2009 <- deltamethod.special("prod",ffull.survival$real[1081:1104],ffull.survival$vcv.real[1081:1104,1081:1104])
s2010 <- deltamethod.special("prod",ffull.survival$real[1153:1176],ffull.survival$vcv.real[1153:1176,1153:1176])
s2010u <- deltamethod.special("prod",ffull.survival$real[1225:1248],ffull.survival$vcv.real[1225:1248,1225:1248])
s2011 <-  deltamethod.special("prod",ffull.survival$real[1297:1320],ffull.survival$vcv.real[1297:1320,1297:1320])
s2011u <- deltamethod.special("prod",ffull.survival$real[1369:1392],ffull.survival$vcv.real[1369:1392,1369:1392])
s2012 <-  deltamethod.special("prod",ffull.survival$real[1441:1464],ffull.survival$vcv.real[1441:1464,1441:1464])
s2012u <- deltamethod.special("prod",ffull.survival$real[1513:1536],ffull.survival$vcv.real[1513:1536,1513:1536])
s2013 <-  deltamethod.special("prod",ffull.survival$real[1585:1608],ffull.survival$vcv.real[1585:1608,1585:1608])
s2013u <- deltamethod.special("prod",ffull.survival$real[1657:1680],ffull.survival$vcv.real[1657:1680,1657:1680])
s2014 <-  deltamethod.special("prod",ffull.survival$real[1729:1752],ffull.survival$vcv.real[1729:1752,1729:1752])
s2014u <- deltamethod.special("prod",ffull.survival$real[1801:1824],ffull.survival$vcv.real[1801:1824,1801:1824])
s2015 <-  deltamethod.special("prod",ffull.survival$real[1873:1896],ffull.survival$vcv.real[1873:1896,1873:1896])
s2015u <- deltamethod.special("prod",ffull.survival$real[1945:1968],ffull.survival$vcv.real[1945:1968,1945:1968])
s2016u <- deltamethod.special("prod",ffull.survival$real[2017:2040],ffull.survival$vcv.real[2017:2040,2017:2040])
s2017u <- deltamethod.special("prod",ffull.survival$real[2089:2112],ffull.survival$vcv.real[2089:2112,2089:2112])
s2018u <- deltamethod.special("prod",ffull.survival$real[2161:2184],ffull.survival$vcv.real[2161:2184,2161:2184])
s2019 <-  deltamethod.special("prod",ffull.survival$real[2233:2256],ffull.survival$vcv.real[2233:2256,2233:2256])
s2019u <- deltamethod.special("prod",ffull.survival$real[2305:2328],ffull.survival$vcv.real[2305:2328,2305:2328])
s2021 <-  deltamethod.special("prod",ffull.survival$real[2377:2400],ffull.survival$vcv.real[2377:2400,2377:2400])
s2022 <-  deltamethod.special("prod",ffull.survival$real[2449:2472],ffull.survival$vcv.real[2449:2472,2449:2472])
s2022u <- deltamethod.special("prod",ffull.survival$real[2521:2544],ffull.survival$vcv.real[2521:2544,2521:2544])
s2023 <-  deltamethod.special("prod",ffull.survival$real[2593:2616],ffull.survival$vcv.real[2593:2616,2593:2616])
s2023u <- deltamethod.special("prod",ffull.survival$real[2665:2688],ffull.survival$vcv.real[2665:2688,2665:2688])
s2024 <-  deltamethod.special("prod",ffull.survival$real[2737:2760],ffull.survival$vcv.real[2737:2760,2737:2760])
s2024u <- deltamethod.special("prod",ffull.survival$real[2809:2832],ffull.survival$vcv.real[2809:2832,2809:2832])
s2025 <-  deltamethod.special("prod",ffull.survival$real[2881:2904],ffull.survival$vcv.real[2881:2904,2881:2904])
s2025u <- deltamethod.special("prod",ffull.survival$real[2953:2976],ffull.survival$vcv.real[2953:2976,2953:2976])
s2026 <-  deltamethod.special("prod",ffull.survival$real[3025:3048],ffull.survival$vcv.real[3025:3048,3025:3048])
s2026u <- deltamethod.special("prod",ffull.survival$real[3097:3120],ffull.survival$vcv.real[3097:3120,3097:3120])


# Obtain the DSR estimates
survival.24 <- compute.real(top_model_2, design = fdesign2)[c(1:24,
                                                             73:96,
                                                             145:168,
                                                             217:240,
                                                             289:312,
                                                             361:384,
                                                             433:456,
                                                             505:528,
                                                             577:600,
                                                             649:672,
                                                             721:744,
                                                             793:816,
                                                             865:888,
                                                             937:960,
                                                             1009:1032,
                                                             1081:1104,
                                                             1153:1176,
                                                             1225:1248,
                                                             1297:1320,
                                                             1369:1392,
                                                             1441:1464,
                                                             1513:1536,
                                                             1585:1608,
                                                             1657:1680,
                                                             1729:1752,
                                                             1801:1824,
                                                             1873:1896,
                                                             1945:1968,
                                                             2017:2040,
                                                             2089:2112,
                                                             2161:2184,
                                                             2233:2256,
                                                             2305:2328,
                                                             2377:2400,
                                                             2449:2472,
                                                             2521:2544,
                                                             2593:2616,
                                                             2665:2688,
                                                             2737:2760,
                                                             2809:2832,
                                                             2881:2904,
                                                             2953:2976,
                                                             3025:3048,
                                                             3097:3120), ]

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
product2010u <- prod(survival.24$estimate[409:432]) 
product2011 <- prod(survival.24$estimate[433:456]) 
product2011u <- prod(survival.24$estimate[457:480]) 
product2012 <- prod(survival.24$estimate[481:504])
product2012u <- prod(survival.24$estimate[505:528])
product2013 <- prod(survival.24$estimate[529:552]) 
product2013u <- prod(survival.24$estimate[553:576]) 
product2014 <- prod(survival.24$estimate[577:600]) 
product2014u <- prod(survival.24$estimate[601:624]) 
product2015 <- prod(survival.24$estimate[625:648]) 
product2015u <- prod(survival.24$estimate[649:672])
product2016u <- prod(survival.24$estimate[673:696])
product2017u <- prod(survival.24$estimate[697:720]) 
product2018u <- prod(survival.24$estimate[721:744]) 
product2019 <- prod(survival.24$estimate[745:768])
product2019u <- prod(survival.24$estimate[769:792])
product2021 <- prod(survival.24$estimate[793:816]) 
product2022 <- prod(survival.24$estimate[817:840]) 
product2022u <- prod(survival.24$estimate[841:864]) 
product2023 <- prod(survival.24$estimate[865:888]) 
product2023u <- prod(survival.24$estimate[889:912]) 
product2024 <- prod(survival.24$estimate[913:936])
product2024u <- prod(survival.24$estimate[937:960])
product2025 <- prod(survival.24$estimate[961:984]) 
product2025u <- prod(survival.24$estimate[985:1008]) 
product2026 <- prod(survival.24$estimate[1009:1032])
product2026u <- prod(survival.24$estimate[1033:1056])

# Create a dataframe from the products of the 24 nestage estimates above.
probsuccess <- c(product1994,product1995,product1996,product1997,product1998,product1999,product2000,
                 product2001,product2002,product2003,product2004,product2005,product2006,product2007,
                 product2008,product2009,product2010,product2010u,product2011,product2011u,product2012,
                 product2012u,product2013,product2013u,product2014,product2014u,product2015,product2015u,
                 product2016u,product2017u,product2018u,product2019,product2019u,product2021,product2022,
                 product2022u,product2023,product2023u,product2024,product2024u,product2025,product2025u,
                 product2026,product2026u)
probsuccess <- as.data.frame(probsuccess)

# Create a dataframe for the standard errors for year and site.
#se not using the delta method, just the compute.real function
se <- c(s1994,s1995,s1996,s1997,s1998,s1999,s2000,s2001,s2002,s2003,
        s2004,s2005,s2006,s2007,s2008,s2009,s2010,s2010u,s2011,s2011u,
        s2012,s2012u,s2013,s2013u,s2014,s2014u,s2015,s2015u,s2016u,
        s2017u,s2018u,s2019,s2019u,s2021,s2022,s2022u,s2023,s2023u,
        s2024,s2024u,s2025,s2025u,s2026,s2026u)
se <- as.data.frame(se)

# Create dataframe for the 95% confidence intervals
lci <- probsuccess - 1.96*(se)
lci <- as.data.frame(lci)
colnames(lci)[colnames(lci) == "probsuccess"] <- "lci"

uci <- probsuccess + 1.96*(se)
uci <- as.data.frame(uci)
colnames(uci)[colnames(uci) == "probsuccess"] <- "uci"


#convert real-scale estimate and SE to logit scale
# Convert real-scale estimate and SE to the logit scale
logit_est <- qlogis(probsuccess$probsuccess)
logit_se  <- se$se / (probsuccess$probsuccess * (1 - probsuccess$probsuccess))

# Compute CI on logit scale
logit_lci <- logit_est - 1.96 * logit_se
logit_uci <- logit_est + 1.96 * logit_se

# Back-transform to real scale (bounded 0-1)
lci <- as.data.frame(plogis(logit_lci))
uci <- as.data.frame(plogis(logit_uci))
colnames(lci) <- "lci"
colnames(uci) <- "uci"


# Combine 'probsuccess' and 'lci' and 'uci'.
data <- cbind(probsuccess,lci,uci)

data$year <- c("1994","1995","1996","1997","1998","1999","2000","2001","2002","2003",
               "2004","2005","2006","2007","2008","2009","2010","2010","2011","2011",
               "2012","2012","2013","2013","2014","2014","2015","2015","2016","2017",
               "2018","2019","2019","2021","2022","2022","2023","2023","2024","2024",
               "2025","2025","2026","2026")

data$site <- c("kig","kig","kig","kig","kig","kig","kig","kig","kig","kig",
               "kig","kig","kig","kig","kig","kig","kig","utq","kig","utq",
               "kig","utq","kig","utq","kig","utq","kig","utq","utq","utq",
               "utq","kig","utq","kig","kig","utq","kig","utq","kig","utq",
               "kig","utq","kig","utq")

data$probsuccess<-as.numeric(data$probsuccess) #in case it is not recognized as numerical values

# save new data frame as csv to not need to run model set 1 again
annual_ns_estimates <- data
annual_ns_estimates

sample_sizes <- spei_data %>%
  group_by(Year, Site) %>%
  summarise(n = n(), .groups = "drop") %>%
  rename(year=Year, site=Site)
sample_sizes$year <- as.character(sample_sizes$year)

annual_ns_estimates <- annual_ns_estimates %>%
  left_join(sample_sizes, by = c("year", "site"))


#save results so you don't have to run the model each time
save(top_model_2, fdesign2, ffull.survival, 
     file = "nest_survival_top_model_2_results.RData")
ffull.survival2 <- compute.real(top_model_2, design = fdesign2, vcv = TRUE)
saveRDS(ffull.survival2, file = "ffull_survival2.rds")
saveRDS(top_model_2, file = "top_model_2.rds")
load("nest_survival_top_model_2_results.RData")

write.csv(annual_ns_estimates, "C:/Users/shoepfner/Documents/eiders_kigigak_2026/nest_survival_1994-2026/estimates_1994-2026_ssa_revision_2.csv")
annual_ns_estimates <- read.csv("C:/Users/shoepfner/Documents/eiders_kigigak_2026/nest_survival_1994-2026/estimates_1994-2026_ssa_revision_2.csv", header = TRUE)
summary(annual_ns_estimates)
annual_ns_estimates$Year <- as.factor(annual_ns_estimates$year)




#get annual estimates 2022-2026 for each site
ffull.survival

# Pull the row labels straight from the model's own design data (guaranteed aligned)
row_labels <- top_model_2$design.data$S

years_wanted <- 2022:2026
sites_wanted <- c("kig", "utq")
target_groups <- expand.grid(Year = as.character(years_wanted), 
                             Site = sites_wanted, 
                             stringsAsFactors = FALSE)

results_list <- list()

for (i in seq_len(nrow(target_groups))) {
  yr <- target_groups$Year[i]
  st <- target_groups$Site[i]
  
  # Find rows matching this year/site, for nest ages 1 through 24
  rows <- which(row_labels$Year == yr & row_labels$Site == st & row_labels$Age %in% 1:24)
  
  if (length(rows) != 24) {
    warning(paste("Expected 24 rows for", yr, st, "but found", length(rows)))
    next
  }
  
  est <- prod(ffull.survival$real[rows])
  se  <- deltamethod.special("prod", ffull.survival$real[rows], ffull.survival$vcv.real[rows, rows])
  
  logit_est <- qlogis(est)
  logit_se  <- se / (est * (1 - est))
  lci <- plogis(logit_est - 1.96 * logit_se)
  uci <- plogis(logit_est + 1.96 * logit_se)
  
  results_list[[i]] <- data.frame(Year = yr, Site = st, probsuccess = est, se = se, lci = lci, uci = uci)
}

final_results <- bind_rows(results_list)
print(final_results)


#get 2022-2026 model averaged estimates
pooled_table <- annual_ns_estimates %>%
  # filter(year %in% 2022:2026) %>%      #comment out this to get the overall 1994-2026 mean for each site
  mutate(logit_est = qlogis(probsuccess),
    logit_se  = se / (probsuccess * (1 - probsuccess))) %>%
  group_by(site) %>%
  summarise(
    weighted_logit_mean = sum(logit_est / logit_se^2) / sum(1 / logit_se^2),
    pooled_se = sqrt(1 / sum(1 / logit_se^2))) %>%
  mutate(mean_survival = plogis(weighted_logit_mean),
    lci = plogis(weighted_logit_mean - 1.96 * pooled_se),
    uci = plogis(weighted_logit_mean + 1.96 * pooled_se))

print(pooled_table)







#pull out just 2022-2026 for the memo
short <- annual_ns_estimates %>% filter(year %in% c(2022, 2023, 2024, 2025, 2026))
short <- annual_ns_estimates


short
short$year <- as.factor(short$year)
short$site <- as.factor(short$site)
str(short)


# Plot
pd <- position_dodge(width = 0.5)
kig_mean <- mean(data$probsuccess[data$site == "kig"], na.rm = TRUE)
utq_mean <- mean(data$probsuccess[data$site == "utq"], na.rm = TRUE)
ggplot(short, aes(x = year, y = probsuccess, color=site, group=site)) +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.13) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4, position = pd) +
  geom_point(position = pd) +
  # geom_text(aes(y = uci, label = n), 
  #           position = pd, 
  #           vjust = -0.5, 
  #           size = 4,
  #           show.legend = FALSE, color = "black") +
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



#plot just Kig data
ggplot(short %>% filter(site == "kig"), aes(x = year, y = probsuccess, color=site, group=site)) +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.13) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4, position = pd, color="black") +
  geom_point(position = pd, color="black") +
  geom_text(aes(y = uci, label = n), 
            position = pd, 
            vjust = -0.5, 
            size = 3,
            show.legend = FALSE, color = "black") +
  geom_hline(yintercept = kig_mean, color = "black", lty = "dashed") +
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
ggplot(short %>% filter(site == "utq"), aes(x = year, y = probsuccess, color=site, group=site)) +
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



#plot with Randall's old means
ggplot(short, aes(x = year, y = probsuccess, color=site, group=site)) +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.13) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.4, position = pd) +
  geom_point(position = pd) +
  geom_text(aes(y = uci, label = n), 
            position = pd, 
            vjust = -0.5, 
            size = 3.5,
            show.legend = FALSE, color = "black") +
  annotate("segment", x = "1994", xend = "2021", y = 0.78,
               color = "red", linetype = "dotdash", linewidth=0.3) +  #Randall's Kig mean
  annotate("segment", x = "2010", xend = "2019", y = 0.67,
               color = "blue", linetype = "dotdash", linewidth=0.1) +   #Randall's Utq mean
  geom_hline(yintercept = kig_mean, color = "red", lty = "dashed") +
  geom_hline(yintercept = utq_mean, color = "blue", lty = "dashed") +
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
  scale_color_brewer(palette = "Set1", labels = c("kig" = "Kigigak", "utq" = "Utqiagvik"))




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
  #nests in 2021 and 12 in 2024 did not have columns n_eggsTotal and n_eggsViable not entered so I entered them 
      #but no longer relevant now since those years aren't being included
#first is Kig data and then UTQ data below

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
            aes(x = factor(year), y = max_pct + 5, label = paste0("n = ", total_n)),
            inherit.aes = FALSE, size = 3.5) +
  labs(x = "Year", y = "Percentage (%)", fill = "Clutch size") +
  theme_bw() 







###UTQ data
utq_eggs <- read_csv("C:/Users/shoepfner/Documents/eiders_kigigak_2026/nest_survival_1994-2026/UTQ/attributes_compiled_2026_09_03.csv")

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
            aes(x = factor(year), y = max_pct + 5, label = paste0("n = ", total_n)),
            inherit.aes = FALSE, size = 3.5) +
  labs(x = "Year", y = "Percentage (%)", fill = "Clutch size") +
  theme_bw() 



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


