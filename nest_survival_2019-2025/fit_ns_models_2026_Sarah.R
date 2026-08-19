#Summary of Reproductive Output of Spectacled Eiders at Kigigak Island, 2019-2025
#code and analysis by Sarah Hoepfner August 2026
  #for nest survival analyses most of the code is modified from Randall Friendly's code in Friendly et al. (2025)
  #end of the script has summary stat code for clutch sizes and membranes found in the nest

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


#data manipulation in R through Access and not making edits only in Excel
  #or skip to line 155 and load in the cleaned file
db <- "C:/Users/shoepfner/Desktop/nest survival/db_kig_nesting_waterfowl_2019-2025_compiled_20250724.accdb"
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
                          -n_feathersCollected, is_flagCollected)
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

#add column for AgeDay1 = age of the nest on study day 1, not the age of the nest when first found
  #day 1 (137) and subtract initiation date 
hatched <- hatched %>%
  mutate(AgeDay1 = 137 - init)

#DJR nests have repeats, remove them
hatched <- hatched %>% distinct(id_yrNest, .keep_all = TRUE)     #DJR001 has nests in 2019, 2023, and 2024


#convert Julian dates to days from the first monitoring day= 137
  #also change the columns names to the naming scheme for RMark
days <- hatched
days$FirstFound <- days$Found - 136
days$LastPresent <- days$LastPres - 136
days$LastChecked <- days$LastCheck - 136


##CENTER initiation date?
  #centering Init and NestAge (subtract the mean) before modeling, mean per year
    #doesn't change inference but helps convergence and keeps the intercept interpretable 
      #DSR at the mean date/age rather than at Init=0, which is meaningless here
days %>%
  group_by(Year) %>%
  summarise(Mean_Value = mean(init, na.rm = TRUE))
#2019=141, 2021=147, 2022=149, 2023=148, 2024=154, 2025=145
days <- days %>% group_by(Year) %>% 
  mutate(
    Init = if_else(Year == "2019", init-141, 
           if_else(Year == "2021", init-147, 
           if_else(Year =="2022", init-149,
           if_else(Year == "2023", init-148,
           if_else(Year == "2024", init-154,
           if_else(Year == "2-25", init-145, 0)))))))       #now has the values -22 to 19, cor
 
 
#write to excel file to filter and check nests and dates
write.csv(days, "C:/Users/shoepfner/Desktop/nest survival/nest_survival_2022-2025/INP_2026_Sarah.csv")





# Add path to MARK.exe
Sys.setenv(MarkPath = "C:/Program Files (x86)/MARK")
mark.path <- "C:/Program Files (x86)/MARK"
options(mark.path = mark.path)
data("dipper")


# Load the SPEI data INP file
# spei_data<- read.csv("nest_survival/data/INP_20220921.csv", header = TRUE)
spei_data <- read_csv("C:/Users/shoepfner/Desktop/nest survival/nest_survival_2022-2025/INP_2026_Sarah.csv")


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


# #add in function to calculate nestage correctly using process.data in RMark
#   #creates a processed data file to be run into the model
# nest.proc <- process.data(spei_data, model = "Nest", groups = "Year", nocc=59)
# nest.ddl  <- make.design.data(nest.proc)


###############################################################################

# Fit nest survival models by taking away the intercept and use initial values
# Data from 2019-2025 has earliest date of 137 and oldest as 195 
# To get the Number of Occasions (NOCC), a.k.a duration of the study, the season length
# NOCC is 59 (really 136 to 195)
# code from Friendly et al. (2025)

###############################################################################

run.models_1 = function()
{
  # 1. constant daily survival rate model (null)
  S.dot = mark(spei_data, nocc = 59, model = "Nest", model.name = "S.dot",
               # ddl= nest.ddl,
               model.parameters = list(S=list(formula = ~ 1)))

  # 2. year
  S.sy = mark(spei_data, nocc = 59, model = "Nest", model.name = "S.sy", groups = c("Year"),
              # ddl= nest.ddl,
              model.parameters = list(S=list(formula = ~ Year)))

  # 3. year + init
  S.syi = mark(spei_data, nocc = 59, model = "Nest", model.name = "S.syi", groups = c("Year"),
               # ddl= nest.ddl,
               model.parameters = list(S=list(formula = ~ Year + Init)),
               initial = S.sy)

  # 4. year + nestage
  S.syn = mark(spei_data, nocc = 59, model = "Nest", model.name = "S.syn", groups = c("Year"),
               # ddl= nest.ddl,
               model.parameters = list(S=list(formula = ~ Year + NestAge)),
               initial = S.sy)

  # 5. year + init + nestage
  S.syin = mark(spei_data, nocc = 59, model = "Nest", model.name = "S.syin", groups = c("Year"),
                # ddl= nest.ddl,
                model.parameters = list(S=list(formula = ~ Year + Init + NestAge)),
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

model.results_1$S.syin$results$beta
model.results_1$S.syn$results$beta
model.results_1$S.sy$results$beta
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
ffc1 <- find.covariates(top_model_1, spei_data)
ffc2 <- find.covariates(top_model_2, spei_data)
ffc3 <- find.covariates(top_model_3, spei_data) 

#look at average initiation dates for each year
  #this is the mean for the centered average initiation dates
spei_data %>%
  group_by(Year) %>%
  summarise(Mean_Value = mean(Init, na.rm = TRUE))
#2019=0.256, 2021=-0.0196, 2022=-0.158, 2023=0.444, 2024=0.0517, 2025=0, these are the means of the centered values
#2019=141, 2021=147, 2022=149, 2023=148, 2024=154, 2025=145, these are the averages from the earlier calculated Julian date values

# Build a design matrix for nestages 1:24, which is my assumed incubation period until
# success. The following code will assign nest ages 1:24 for nestage for each year/site.
# When doing this, be careful and make sure the year and site are corresponding to on another
# the design matrix. Or else your estimates will be mixed.

ffc2$value[349:372] <- 1:24 #2019
ffc2$value[407:430] <- 1:24 #2021
ffc2$value[465:488] <- 1:24 #2022
ffc2$value[523:546] <- 1:24 #2023
ffc2$value[581:604] <- 1:24 #2024
ffc2$value[639:662] <- 1:24 #2025


# Assign appropriate initiation date values for each site
  #average initiation dates for each year, of the "centered" values
ffc2$value[1:58] <- seq(0.256, length = 1) #2019
ffc2$value[59:116] <- seq(-0.0196, length = 1) #2021
ffc2$value[117:174] <- seq(-0.158, length = 1) #2022
ffc2$value[175:232] <- seq(0.444, length = 1) #2023
ffc2$value[233:290] <- seq(0.0517, length = 1) #2024
ffc2$value[291:348] <- seq(0, length = 1) #2025


fdesign2 <- fill.covariates(top_model_2, ffc2)

# Extract the first 24 nestages for each from design matrix
  #this code just pulls out all 348 values, later the exact columns by year get pulled out
ffull.survival <- compute.real(top_model_2, design = fdesign2, vcv = TRUE) #vcv = TRUE, returns vcv instead of se
freal <- ffull.survival$real
vcv <- ffull.survival$vcv.real 


# DELTA-METHOD
# The function deltamethod.special computes delta-method standard errors.
  #uses Delta method special function and then the product of the estimates and vcv matrix for the same rows
# After you run the function, it will return the standard errors.
# To get the variance, you square that value.
# The codes below will now compute to get the standard error.
s2019 <- deltamethod.special("prod",ffull.survival$real[1:24],ffull.survival$vcv.real[1:24,1:24])
s2021 <- deltamethod.special("prod",ffull.survival$real[59:82],ffull.survival$vcv.real[59:82,59:82])
s2022 <- deltamethod.special("prod",ffull.survival$real[117:140],ffull.survival$vcv.real[117:140,117:140])
s2023 <- deltamethod.special("prod",ffull.survival$real[175:198],ffull.survival$vcv.real[175:198,175:198])
s2024 <- deltamethod.special("prod",ffull.survival$real[233:256],ffull.survival$vcv.real[233:256,233:256])
s2025 <- deltamethod.special("prod",ffull.survival$real[290:313],ffull.survival$vcv.real[290:313,290:313])
                             

# Obtain the DSR estimates
survival.24 <- compute.real(top_model_2, design = fdesign2)[c(1:24,
                                                             59:82,
                                                             117:140,
                                                             175:198,
                                                             233:256,
                                                             290:313), ]

# #Claude code 
# ss <- get.real(top_model_2, "S", se = TRUE, vcv = TRUE)
# cum.results <- data.frame(Year = character(), CumulativeSurvival = numeric(), SE = numeric())
# 
# # split row-indices by year instead of looping
# idxbyyear <- split(seq_len(nrow(ss$estimates)), ss$estimates$Year)
# 
# cum.results <- do.call(rbind, lapply(idxbyyear, function(idx) {
#   idx <- idx[order(rr$estimates$Time[idx])]
#   daily.est <- rr$estimates$estimate[idx]
#   daily.vcv <- rr$vcv.real[idx, idx]
#   data.frame(
#     CumulativeSurvival = prod(daily.est),
#     SE = deltamethod.special("prod", daily.est, daily.vcv)
#   )
# }))


# Get the product of the 24 nestage DSR estimates to achieve cumulative nest success probability.
product2019 <- prod(survival.24$estimate[1:24]) 
product2021 <- prod(survival.24$estimate[25:48]) 
product2022 <- prod(survival.24$estimate[49:72]) 
product2023 <- prod(survival.24$estimate[73:96]) 
product2024 <- prod(survival.24$estimate[97:120]) 
product2025 <- prod(survival.24$estimate[121:144])

# Create a dataframe from the products of the 24 nestage estimates above.
probsuccess <- c(product2019,product2021,product2022,product2023,product2024,product2025)
probsuccess <- as.data.frame(probsuccess)

# Create a dataframe for the standard errors for year and site.
# se <- survival.24$se  #se not using the delta method, just the compute.real function
se <- c(s2019,s2021,s2022,s2023,s2024,s2025)
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

data$year <- c("2019","2021","2022","2023","2024","2025")

data$probsuccess<-as.numeric(data$probsuccess) #in case it is not recognized as numerical values

# save new data frame as csv to not need to run model set 1 again
annual_ns_estimates <- data
annual_ns_estimates


write.csv(data, "C:/Users/shoepfner/Desktop/nest survival/nest_survival_2022-2025/annual_NS_2019-2025.csv")
annual_ns_estimates <- read.csv("C:/Users/shoepfner/Desktop/nest survival/nest_survival_2022-2025/annual_NS_2019-2025.csv", header = TRUE)
summary(annual_ns_estimates)
# annual_ns_estimates$Site <- as.factor(annual_ns_estimates$Site)
annual_ns_estimates$year <- as.factor(annual_ns_estimates$year)


# Plot
ggplot(data, aes(x = year, y = probsuccess)) +
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


# Get simple statistics
# Average nest success 
std.error(data$probsuccess[1:24]) #0.08674226
k.m.uci <- 0.788046467291436 + 1.96*0.08674226   #not sure where that first value comes from
k.m.lci <- 0.788046467291436 - 1.96*0.08674226




################################################################################
################################################################################
### This is important! You don't want to have a messy directory of Program MARK



# If you want to clean up the mark*.inp, .vcv, .res and .out
#  and .tmp files created by RMark in the working directory,
#  execute 'rm(list = ls(all = TRUE))' - see 2 lines below.
# NOTE: this will delete all objects in the R session.
rm(list = ls(all=TRUE))
# Then, execute 'cleanup(ask = FALSE)' to delete orphaned output
#  files from MARK. Execute '?cleanup' to learn more
cleanup(ask = FALSE)





####try Mayfield method####
  #just to see if results are similar, actually graphs are very close
mayfield <- data.frame(
  year = c(2019,2021,2022,2023,2024,2025),
  expo = c(2187.5, 1490, 312, 974.5, 873, 210),
  fail = c(47,17,25,22,11,13))

mayfield$year <- as.factor(mayfield$year)
mayfield$dsr <- (mayfield$expo-mayfield$fail)/mayfield$expo
mayfield$ns <- mayfield$dsr^24
mayfield$var.dsr <- ((mayfield$expo-mayfield$fail)*mayfield$fail)/(mayfield$expo^3)
mayfield$lci <- (mayfield$dsr-(1.96*sqrt(mayfield$var.dsr)))^24
mayfield$uci <- (mayfield$dsr+(1.96*sqrt(mayfield$var.dsr)))^24


# Plot
ggplot(mayfield, aes(x = year, y = ns)) +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.13) +
  geom_errorbar(aes(ymin = lci, ymax = uci, width = 0.4)) +
  geom_point() +
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






####clutch size and membrane summary stats####
  #look at clutch size of nests from 2019-2025 for Erik Osnas' IPM
  #nests in 2021 and 12 in 2024 did not have columns n_eggsTotal and n_eggsViable not entered so I entered them 

db <- "C:/Users/shoepfner/Desktop/nest survival/db_kig_nesting_waterfowl_2019-2025_compiled_20250724_clutch.accdb"
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
#253/1534 have no egg  values = 16.5%, I filled in at least one row for each nest
#years 2021, and some of 2024, had total egg numbers missing


#clutch counts

#find most "true" clutch size
  #need to find the visit that has the most eggs per nest
unique(visit$id_nest)   #477 nests
eggs <- visit %>% group_by(id_nest) %>% 
  slice_max(order_by = n_eggsTotal, n = 1, with_ties = FALSE)

#exclude nests that had less than three eggs and an early float suggesting still in lay
  #removed 44 nests
eggs_3 <- eggs %>% filter(!(n_eggsTotal <= 3 & cat_float1 == "SF"))


#number of eggs in a clutch per year
eggs_3 %>% group_by(year) %>%
  summarise(n = n())
#overall average
mean(eggs_3$n_eggsTotal)   #4.9168
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
  u[tab == max(tab)]
}
eggs_3 %>% group_by(year) %>%
  summarise(mode_value = find_mode(n_eggsTotal))


#plots
plot_clutch <- eggs_3 %>% group_by(year, n_eggsTotal) %>%
  summarise(n = n())
plot_clutch$n_eggsTotal <- as.factor(plot_clutch$n_eggsTotal)
ggplot(plot_clutch, aes(x = year, y = n, fill = n_eggsTotal)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "SPEI clutch sizes 2019-2025",
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




#membrane count

#Access shows 236 nest (visits) with membranes observed
membrane <- visit %>% group_by(id_nest) %>% 
  slice_max(order_by = n_eggMembrane, n = 1, with_ties = FALSE) %>%
  filter(!(n_eggMembrane == 0))      #get 234 here, there may have been repeat visits to nests where membranes were counted
#number of nests per year with membranes
mean(membrane$n_eggMembrane)
membrane %>% group_by(year) %>%
  summarise(n = n())
membrane %>% group_by(year) %>%
  summarise(mean_value = mean(n_eggMembrane))
membrane %>% group_by(year) %>%
  summarise(median_value = median(n_eggMembrane))
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
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
    title = "SPEI membranes present in nests 2019-2025",
    x = "Year",
    y = "Count",
    fill = "membrane count" ) +
  theme_minimal(base_size = 14)


