#comparing nest summary info 2019-2026
  #use the Access file from Dan, appended with 2026 data, and using the edits made for the NS analysis

library(tidyverse) #helps to handle and transform data
library(data.table) #for data manipulation and visuals
library(readr) #read in excel files that are not .csv
library(ggplot2) #create figures using ggplot
library(plotrix) #compute simple statistics
library(RODBC) #read in files from Access
library(dplyr) #data management and piping functions


#data manipulation in R through Access and not making edits only in Excel
#or skip to line 155 and load in the cleaned file
db <- "C:/Users/shoepfner/Desktop/nest survival/db_kig_nesting_waterfowl_2019-2026_appended_20260821.accdb"
con_db<-odbcConnectAccess2007(db)
sqlTables(con_db,tableType="TABLE")$TABLE_NAME
nests <- sqlFetch(con_db, "tbl_Nest")
hatch <- sqlFetch(con_db, "qry_estHatch")
str(hatch)

nests$Year <- as.numeric(format(as.Date(nests$dt_found, format="%m/%d/%Y"), "%Y"))
nests$Year <- as.factor(nests$Year)
#remove COEI nests
nests <- nests %>% filter(cat_species == "SPEI")
nests <- nests %>% filter(!(id_nest == "LAH001"))  #probably a COEI nest
nests <- nests %>% filter(!(id_nest == "LLK002"))  #dump egg nest

#remove unneeded columns
names(nests)

#condense fates to hatch=0, failed=1
nests %>%
  distinct(cat_fate) %>%   # Keep only unique names
  pull(cat_fate)    
nests <- nests %>%
  mutate(cat_fate = case_when(
    cat_fate == "eggs_missing" ~ "1",
    cat_fate == "eggs_destroyed" ~ "1",
    cat_fate == "eggs_inviable" ~ "1",
    cat_fate == "eggs_abandoned" ~ "1",
    cat_fate == "active_lastcheck" ~ "0",
    cat_fate == "active" ~ "0",
    cat_fate == "hatched" ~ "0",
    TRUE ~ cat_fate )) #keep other values as they are 


#merge in qry_estHatch table to get estimated hatch dates from latest float
hatched <- merge(nests, hatch, by = "id_nest", all=TRUE)  #gives 18 more nests than there should be...
str(hatched)

#make some corrections for errors in the Access file
hatched$cat_fate[hatched$id_nest == "LLK201"] <- 0
hatched$cat_fate[hatched$id_nest == "RJF633"] <- 0
hatched$cat_fate[hatched$id_nest == "RJF647"] <- 0

#fill in hatch dates, first from known hatched nests
str(hatched)
hatched <- hatched %>% mutate(
  Hatch_Date = if_else(cat_fate == "0", dt_Hatch, #first pull hatch date from known hatched nests
                       estHatch))   #rest of column is filled in from float/candle estimated hatch date
sum(is.na(hatched$Hatch_Date))
hatched$id_nest[which(!complete.cases(hatched$Hatch_Date))]

#fix weird or blank estimated hatch dates
  #ignore nests that were found with <3 eggs and in lay, and only one visit
      #AGB048, AGB053, CLB107, JCA308, JCA318, JMT238, JMT248, LLK001, LRB141, LRB201, 
      #LRB203, MLB012, RJF630, TDM003
hatched$Hatch_Date[hatched$id_nest == "AFM011"] <- as.POSIXct("2023-06-23")
hatched$Hatch_Date[hatched$id_nest == "AFM015"] <- as.POSIXct("2023-07-02")
hatched$Hatch_Date[hatched$id_nest == "LLK201"] <- as.POSIXct("2023-06-23")
hatched$Hatch_Date[hatched$id_yrNest == "2019DJR001"] <- as.POSIXct("2019-06-25")
hatched$Hatch_Date[hatched$id_nest == "JMT308"] <- as.POSIXct("2021-06-19")
hatched$Hatch_Date[hatched$id_nest == "JMT258"] <- as.POSIXct("2019-06-18")
hatched$Hatch_Date[hatched$id_nest == "TCD019"] <- as.POSIXct("2019-06-18")
hatched$Hatch_Date[hatched$id_nest == "AFM013"] <- as.POSIXct("2023-07-02")
hatched$Hatch_Date[hatched$id_nest == "AFM016"] <- as.POSIXct("2023-07-13")
hatched$Hatch_Date[hatched$id_nest == "AFM017"] <- as.POSIXct("2023-07-02")
hatched$Hatch_Date[hatched$id_nest == "AFM109"] <- as.POSIXct("2024-07-04")
hatched$Hatch_Date[hatched$id_nest == "AFM111"] <- as.POSIXct("2024-07-04")
hatched$Hatch_Date[hatched$id_nest == "AGB088"] <- as.POSIXct("2019-06-25")
hatched$Hatch_Date[hatched$id_nest == "AGB092"] <- as.POSIXct("2019-06-24")
hatched$Hatch_Date[hatched$id_nest == "AGB093"] <- as.POSIXct("2019-07-01")
hatched$Hatch_Date[hatched$id_nest == "AWG044"] <- as.POSIXct("2022-06-22")
hatched$Hatch_Date[hatched$id_nest == "CJP066"] <- as.POSIXct("2025-06-28")
hatched$Hatch_Date[hatched$id_nest == "CLB136"] <- as.POSIXct("2021-06-18")
hatched$Hatch_Date[hatched$id_yrNest == "2019DJR003"] <- as.POSIXct("2019-06-20")
hatched$Hatch_Date[hatched$id_yrNest == "2023DJR003"] <- as.POSIXct("2023-06-13")
hatched$Hatch_Date[hatched$id_yrNest == "2019DJR005"] <- as.POSIXct("2019-06-21")
hatched$Hatch_Date[hatched$id_yrNest == "2023DJR005"] <- as.POSIXct("2023-06-30")
hatched$Hatch_Date[hatched$id_yrNest == "2019DJR010"] <- as.POSIXct("2019-06-16")
hatched$Hatch_Date[hatched$id_yrNest == "2023DJR010"] <- as.POSIXct("2023-07-03")
hatched$Hatch_Date[hatched$id_nest == "EWH155"] <- as.POSIXct("2025-06-27")
hatched$Hatch_Date[hatched$id_nest == "EWH156"] <- as.POSIXct("2025-06-27")
hatched$Hatch_Date[hatched$id_nest == "JAM003"] <- as.POSIXct("2019-06-27")
hatched$Hatch_Date[hatched$id_nest == "JCA315"] <- as.POSIXct("2021-06-29")
hatched$Hatch_Date[hatched$id_nest == "JMT254"] <- as.POSIXct("2019-06-25")
hatched$Hatch_Date[hatched$id_nest == "JMT260"] <- as.POSIXct("2019-06-20")
hatched$Hatch_Date[hatched$id_nest == "JMT637"] <- as.POSIXct("2021-07-07")
hatched$Hatch_Date[hatched$id_nest == "JMT645"] <- as.POSIXct("2021-07-06")
hatched$Hatch_Date[hatched$id_nest == "LAH004"] <- as.POSIXct("2024-06-27")
hatched$Hatch_Date[hatched$id_nest == "LLK207"] <- as.POSIXct("2023-06-29")
hatched$Hatch_Date[hatched$id_nest == "LLK208"] <- as.POSIXct("2023-07-01")
hatched$Hatch_Date[hatched$id_nest == "LLK210"] <- as.POSIXct("2023-07-01")
hatched$Hatch_Date[hatched$id_nest == "LLK213"] <- as.POSIXct("2023-07-01")
hatched$Hatch_Date[hatched$id_nest == "LLK217"] <- as.POSIXct("2023-07-02")
hatched$Hatch_Date[hatched$id_nest == "LLK220"] <- as.POSIXct("2023-07-02")
hatched$Hatch_Date[hatched$id_nest == "LRB142"] <- as.POSIXct("2021-06-29")
hatched$Hatch_Date[hatched$id_nest == "LRB150"] <- as.POSIXct("2021-07-07")
hatched$Hatch_Date[hatched$id_nest == "LRB151"] <- as.POSIXct("2021-07-06")
hatched$Hatch_Date[hatched$id_nest == "LRB202"] <- as.POSIXct("2022-06-21")
hatched$Hatch_Date[hatched$id_nest == "LRS078"] <- as.POSIXct("2026-07-01")
hatched$Hatch_Date[hatched$id_nest == "MAM043"] <- as.POSIXct("2022-06-28")
hatched$Hatch_Date[hatched$id_nest == "MAM045"] <- as.POSIXct("2022-07-02")
hatched$Hatch_Date[hatched$id_nest == "MAM177"] <- as.POSIXct("2023-06-30")
hatched$Hatch_Date[hatched$id_nest == "MAM178"] <- as.POSIXct("2023-07-09")
hatched$Hatch_Date[hatched$id_nest == "MLB002"] <- as.POSIXct("2019-06-25")
hatched$Hatch_Date[hatched$id_nest == "MLB010"] <- as.POSIXct("2019-06-24")
hatched$Hatch_Date[hatched$id_nest == "MLB013"] <- as.POSIXct("2019-06-22")
hatched$Hatch_Date[hatched$id_nest == "MLB016"] <- as.POSIXct("2019-06-21")
hatched$Hatch_Date[hatched$id_nest == "MLB018"] <- as.POSIXct("2019-06-30")
hatched$Hatch_Date[hatched$id_nest == "RJF257"] <- as.POSIXct("2019-06-17")
hatched$Hatch_Date[hatched$id_nest == "RJF270"] <- as.POSIXct("2019-06-23")
hatched$Hatch_Date[hatched$id_nest == "RJF273"] <- as.POSIXct("2019-06-20")
hatched$Hatch_Date[hatched$id_nest == "RJF277"] <- as.POSIXct("2019-06-29")
hatched$Hatch_Date[hatched$id_nest == "RJF633"] <- as.POSIXct("2021-06-23")
hatched$Hatch_Date[hatched$id_nest == "RJF647"] <- as.POSIXct("2021-06-28")
hatched$Hatch_Date[hatched$id_nest == "TCD014"] <- as.POSIXct("2019-06-22")
hatched$Hatch_Date[hatched$id_nest == "TCD021"] <- as.POSIXct("2019-06-23")
hatched$Hatch_Date[hatched$id_nest == "TCD029"] <- as.POSIXct("2019-06-30")
hatched$Hatch_Date[hatched$id_nest == "TCD030"] <- as.POSIXct("2019-06-30")
hatched$Hatch_Date[hatched$id_nest == "TCD031"] <- as.POSIXct("2019-06-22")
hatched$Hatch_Date[hatched$id_nest == "TDM004"] <- as.POSIXct("2022-06-23")
hatched$Hatch_Date[hatched$id_nest == "THK008"] <- as.POSIXct("2023-06-29")

hatched$id_nest[which(!complete.cases(hatched$Hatch_Date))]



#add in initiation date by subtracting 24 days from hatch date
str(hatched)
hatched <- hatched %>%
  mutate(init = Hatch_Date - (24 * 86400))

#DJR nests have repeats, remove them
hatched <- hatched %>% distinct(id_yrNest, .keep_all = TRUE)     #DJR001 has nests in 2019, 2023, and 2024


##prep the tbl_Visit to combine the clutch files
visit <- sqlFetch(con_db, "tbl_Visit")
visit <- visit %>% mutate(year = lubridate::year(dt_visit))
visit$year <- as.factor(visit$year)
visit$n_eggsTotal <- as.numeric(visit$n_eggsTotal)
visit$n_eggMembrane <- as.numeric(visit$n_eggMembrane)
visit$n_ducklingAlive <- as.numeric(visit$n_ducklingAlive)
visit$n_ducklingDead <- as.numeric(visit$n_ducklingDead)

visit %>% summarise(na_count = sum(is.na(n_eggsTotal)))

#find most "true" clutch size
#need to find the visit that has the most eggs per nest
unique(visit$id_nest)   #508 nests
eggs <- visit %>% group_by(id_nest) %>% 
  slice_max(order_by = n_eggsTotal, n = 1, with_ties = FALSE)

#exclude nests that had less than three eggs and an early float suggesting still in lay
#removed 45 nests
eggs_3 <- eggs %>% filter(!(n_eggsTotal <= 3 & cat_float1 == "SF"))

merged <- hatched %>%
  left_join(eggs_3 %>% select(id_yrNest, n_eggsTotal), by = "id_yrNest")
merged$id_yrNest[which(!complete.cases(merged$n_eggsTotal))]
  #NA is for clutches that were found in lay = 50


write.csv(merged, "C:/Users/shoepfner/Desktop/summary_nests/all_nests_2019-2026.csv")




####combine in with Randall's nests 1992-2019
  #prep his files
clutch_RJF <- read_csv("C:/Users/shoepfner/Desktop/nest survival/Randall's work/clutch_wx.csv")
nest_inp_RJF <- read_csv("C:/Users/shoepfner/Desktop/nest survival/Randall's work/INP_20220921.csv")

names(clutch_RJF)
clutch_RJF <- clutch_RJF %>% filter(site == "kig")
clutch_RJF <- clutch_RJF %>% select(-site, -win_hi, -spr_hi, -wxt, -sxt, -wxw, -sxw, -wsi)
clutch_RJF <- clutch_RJF %>% rename(Year = year)
clutch_RJF <- clutch_RJF %>% mutate(id_yrNest = paste(Year, Nest, sep = ""))

names(nest_inp_RJF)
nest_inp_RJF <- nest_inp_RJF %>% filter(Site == "kig")
nest_inp_RJF <- nest_inp_RJF %>% select(-Site, -Freq, -AgeDay1, -Win_Hi, - Win_Lo, -Spr_Hi, -Spr_Lo, -wxt, -sxt, -wxw, -sxw, -wsi, -wi)
nest_inp_RJF <- nest_inp_RJF %>% mutate(id_yrNest = paste(Year, Nest, sep = ""))
nest_inp_RJF %>% filter (Year == 2019) %>%
  summarise(n=n()) #165 nests in 2019

#see if same nests in these datasets
setdiff(nest_inp_RJF$id_yrNest, clutch_RJF$id_yrNest) #130 nests in INP and not in clutch, have fate but no egg count
setdiff(clutch_RJF$id_yrNest, nest_inp_RJF$id_yrNest) #160 nests in clutch but not INP, have egg count but no fate
#want to combine them both so all nests are included

#combine all files
old_nests <- full_join(clutch_RJF, nest_inp_RJF, by = "id_yrNest") 
old_nests$id_yrNest[duplicated(old_nests$id_yrNest)]  #no repeats
names(old_nests)
old_nests <- old_nests %>% select(-Init.y, -Year.y, - FirstFound, -LastPresent, -Nest.y, -Nest.x, -LastChecked) %>%
  rename(Init=Init.x, Year=Year.x)
old_nests$Fate <- as.factor(old_nests$Fate)
names(old_nests)

#compare Randall's 2019 to mine
old_nests %>% filter (Year == 2019) %>%
  summarise(n=n())                         #156 nests 
old2019 <- old_nests %>% filter (Year == 2019) 
hatched %>% filter (Year == 2019) %>%
  summarise(n=n())                         #171 nests....
new2019 <- hatched %>% filter (Year == 2019) 

setdiff(new2019$id_yrNest, old2019$id_yrNest) #nestIDs in new2019 and not in old2019
#use mine because it includes more nests for density, includes nests "not monitored"
  #years 1994-2016 are missing nests that may not have been monitored or found with one egg then depredated, biased low

old_nests <- old_nests %>% filter(!Year %in% c(2019, 2021))

#combine earlier years with more recent nests
all_nests <- read_csv("C:/Users/shoepfner/Desktop/summary_nests/all_nests_2019-2026.csv")
names(all_nests)
all_nests <- all_nests %>% 
  rename(maxclutch=n_eggsTotal, Init=init, Fate=cat_fate)
all_nests$Init <- as.POSIXlt(all_nests$Init)$yday
all_nests <- all_nests %>% select(maxclutch, Init, Year, id_yrNest, Fate)
str(all_nests)
names(old_nests)

allll <- bind_rows(old_nests, all_nests)
#just the five rows that both datasets have

write.csv(allll, "C:/Users/shoepfner/Desktop/summary_nests/all_nests_1994-2026.csv")


####look at nest number summaries####
  #just the newer nests so far

all <- read_csv("C:/Users/shoepfner/Desktop/summary_nests/all_nests_1994-2026.csv")
all <- all %>% select(-"...1")
str(all)

all <- all %>% distinct(.keep_all = TRUE)   #remove some repeated nests

test <- all %>%
  group_by(id_yrNest) %>%
  filter(n() > 1) 

#summarize total nest numbers by year
nest_number <- all %>% group_by(Year) %>%
  summarise(n = n())
nest_number
sum(nest_number$n) #3128

ggplot(nest_number, aes(x = Year, y = n)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "SPEI nest numbers, 1994-2026",
    x = "Year",
    y = "Number of nests") +
  theme_minimal(base_size = 14) 


nest_number %>%
  arrange(n)
#last 3 years are in the 5 lowest nest number years out of 29 years
#last 5 years are in top 8 lowest years

mean(nest_number$n) #104.267
sum(nest_number$n)  #3128
min(nest_number$n)  #22
max(nest_number$n)  #179


####nest numbers as nest density####
nest_number$density <- nest_number$n/AREA





####initiation date####
  #not totally accurate because Randall's dates are initiation dates,
    #and mine are the incubation dates
init_all <- all %>% group_by(Year) %>%
  summarise(mean = mean(Init, na.rm = TRUE),
            min = min(Init, na.rm = TRUE), 
            max = max(Init, na.rm = TRUE),
            range = max-min)

ggplot(init_all, aes(x = Year, y = mean)) +
  geom_point() +
  labs(
    title = "Average annual SPEI nest initiation date, 1994-2026",
    x = "Year",
    y = "Julian date") +
  theme_minimal(base_size = 14)


ggplot(all, aes(x = Year, y = Init, group = Year)) +
  geom_boxplot(notch = TRUE, outlier.colour = "red", outlier.shape = 8) +
  labs(
    title = "SPEI nest initiation dates 1994-2026",
       x = "Year",
       y = "Julian Date") +
  theme_minimal()


ggplot(init_all, aes(x = Year, y = range)) +
  geom_point() +
  labs(
    title = "Range of SPEI nest initiation date, 1994-2026",
    x = "Year",
    y = "Julian date") +
  theme_minimal(base_size = 14) 



init_all %>%
  arrange(mean)
#pattern here?

init_range <- init_all %>%
  arrange(range)
mean(init_range$range)  #27.759 days


mean(all$Init, na.rm = TRUE)  #individual mean is 149.033
mean(init_all$mean) #149.324   average of annual averages
min(all$Init, na.rm = TRUE)  #127
max(all$Init, na.rm = TRUE)  #176




####fates####
fate <- all
sum(is.na(fate$Fate))   #167 NA fates.... 5.6% of all nests so minimal

fate$id_yrNest[is.na(fate$Fate)] #nests range from 1994-2015, come from the clutch file
str(fate)
fate$Fate <- as.factor(fate$Fate)

fate %>%
  filter(!is.na(Fate)) %>%
  group_by(Fate) %>%
  summarise(count = n()) 
#2,271 nests were successful and 560 failed over all years

fate_number <- fate %>%
  filter(!is.na(Fate)) %>%
  group_by(Year, Fate) %>%
  summarise(count = n())
#removes all the NAs


ggplot(fate_number, aes(x = Year, y = count, fill = Fate)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_discrete(labels = c("successful", "failed"))+
 labs(
    title = "SPEI nest fates 1994-2026",
    x = "Year",
    y = "Count",
    fill = "nest fate" ) +
  theme_minimal(base_size = 14) 


#same as above but in proportions
fate <- fate %>% filter(!is.na(Fate))
p <- ggplot(fate, aes(x = factor(Year), fill = Fate)) +
  geom_bar(position = position_fill(reverse = TRUE)) +  # position="fill" makes proportions
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(labels = c("successful", "failed"))+
  labs(
    title = "Proportion of apparent nest survival of SPEI nests 1994-2026",
    x = "Year",
    y = "Proportion successful") +
  theme_minimal()
# library(plotly)
ggplotly(p)

plot_data <- ggplot_build(p)$data[[1]]






###clutch sizes####
clutch <- read_csv("C:/Users/shoepfner/Desktop/summary_nests/all_nests_1994-2026.csv")
#remove NAs
clutch <- clutch %>% filter(!is.na(maxclutch))   #removes 50 nests

#number of eggs in a clutch per year
clutch %>% group_by(Year) %>%
  summarise(n = n())
#overall average
mean(clutch$maxclutch)   #4.9352
#annual average
clutch %>% group_by(Year) %>%
  summarise(mean_value = mean(maxclutch))
#annual median
clutch %>% group_by(Year) %>%
  summarise(median_value = median(maxclutch))
#annual mode
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
clutch %>% group_by(Year) %>%
  summarise(mode_value = find_mode(maxclutch))


#plots
plot_clutch <- clutch %>% group_by(Year, maxclutch) %>%
  summarise(n = n())
plot_clutch$maxclutch <- as.factor(plot_clutch$maxclutch)
ggplot(plot_clutch, aes(x = Year, y = n, fill = maxclutch)) +
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




####nest site habitat####
#might have to just do 2019-2026 since earlier years are just the INP file from Randall
nests <- read_csv("C:/Users/shoepfner/Desktop/summary_nests/all_nests_2019-2026.csv")
names(nests)

#fix capital/lower case "island"
site <- nests %>% group_by(cat_nestSite) %>%
  mutate(cat_nestSite = tolower(cat_nestSite))
site.yr <- site %>% group_by(Year, cat_nestSite) %>% 
  summarise(n=n())

ggplot(site.yr, aes(x = Year, y = n, fill = cat_nestSite)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    # title = "SPEI clutch sizes 2019-2025",
    x = "Year",
    y = "Count",
    fill = "nest site" ) +
  theme_minimal(base_size = 14) 


#same as above but as a percentage
site.yr_pct <- site.yr %>%
  group_by(Year) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

ggplot(site.yr_pct, aes(x = Year, y = pct, fill = cat_nestSite)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage of SPEI nests by nest site habitat",
    x = "Year",
    y = "Percentage",
    fill = "nest site") +
  theme_minimal(base_size = 14)



####nest fates by habitat####
nests <- read_csv("C:/Users/shoepfner/Desktop/summary_nests/all_nests_2019-2026.csv")
names(nests)

fate.site <- nests %>% group_by(cat_nestSite) %>%
  mutate(cat_nestSite = tolower(cat_nestSite)) %>%
  filter(!(cat_fate == "not_monitored"))
fate.site <- fate.site %>% group_by(cat_fate, cat_nestSite) %>% 
  summarise(n=n())

fate.site_pct <- fate.site %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

ggplot(fate.site_pct, aes(x = cat_fate, y = pct, fill = cat_nestSite)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "SPEI nest fate by nest site habitat",
       x = "Fate (0 = successful, 1 = failed)",
       y = "Percentage",
       fill = "nest site") +
  theme_minimal(base_size = 14)


#same above but broken down by year
fate.site.year <- nests %>% group_by(cat_nestSite) %>%
  mutate(cat_nestSite = tolower(cat_nestSite)) %>%
  filter(!(cat_fate == "not_monitored")) %>%
  group_by(Year, cat_fate, cat_nestSite) %>% 
  summarise(n=n())

fate.site.year_pct <- fate.site.year %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

fate.site.year_pct <- fate.site.year_pct %>% 
  mutate(cat_fate = recode(cat_fate,
                              "0" = "successful",
                              "1" = "failed"))
  
ggplot(fate.site.year_pct, aes(x = factor(Year), y = pct, fill = cat_nestSite)) +
  geom_col(width = 0.7) +
  facet_wrap(~ cat_fate) +
  labs(title = "SPEI nest fate by year and nest habitat 2019-2026",
    x = "Year",
    y = "Percentage",
    fill = "Habitat")

fate.site.year_pct2 <- fate.site.year_pct %>%
  group_by(Year, cat_fate, cat_nestSite) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Year, cat_fate) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

ggplot(fate.site.year_pct2, aes(x = factor(Year), y = pct, fill = cat_fate)) +
  geom_col(width = 0.7) +
  facet_wrap(~ cat_nestSite) +
  labs(title = "SPEI nest fate by habitat and year",
    x = "Year",
    y = "Percentage",
    fill = "Nest site")


#check statistically if fates are affected by nest site habitat
c_fate.site <- nests %>% group_by(cat_nestSite) %>%
  mutate(cat_nestSite = tolower(cat_nestSite)) %>%
  filter(!(cat_fate == "not_monitored")) %>%
  select(cat_nestSite, id_yrNest, Year, cat_fate)

c_fate.site <- table(c_fate.site$cat_nestSite, c_fate.site$cat_fate)
c_fate.site

str(c_fate.site)
chisq.test(c_fate.site)$expected

chisq_result <- chisq.test(c_fate.site)
chisq_result   #X squared=6.0292, p=0.197, not significantly different


#try including year
c_fate.site <- nests %>% group_by(cat_nestSite) %>%
  mutate(cat_nestSite = tolower(cat_nestSite)) %>%
  filter(!(cat_fate == "not_monitored")) %>%
  select(cat_nestSite, id_yrNest, Year, cat_fate)

model <- glm(as.factor(cat_fate) ~ cat_nestSite + factor(Year), 
             data = c_fate.site, family = binomial)
summary(model)

anova(model, test = "Chisq")
#year is the main driver in nest fate, not habitat





####nesting dates####
#is the range of first found to last checked changing?
  #obviously mostly driven by study design
length <- read_csv("C:/Users/shoepfner/Desktop/nest survival/nest_survival_2022-2025/inp_ns_models_1994-2026_ssa_revision.csv")
str(length)

length <- length %>% group_by(Year) %>%
  mutate(avg_date = as.Date((as.numeric(LastPresent) + as.numeric(LastChecked)) / 2)) %>%
  mutate(length1 = avg_date-FirstFound )
length$length1 <- as.numeric(length$length1)

ggplot(length, aes(x = Year, y = length1, group = Year)) +
  geom_boxplot(notch = TRUE, outlier.colour = "red", outlier.shape = 8) +
  labs(title = "SPEI nest monitoring days 1994-2026",
       x = "Year",
       y = "days monitored") +
  theme_minimal()



length1 <- length %>% group_by(Year, length1) %>% 
  summarise(n=n())

length2 <- length %>% group_by(Year) %>% 
  summarise(
    min_value = min(FirstFound, na.rm = TRUE),
    max_value = max(avg_date, na.rm = TRUE))




