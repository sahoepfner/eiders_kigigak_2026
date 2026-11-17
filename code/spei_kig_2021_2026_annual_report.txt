#code for 2021-2026 annual field report summaries
  #Sarah Hoepfner, November 2026
#comparing nest summary info 2021-2026 to fill in gap from last annual report in 2019
  #use the Access file from Dan, appended with 2026 data

#ADD in renv so depreciated packages can still be used in the future!

library(tidyverse) #helps to handle and transform data
library(data.table) #for data manipulation and visuals
library(readr) #read in excel files that are not .csv
library(ggplot2) #create figures using ggplot
library(plotrix) #compute simple statistics
library(RODBC) #read in files from Access
library(dplyr) #data management and piping functions

#setwd to wherever the project "spei_kig_2021-2026_annualReport" is saved
setwd("C:/Users/shoepfner/Documents/spei_kig_2021-2026_annualReport")

#data manipulation in R through Access and not making edits only in Excel
db <- "data/db_kig_nesting_waterfowl_2019-2026_compiled_20260901.accdb"
con_db<-odbcConnectAccess2007(db)
sqlTables(con_db,tableType="TABLE")$TABLE_NAME
nests <- sqlFetch(con_db, "tbl_Nest")
hatch <- sqlFetch(con_db, "qry_estHatch")
str(hatch)

hatch <- hatch %>%
  mutate(id_yrNest = paste0(year(dt_visit), id_nest))

nests$Year <- as.numeric(format(as.Date(nests$dt_found, format="%m/%d/%Y"), "%Y"))
nests$Year <- as.factor(nests$Year)
#remove COEI nests
nests <- nests %>% filter(cat_species == "SPEI")
nests <- nests %>% filter(!(id_nest == "LAH001"))  #probably a COEI nest
nests <- nests %>% filter(!(id_nest == "LLK002"))  #dump egg nest

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
    cat_fate == "hatched" ~ "0",
    TRUE ~ cat_fate )) #keep other values as they are 


#merge in qry_estHatch table to get estimated hatch dates from latest float
hatched <- left_join(nests, hatch, by = "id_yrNest")  #gives 18 more nests than there should be...
names(nests)
setdiff(hatched$id_yrNest, nests$id_yrNest)
unique(hatched$id_yrNest)   #513 nests

#fill in hatch dates, first from known hatched nests
str(hatched)
hatched <- hatched %>% mutate(
  Hatch_Date = if_else(cat_fate == "0", dt_Hatch, #first pull hatch date from known hatched nests
                       estHatch))   #rest of column is filled in from float/candle estimated hatch date
sum(is.na(hatched$Hatch_Date))
hatched$id_nest[which(!complete.cases(hatched$Hatch_Date))]
hatched <- hatched %>% 
  rename(id_nest=id_nest.x)

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
#should be 513 now


# write.csv(hatched, "data/all_nests_2019-2026.csv")


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
unique(visit$id_nest)   #703 nests
eggs <- visit %>% group_by(id_yrNest) %>% 
  slice_max(order_by = n_eggsTotal, n = 1, with_ties = FALSE)

#exclude nests that had less than three eggs and an early float suggesting still in lay
#removed 45 nests
eggs_3 <- eggs %>% filter(!(n_eggsTotal <= 3 & cat_float1 == "SF"))

merged <- hatched %>%
  left_join(eggs_3 %>% select(id_yrNest, n_eggsTotal), by = "id_yrNest")
merged$id_yrNest[which(!complete.cases(merged$n_eggsTotal))]
  #NA is for clutches that were found in lay = 50

# write.csv(merged, "data/spei_kig_clutch_2019-2026.csv")







####look at nest number summaries####
  #just the newer nests so far
all <- read_csv("data/all_nests_2019-2026.csv")
all <- all %>% select(-"...1")
str(all)
all$year <- as.factor(all$Year)

#subset so just 2021-2026
all <- all %>% filter(!(Year == 2019))


#summarize total nest numbers by year
nest_number <- all %>% group_by(Year) %>%
  summarise(n = n())
nest_number
sum(nest_number$n) #342
mean(nest_number$n) #57
nest_number$Year <- as.factor(nest_number$Year)

ggplot(nest_number, aes(x = Year, y = n)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "SPEI nest numbers, 2021-2026",
    x = "Year",
    y = "Number of nests") +
  theme_minimal(base_size = 14) +
  geom_text(aes(label = n),
            vjust = 1.5,        # Push text down inside the bar
            color = "white",    # White text for contrast
            fontface = "bold",
            size = 4) 

#make a line plot instead?
ggplot(nest_number, aes(x = factor(Year), y = n, group=1)) +
  geom_line() +
  geom_point(size = 3) +
  geom_text(aes(label = n), vjust = -1) +
  labs(x = "Year", y = "Number of nests") +
  geom_hline(yintercept = mean(nest_number$n), linetype = "dashed", color = "black") +
  geom_hline(yintercept = 114, linetype = "dashed", color = "red") +
  theme_minimal() + annotate("text", x = 5.5, y = mean(nest_number$n), label = "2021-2026 nest average", 
         vjust = -0.5, hjust = 0, color = "black", size = 3.5) +
  theme_minimal() + annotate("text", x = 5.5, y = 114, label = "1992-2026 nest average", 
                             vjust = -0.5, hjust = 0, color = "red", size = 3.5) +
  ylim(0,120) +
  theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12))

#add in nest numbers from 1992-2019
df <- data.frame(
  Year = c(1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,
              2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2019,
              2021,2022,2023,2024,2025,2026),
  n = c(64,75,84,103,120,147,111,134,119,43,143,135,
              157,140,174,183,139,98,118,112,122,92,100,71,
              171,106,40,83,59,22,32))

ggplot(df, aes(x = (Year), y = n, group=1)) +
  geom_line() +
  geom_point(size = 3) +
  geom_text(aes(label = n), vjust = -1) +
  labs(x = "Year", y = "Number of nests") +
  geom_hline(yintercept = mean(nest_number$n), linetype = "dashed", color = "red") +
  geom_hline(yintercept = 114, linetype = "dashed", color = "black") +
  # theme_minimal() + annotate("text", x = 5.5, y = mean(nest_number$n), label = "2021-2026 nest average", 
  #                            vjust = -0.5, hjust = -0.55, color = "black", size = 3.5) +
  # theme_minimal() + annotate("text", x = 5.5, y = 114, label = "1992-2026 nest average", 
  #                            vjust = -0.5, hjust = -1, color = "red", size = 3.5) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(df$Year), max(df$Year), by = 1)) +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y  = element_text(size = 12))



nest_number %>%
  arrange(n)
#last 3 years are in the 5 lowest nest number years out of 29 years
#last 5 years are in top 8 lowest years

mean(nest_number$n) #57
sum(nest_number$n)  #342
min(nest_number$n)  #22
max(nest_number$n)  #106





####initiation date####
  #mine are the incubation dates
init_all <- all

init_all$init <- as.Date(init_all$init, format = "%m/%d/%Y")

init_all$month_day <- as.Date(
  paste0("2000-", strftime(as.Date(init_all$init), "%m-%d")))

str(init_all)

init_all1 <- init_all %>% 
  summarise(mean = mean(month_day, na.rm = TRUE),
            median = median(month_day, na.rm = TRUE),
            min = min(month_day, na.rm = TRUE), 
            max = max(month_day, na.rm = TRUE),
            range = max-min)
init_all1

init_all2 <- init_all %>% group_by(Year) %>%
  summarise(mean = mean(init, na.rm = TRUE),
            median = median(init, na.rm = TRUE),
            min = min(init, na.rm = TRUE), 
            max = max(init, na.rm = TRUE),
            range = max-min)
init_all2 <- init_all2 %>%  
  mutate(across(c(mean, median, min, max), 
                ~ format(as.POSIXct(., origin = "1970-01-01"), "%m-%d")))
init_all2
init_all2$Year <- as.factor(init_all2$Year)

ggplot(init_all2, aes(x = Year, y = mean)) +
  geom_point() +
  labs(
    title = "Average annual SPEI nest initiation date, 2021-2026",
    x = "Year",
    y = "Julian date") +
  theme_minimal(base_size = 14)

all$Year <- as.factor(all$Year)
all <- all %>%  
  mutate(across(c(init), 
                ~ format(as.POSIXct(., origin = "1970-01-01"), "%m-%d")))
ggplot(all, aes(x = Year, y = init, group = Year)) +
  geom_boxplot(notch = TRUE, outlier.colour = "red", outlier.shape = 8) +
  labs(
    title = "SPEI nest initiation dates 2021-2026",
       x = "Year",
       y = "Julian Date") +
  theme_minimal()


ggplot(init_all2, aes(x = Year, y = range)) +
  geom_point() +
  labs(
    title = "Range of SPEI nest initiation date, 1994-2026",
    x = "Year",
    y = "Julian date") +
  theme_minimal(base_size = 14) 



init_all2 %>%
  arrange(mean)
#no pattern

init_range <- init_all2 %>%
  arrange(range)
mean(init_range$range)  #27.759 days

#need to pull out just months
all$init <- as.Date(all$init)
mean(all$init, na.rm = TRUE)  #individual mean is 149.033
mean(init_all2$mean) #149.324   average of annual averages
min(all$init, na.rm = TRUE)  #127
max(all$init, na.rm = TRUE)  #176





####init/hatch dates####
#plot histogram of all initiation dates by years
all <- read_csv("data/all_nests_2019-2026.csv")
all <- all %>% select(-"...1")
str(all)
all$year <- as.factor(all$Year)

#subset so just 2021-2026
all <- all %>% filter(!(Year == 2019))
str(all)

all$init <- as.Date(all$init, format = "%m/%d/%Y")
all$Hatch_Date <- as.Date(all$Hatch_Date, format = "%m/%d/%Y")

median_init <- all %>%
  group_by(Year) %>%
  summarise(median_init = median(init, na.rm = TRUE))
median_init$Year <- as.factor(median_init$Year)
median_init


init_2021 <- all %>% filter(Year==2021) %>%
  filter(!is.na(init)) 
init_2022 <- all %>% filter(Year==2022) %>%
  filter(!is.na(init)) 
init_2023 <- all %>% filter(Year==2023) %>%
  filter(!is.na(init)) 
init_2024 <- all %>% filter(Year==2024) %>%
  filter(!is.na(init)) 
init_2025 <- all %>% filter(Year==2025) %>%
  filter(!is.na(init)) 
init_2026 <- all %>% filter(Year==2026) %>%
  filter(!is.na(init)) 


#get earliest dates
init.range <- all %>%  mutate(mm_dd = format(init, "%m-%d"))
min(init.range$mm_dd, na.rm = TRUE)  #5-08
max(init.range$mm_dd, na.rm = TRUE)  #6-20

#pull out only nests that actually did hatch
  #want to graph all init dates, but only the hatch dates of nests that actually hatched
hatch.range <- all %>% filter(!is.na(dt_Hatch))  #211 nests

hatch.range <- hatch.range %>%
  mutate(
    Hatch_Date = as.Date(Hatch_Date, format = "%m/%d/%Y"),
    mm_dd = format(Hatch_Date, "%m-%d"))

min(hatch.range$mm_dd, na.rm = TRUE)  #6-09
max(hatch.range$mm_dd, na.rm = TRUE)  #7-14

median_hatch <- hatch.range %>%
  group_by(Year) %>%
  summarise(median_hatch = median(Hatch_Date, na.rm = TRUE))
median_hatch$Year <- as.factor(median_hatch$Year)
median_hatch

#USE Hatch_Date or dt_Hatch???? Only use nests that actually hatched? separates dates from just init+24

plot_data_2021 <- init_2021 %>%
  select(init, Hatch_Date) %>%
  pivot_longer(cols = everything(), names_to = "group", values_to = "value")
plot_2021 <- ggplot(plot_data_2021, aes(x = value, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30, color = "black") +
  labs(x = " ", y = " ", fill = " ", title = "2021") +
  geom_vline(xintercept=(as.Date("2021-05-28")), 
             linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept=(as.Date("2021-06-21")), 
             linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("init" = "lightblue", "HatchDate" = "gray")) +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = as.Date(c("2021-05-07", "2021-07-15")), ylim = c(0, 25)) +
  scale_x_date(date_breaks = "5 days",       # labeled major ticks, every 5 days
               date_minor_breaks = "1 day",  # unlabeled minor tick marks, every day
               date_labels = "%b %d") +
  labs(caption = "n = 100 nests") 
plot_2021

plot_data_2022 <- init_2022 %>%
  select(init, Hatch_Date) %>%
  pivot_longer(cols = everything(), names_to = "group", values_to = "value")
plot_2022 <- ggplot(plot_data_2022, aes(x = value, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30, color = "black") +
  labs(x = " ", y = " ", fill = " ", title = "2022") +
  geom_vline(xintercept=(as.Date("2022-05-29")), 
             linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept=(as.Date("2022-06-22")), 
             linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("init" = "lightblue", "HatchDate" = "gray")) +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = as.Date(c("2022-05-07", "2022-07-15")), ylim = c(0, 25)) +
  scale_x_date(date_breaks = "5 days",       # labeled major ticks, every 5 days
               date_minor_breaks = "1 day",  # unlabeled minor tick marks, every day
               date_labels = "%b %d") +
  labs(caption = "n = 36 nests") 
plot_2022

plot_data_2023 <- init_2023 %>%
  select(init, Hatch_Date) %>%
  pivot_longer(cols = everything(), names_to = "group", values_to = "value")
plot_2023 <- ggplot(plot_data_2023, aes(x = value, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30, color = "black") +
  labs(x = " ", y = " ", fill = " ", title = "2023") +
  geom_vline(xintercept=(as.Date("2023-05-29")), 
             linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept=(as.Date("2023-06-22")), 
             linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("init" = "lightblue", "HatchDate" = "gray")) +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = as.Date(c("2023-05-07", "2023-07-15")), ylim = c(0, 25)) +
  scale_x_date(date_breaks = "5 days",       # labeled major ticks, every 5 days
               date_minor_breaks = "1 day",  # unlabeled minor tick marks, every day
               date_labels = "%b %d") +
  labs(caption = "n = 83 nests") 
plot_2023

plot_data_2024 <- init_2024 %>%
  select(init, Hatch_Date) %>%
  pivot_longer(cols = everything(), names_to = "group", values_to = "value")
plot_2024 <- ggplot(plot_data_2024, aes(x = value, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30, color = "black") +
  labs(x = " ", y = " ", fill = " ", title = "2024") +
  geom_vline(xintercept=(as.Date("2024-06-03")), 
             linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept=(as.Date("2024-06-27")), 
             linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("init" = "lightblue", "HatchDate" = "gray")) +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = as.Date(c("2024-05-07", "2024-07-15")), ylim = c(0, 25)) +
  scale_x_date(date_breaks = "5 days",       # labeled major ticks, every 5 days
               date_minor_breaks = "1 day",  # unlabeled minor tick marks, every day
               date_labels = "%b %d")+
  labs(caption = "n = 59 nests") 
plot_2024

plot_data_2025 <- init_2025 %>%
  select(init, Hatch_Date) %>%
  pivot_longer(cols = everything(), names_to = "group", values_to = "value")
plot_2025 <- ggplot(plot_data_2025, aes(x = value, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30, color = "black") +
  labs(x = " ", y = " ", fill = " ", title = "2025") +
  geom_vline(xintercept=(as.Date("2025-05-25")), 
             linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept=(as.Date("2025-06-18")), 
             linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("init" = "lightblue", "HatchDate" = "gray")) +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = as.Date(c("2025-05-07", "2025-07-15")), ylim = c(0, 25)) +
  scale_x_date(date_breaks = "5 days",       # labeled major ticks, every 5 days
               date_minor_breaks = "1 day",  # unlabeled minor tick marks, every day
               date_labels = "%b %d") +
  labs(caption = "n = 22 nests") 
plot_2025

plot_data_2026 <- init_2026 %>%
  select(init, Hatch_Date) %>%
  pivot_longer(cols = everything(), names_to = "group", values_to = "value")
sample_sizes <- plot_data_2026 %>%
  group_by(group) %>%
  summarise(n = n())
plot_2026 <- ggplot(plot_data_2026, aes(x = value, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30, color = "black") +
  labs(x = " ", y = " ", fill = " ", title = "2026") +
  geom_vline(xintercept=(as.Date("2026-05-28")), 
             linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(xintercept=(as.Date("2026-06-21")), 
             linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("init" = "lightblue", "HatchDate" = "gray")) +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(xlim = as.Date(c("2026-05-07", "2026-07-15")), ylim = c(0, 25)) +
  scale_x_date(date_breaks = "5 days",       # labeled major ticks, every 5 days
    date_minor_breaks = "1 day",  # unlabeled minor tick marks, every day
    date_labels = "%b %d") +
  labs(caption = "n = 32 nests") 
plot_2026



library(ggpubr)
combined_plot <- ggarrange(plot_2021, plot_2022, plot_2023, plot_2024,
                           plot_2025, plot_2026, ncol=1, nrow=6)

annotate_figure(combined_plot,
                bottom = text_grob(" ", size = 14),
                left = text_grob("Number of nests", size = 14, rot = 90))






####fates####
fate <- all
sum(is.na(fate$cat_fate))   

fate$id_yrNest[is.na(fate$cat_fate)] #nests range from 1994-2015, come from the clutch file
str(fate)
fate$Fate <- as.factor(fate$cat_fate)

fate %>%
  filter(!is.na(cat_fate)) %>%
  group_by(cat_fate) %>%
  summarise(count = n()) 
#244 hatched, 94 failed, and 4 not monitored

fate %>%
  group_by(year) %>%
  filter(!(cat_fate=="not_monitored")) %>% 
  summarise(
    n_zero = sum(cat_fate == 0),
    n_total = n(),
    prop_zero = n_zero / n_total)

fate_number <- fate %>%
  filter(!is.na(cat_fate)) %>%
  group_by(year, cat_fate) %>%
  summarise(count = n()) %>%
  filter(!(cat_fate=="not_monitored"))
#removes all the NAs, and not_monitored


ggplot(fate_number, aes(x = year, y = count, fill = cat_fate)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_discrete(labels = c("successful", "failed"))+
 labs(title = "SPEI nest fates 2021-2026",
    x = "Year",
    y = "Count",
    fill = "nest fate" ) +
  theme_minimal(base_size = 14) 



#same as above but in proportions
library(plotly)
fate <- fate %>% filter(!is.na(Fate)) %>%
  filter(!(cat_fate=="not_monitored"))
#change 0/1 to successful/failed
fate <- fate %>%
  mutate(Fate = recode(Fate,
                         "0" = "successful",
                         "1" = "failed"))

p <- ggplot(fate, aes(x = factor(Year), fill = Fate)) +
  geom_bar(position = position_fill(reverse = TRUE)) +  # position="fill" makes proportions
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of apparent nest survival of SPEI nests 2021-2026",
    x = "Year",
    y = "Proportion successful") +
  theme_minimal() 
p

ggplotly(p)

plot_data <- ggplot_build(p)$data[[1]]





####hatch dates####
all <- read_csv("data/all_nests_2019-2026.csv")
all <- all %>% select(-"...1")
str(all)
all$year <- as.factor(all$Year)

#subset so just 2021-2026
all <- all %>% filter(!(Year == 2019))

hatch <- all %>% filter(cat_fate == 0) 

hatch %>% group_by(Year) %>% 
  summarise(mean = mean(Hatch_Date, na.rm = TRUE),
            median = median(Hatch_Date, na.rm = TRUE),
            min = min(Hatch_Date, na.rm = TRUE), 
            max = max(Hatch_Date, na.rm = TRUE),
            range = max-min)

#total averages not grouped by year
hatch1 <- hatch

hatch1$month_day <- as.Date(format(hatch1$dt_Hatch, "2000-%m-%d"))
str(hatch1)

hatch1 <- hatch1 %>% 
  summarise(mean = mean(month_day, na.rm = TRUE),
            median = median(month_day, na.rm = TRUE),
            min = min(month_day, na.rm = TRUE), 
            max = max(month_day, na.rm = TRUE),
            range = max-min)
hatch1





###clutch sizes####
clutch <- read_csv("data/all_nests_1994-2026.csv")
#remove NAs
clutch <- clutch %>% filter(!is.na(maxclutch))   #removes 50 nests

#filter out years of interest
clutch <- clutch %>% filter(Year %in% c(2021, 2022, 2023, 2024, 2025, 2026))

#total number of eggs in max clutch
sum(clutch$maxclutch)

#number of nests per year with max clutch
clutch %>% group_by(Year) %>%
  summarise(n = n())
#overall average
mean(clutch$maxclutch)   #5.025
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

#get 95%CI on the mean
sd(eggs_3$n_eggsTotal) #1.216085
se <- 1.216085 / sqrt(217)  #SE= 0.08255
estimate <- 5.009      # your point estimate        
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



####membrane counts####
db <- "data/db_kig_nesting_waterfowl_2019-2026_compiled_20260901.accdb"
con_db<-odbcConnectAccess2007(db)
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
visit <- visit %>% filter(id_species=="SPEI")

visit %>% summarise(na_count = sum(is.na(n_eggsTotal)))
#7/1665 have no egg  values = 0.4%, I filled in at least one row for each nest


##clutch counts##

#find most "true" clutch size
#need to find the visit that has the most eggs per nest
unique(visit$id_yrNest)   #346 nests
#subset just to 2021-2026
visit <- visit %>% filter(year %in% c(2021, 2022, 2023, 2024, 2025, 2026))
eggs <- visit %>% group_by(id_yrNest) %>% 
  slice_max(order_by = n_eggsTotal, n = 1, with_ties = FALSE)
eggs %>% group_by(year) %>%
  summarise(total = sum(n_eggsTotal, na.rm = TRUE))

#exclude nests that had less than three eggs and an early float suggesting still in lay
#removed 45 nests
eggs_3 <- eggs %>% filter(!(n_eggsTotal <= 3 & cat_float1 == "SF"))




#number of eggs in a clutch per year
eggs_3 %>% group_by(year) %>%
  summarise(n = n()) #number of nests really
#overall average
mean(eggs_3$n_eggsTotal)   #4.996855
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
sd(eggs_3$n_eggsTotal) #1.075973
se <- 1.075973 / sqrt(318)  #SE= 0.060337
estimate <- 4.996855      # your point estimate        
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



#number of inviable eggs
invia <- visit %>% group_by(id_yrNest) %>% 
  slice_max(order_by = n_eggsNotviable, n = 1, with_ties = FALSE)
invia <- invia %>% filter(n_eggsNotviable>0)  #79 nests had inviable eggs, includes both hatched and failed nests
invia <- invia %>% filter(!(year=="2019"))

#number of nests with inviable eggs per year
invia %>% group_by(year) %>%
  summarise(n = n()) 
#number of inviable eggs per year
invia %>% group_by(year) %>%
  summarise(total = sum(n_eggsNotviable, na.rm = TRUE))
sum(invia$n_eggsNotviable)   #145 eggs inviable
#overall average
mean(invia$n_eggsNotviable)   #1.883
min(invia$n_eggsNotviable)   #1
max(invia$n_eggsNotviable)   #6
#annual average
invia %>% group_by(year) %>%
  summarise(mean_value = mean(n_eggsNotviable))  #range from 1.25 (2022) to 2.22 (2024)
#annual median
invia %>% group_by(year) %>%
  summarise(median_value = median(n_eggsNotviable))


#combine into one dataframe to plot together
inv <- invia %>% group_by(year) %>%
  summarise(n_nests = n())
inv$n_eggs <- c(34,5,20,20,15,19)
inv$total_nests <- c(106,40,83,59,22,32)
inv$total_eggs <- c(504,171,413,293,100,165)
inv$prop_nests <- inv$n_nests/inv$total_nests
inv$prop_eggs <- inv$n_eggs/inv$total_eggs
inv

scale_factor.n <- max(inv$n_nests) / max(inv$prop_nests)
inv.nests <- inv %>%
  mutate(proportion_scaled = prop_nests * scale_factor.n)

ggplot(inv.nests, aes(x = factor(year))) +
  geom_col(aes(y = n_nests), fill = "steelblue", width = 0.6) +
  geom_point(aes(y = proportion_scaled), color = "darkred", size = 3) +
  geom_line(aes(y = proportion_scaled, group = 1), color = "darkred", linewidth = 0.8) +
  scale_y_continuous(
    name = "Count",
    sec.axis = sec_axis(~ . / scale_factor.n, name = "Proportion",
                        labels = scales::percent_format(accuracy = 1))) +
  labs(x = "Year", title = "Inviable nests") +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "darkred"),
    axis.text.y.right  = element_text(color = "darkred"),
    axis.title.y.left  = element_text(color = "steelblue"),
    axis.text.y.left   = element_text(color = "steelblue"))



scale_factor.e <- max(inv$n_eggs) / max(inv$prop_eggs)
inv.eggs <- inv %>%
  mutate(proportion_scaled = prop_eggs * scale_factor.e)

ggplot(inv.eggs, aes(x = factor(year))) +
  geom_col(aes(y = n_eggs), fill = "steelblue", width = 0.6) +
  geom_point(aes(y = proportion_scaled), color = "darkred", size = 3) +
  geom_line(aes(y = proportion_scaled, group = 1), color = "darkred", linewidth = 0.8) +
  scale_y_continuous(
    name = "Count",
    sec.axis = sec_axis(~ . / scale_factor.e, name = "Proportion",
                        labels = scales::percent_format(accuracy = 1))) +
  labs(x = "Year", title = "Inviable eggs") +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "darkred"),
    axis.text.y.right  = element_text(color = "darkred"),
    axis.title.y.left  = element_text(color = "steelblue"),
    axis.text.y.left   = element_text(color = "steelblue"))




##membrane count##
#Access shows 236 nest (visits) with membranes observed
membrane <- visit %>% group_by(id_nest) %>% 
  slice_max(order_by = n_eggMembrane, n = 1, with_ties = FALSE) %>%
  filter(!(n_eggMembrane == 0))      #get 256 here, there may have been repeat visits to nests where membranes were counted

#subset just to 2021-2026
membrane <- membrane %>% filter(year %in% c(2021, 2022, 2023, 2024, 2025, 2026))

#number of nests per year with membranes
mean(membrane$n_eggMembrane)   #3.946746
membrane %>% group_by(year) %>%
  summarise(n = n())
membrane %>% group_by(year) %>%
  summarise(mean_value = mean(n_eggMembrane))
membrane %>% group_by(year) %>%
  summarise(count = sum(n_eggMembrane))
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
sd(membrane$n_eggMembrane) #1.402635
se <- 1.402635 / sqrt(169)  #SE= 0.107895
estimate <- 3.946746      # your point estimate        
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







####nest site habitat####
#might have to just do 2019-2026 since earlier years are just the INP file from Randall
nests <- read_csv("data/all_nests_2019-2026.csv")
names(nests)

nests <- nests %>% filter(!(Year == 2019))
nests$Year <- as.factor(nests$Year)

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
nests <- read_csv("data/all_nests_2019-2026.csv")
names(nests)

nests <- nests %>% filter(!(Year == 2019))
nests$Year <- as.factor(nests$Year)

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



site.fate <- nests %>% 
  mutate(cat_nestSite = tolower(cat_nestSite)) %>%
  filter(!(cat_fate == "not_monitored")) %>%
  group_by(cat_nestSite, cat_fate) %>%
  summarise(n=n())
site.fate

site.fate_pct <- site.fate %>%
  group_by(cat_nestSite) %>% 
  mutate(pct = n / sum(n)) %>%
  ungroup()

ggplot(site.fate_pct, aes(x = factor(cat_nestSite), y = pct, fill = cat_fate)) +
  geom_col(width = 0.7) +
  scale_fill_discrete(labels = c("0" = "successful", "1" = "failed")) +
  # facet_wrap(~ cat_fate) +
  labs(title = "SPEI nest fate by nest habitat 2021-2026",
       x = "Nest site habitat",
       y = "Percentage",
       fill = "Nest fate") 


#same as above but including year
site.fate.year <- nests %>% 
  mutate(cat_nestSite = tolower(cat_nestSite)) %>%
  filter(!(cat_fate == "not_monitored")) %>%
  group_by(cat_nestSite, cat_fate, Year) %>%
  summarise(n=n())
site.fate.year

site.fate.year_pct <- site.fate.year %>%
  group_by(Year, cat_nestSite) %>% 
  mutate(pct = n / sum(n)) %>%
  ungroup()

ggplot(site.fate.year_pct, aes(x = factor(cat_nestSite), y = pct, fill = cat_fate)) +
  geom_col(width = 0.7) +
  facet_wrap(~ Year) +
  scale_fill_discrete(labels = c("0" = "successful", "1" = "failed")) +
  labs(title = "SPEI nest fate by nest habitat and year, 2021-2026",
       x = "Nest site habitat",
       y = "Percentage",
       fill = "Nest fate")


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
chisq_result   #X squared=2.777, p=0/5958, not significantly different


#try including year
c_fate.site <- nests %>% group_by(cat_nestSite) %>%
  mutate(cat_nestSite = tolower(cat_nestSite)) %>%
  filter(!(cat_fate == "not_monitored")) %>%
  select(cat_nestSite, id_yrNest, Year, cat_fate)

model <- glm(as.factor(cat_fate) ~ cat_nestSite + factor(Year), 
             data = c_fate.site, family = binomial)
summary(model)

anova(model, test = "Chisq")
#year is the significant driver in nest fate, not habitat




####abandoned nests####
db <- "data/db_kig_nesting_waterfowl_2019-2026_compiled_20260901.accdb"
con_db<-odbcConnectAccess2007(db)
visit <- sqlFetch(con_db, "tbl_Visit")
nest <- sqlFetch(con_db, "tbl_Nest")
nest <- nest %>% filter(cat_species=="SPEI")
nest$Year <- as.numeric(format(as.Date(nest$dt_found, format="%m/%d/%Y"), "%Y"))
nest <- nest %>% filter(!(Year==2019)) 
nest.a <- nest %>% filter(cat_fate=="eggs_abandoned") #only 9 SPEI nests deemed abandoned


#count how many vfilter_()#count how many visits per nest
visits <- visit %>% 
  group_by(id_yrNest) %>%
  summarise(n_visits=n())
visits$n_visits <- as.numeric(visits$n_visits)
visits$n_visits <- visits$n_visits-1 #subtract one since the last visit is fating the nest
visits

nests <- merge(nest.a, visits, by="id_yrNest")

nests.abandon <- nests %>% 
  summarise(mean = mean(n_visits, na.rm = TRUE),
            median = median(n_visits, na.rm = TRUE),
            min = min(n_visits, na.rm = TRUE), 
            max = max(n_visits, na.rm = TRUE))
nests.abandon  #abandoned nests had on average 2.22 visits



#other nests had how many visits?
nest.failed <- nest %>% filter(cat_fate %in% c("eggs_missing","eggs_destroyed")) #only look at failed nests

nest.faileds <- merge(nest.failed, visits, by="id_yrNest")

nest.faileds$Year <- as.numeric(format(as.Date(nest.faileds$dt_found, format="%m/%d/%Y"), "%Y"))

nest.faileds <- nest.faileds %>% 
  summarise(mean = mean(n_visits, na.rm = TRUE),
            median = median(n_visits, na.rm = TRUE),
            min = min(n_visits, na.rm = TRUE), 
            max = max(n_visits, na.rm = TRUE))
nest.faileds  #failed nests had on average 1.57 visits


#number of abandoned nests per year
nest.a
abandon.yr <- nest.a %>% group_by(Year) %>%
  summarise(n=n())
abandon.yr
mean(abandon.yr$n)



####nesting date range####
#is the range of first found to last checked changing?
  #obviously mostly driven by study design
length <- read_csv("data/inp_ns_models_1994-2026_ssa_revision_final.csv")
str(length)

length <- length %>% group_by(Year) %>%
  mutate(avg_date = as.Date((as.numeric(LastPresent) + as.numeric(LastChecked)) / 2)) %>%
  mutate(length1 = avg_date-FirstFound )
length$avg_date <- as.numeric(length$avg_date)
length$length1 <- as.numeric(length$length1)
length <- length %>% filter(Year %in% c(2021:2026)) 

ggplot(length, aes(x = Year, y = length1, group = Year)) +
  geom_boxplot(notch = FALSE, outlier.colour = "red", outlier.shape = 8) +
  labs(title = "SPEI nest monitoring days 2021-2026",
       x = "Year",
       y = "days monitored") +
  theme_minimal()


lengths <- length %>% group_by(Year, length1) %>% 
  summarise(n=n())
lengths

length2 <- length %>% group_by(Year) %>% 
  summarise(mean = mean(length1),
    min_value = min(length1, na.rm = TRUE),
    max_value = max(length1, na.rm = TRUE))
length2
mean(length2$mean)
###ADD these to the word doc?



####capture type####
db <- "data/db_kig_nesting_waterfowl_2019-2026_compiled_20260901.accdb"
con_db<-odbcConnectAccess2007(db)
sqlTables(con_db,tableType="TABLE")$TABLE_NAME
band <- sqlFetch(con_db, "tbl_Band")
nest <- sqlFetch(con_db, "tbl_Nest")
names(band)

band <- band %>% filter(cat_species == "SPEI")
band$Year <- as.numeric(format(as.Date(band$dt_capt, format="%m/%d/%Y"), "%Y"))
#keep only adult females
band <- band %>% filter(cat_sex == "female")
band <- band %>% filter(!(cat_age == "local"))

#need to change recap birds that got TDRs removed
  #change so is_recap = yes
band <- band %>% 
  mutate(is_recap = ifelse(cat_tdrStatus == "retrieved", "no", is_recap))

capt <- band %>% group_by(Year, is_recap) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Year) %>% 
  mutate(prop = n / sum(n)) %>%
  ungroup()
capt

ggplot(capt, aes(x = factor(Year), y = prop, fill = is_recap)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Year",
    y = "Percentage",
    fill = "Capture Type") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(
    values = c("no" = "darkgray", "yes" = "darkblue"),
    labels = c("no" = "new capture", "yes" = "recapture"))



#plot percentage of birds resighted and banded and recaught each year
names(band)

#merge band and tbl_Nest
nest <- nest %>% filter(cat_species == "SPEI")
merged <- merge(band, nest, by = "id_yrNest", all = TRUE) #more values because includes arrival and brood captures
names(merged)
unique(merged$id_yrNest)  


#remove birds that had no identified band seen
merged <- merged %>% 
  filter(!(id_plasticBand.x == "NA" & id_plasticBand.y == "NA")) %>%
  filter(!(id_plasticBand.x == "na" & id_plasticBand.y == "na"))

merged <- merged %>% 
  mutate(band_type = case_when(
    is_recap == "yes" ~ "newband",
    cat_resightMethod == "camera" ~ "resight",
    cat_resightMethod == "visual" ~ "resight",
    cat_resightMethod == "dead_recovery" ~ "resight",
    cat_resightMethod == "newBand" ~ "newband", 
    cat_plasticBandStatus == "unknown" ~ "newband",
    cat_resightMethod == "newcapture" ~ "newband", 
    cat_trapStatus == "not_trapped" & cat_plasticBandStatus == "known" ~ "resight",
    cat_periodCapt == "NA" ~ "resight",
    cat_periodCapt == "arrival" ~ "newband",
    cat_periodCapt == "brood" ~ "newband",
    cat_plasticBandStatus == "none" ~ "newband",
    is_success == "yes" & cat_plasticBandStatus == "known" ~ "resight",
    TRUE ~ "Other"))


#try and get this value down as much as possible!!!!
sum(merged$band_type == "Other", na.rm = TRUE)
#rest seem to be resights

merged <- merged %>% 
  mutate(band_type = recode(band_type, "Other" = "resight"))

#add in Year again for nests that have NA
merged <- merged %>% 
  mutate(Year = if_else(
    is.na(Year),
    as.numeric(format(dt_found, "%Y")),
    Year))

#remove 2018
merged <- merged %>% filter(!(Year==2018))


type <- merged %>% group_by(Year, band_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Year) %>% 
  mutate(prop = n / sum(n)) %>%
  ungroup()

#percent graph
ggplot(type, aes(x = factor(Year), y = prop, fill = band_type)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Year",
    y = "Percentage",
    fill = "Annual mark type") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(
    values = c("newband" = "darkgray", "resight" = "darkblue"),
    labels = c("newband" = "new capture", "resight" = "resight"))

type$Year <- as.factor(type$Year)
type <- type %>% filter(!Year == 2019)

#same as above but just counts
ggplot(type, aes(x = Year, y = n, fill = band_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_discrete(labels = c("new band", "resight"))+
  labs(title = "SPEI mark-recap input 2021-2026",
       x = "Year",
       y = "Count",
       fill = " " ) +
  theme_minimal(base_size = 14) 




####blood samples####
  #numbers in tbl_blood and tbl_tissue are all messed up
    #I just went through and counted for each year, skip to REAL numbers below
db <- "data/db_kig_nesting_waterfowl_2019-2026_compiled_20260923.accdb"
con_db<-odbcConnectAccess2007(db)
sqlTables(con_db,tableType="TABLE")$TABLE_NAME
band <- sqlFetch(con_db, "tbl_Band")
tissue <- sqlFetch(con_db, "tbl_tissueSamples")
names(band)
names(tissue)
tissue$id_metalBand <- as.character(tissue$id_metalBand)
tissue$Year <- as.numeric(format(as.Date(tissue$dt_sample, format="%m/%d/%Y"), "%Y"))
band$Year <- as.numeric(format(as.Date(band$dt_capt, format="%m/%d/%Y"), "%Y"))

#number of birds banded/year
band_year <- band %>% group_by(Year) %>%
  filter(cat_species=="SPEI") %>%
  filter(cat_age=="AHY") %>%
  summarise(n=n())
band_year   #this includes TDR that were deployed and retrieved also, so not just newly banded birds



#number of blood samples/year
blood.tissue <- merge(tissue, band, 
                      by.x = c("id_metalBand", "dt_sample"), 
                      by.y = c("id_metalBand", "dt_capt"), 
                      all.x = TRUE)
blood.tissue$Year <- as.numeric(format(as.Date(blood.tissue$dt_sample, format="%m/%d/%Y"), "%Y"))
str(blood.tissue)

blood.tissue <- blood.tissue %>% filter(Year > 2020) %>%
  filter(amt_bloodVol > 0.01) 

blood.tissue <- blood.tissue %>%
  filter(cat_species == "SPEI" | is.na(cat_species)) %>%
  # filter(cat_species == "COEI") %>%    #uncomment for COEI 
  group_by(Year) %>%
  # filter(cat_periodCapt=="brood") %>%  #uncomment for only during incubation period
  summarise(n=n())
blood.tissue
sum(blood.tissue$n)



ggplot(blood.tissue, aes(x = Year, y = n)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "blood samples per year",
       x = "Year",
       y = "Count",
       fill = " " ) +
  # geom_text(aes(label = n), 
  #           vjust = 1.5,        # Push text down inside the bar
  #           color = "white",    # White text for contrast
  #           fontface = "bold",
  #           size = 4) +
  theme_minimal(base_size = 14) 






###banding changes####
#weight of birds by year
db <- "data/db_kig_nesting_waterfowl_2019-2026_compiled_20260923.accdb"
con_db<-odbcConnectAccess2007(db)
sqlTables(con_db,tableType="TABLE")$TABLE_NAME
band <- sqlFetch(con_db, "tbl_Band")
band <- band %>% filter(cat_species=="SPEI")
# band$dt_capt <- as.factor(band$dt_capt)
band$year <- as.factor(format(as.Date(band$dt_capt, format="%m/%d/%Y"), "%Y"))
band <- band %>% filter(!(year==2019))
band <- band %>% filter(!(year==2018))
weight <- band %>% filter(!(val_bodyMass_kg=="-9"))
#include only birds banded during incubation (which is also just adult females)
weight <- weight %>% filter(cat_periodCapt=="incubation")


weight$year <- as.factor(weight$year)
weights <- weight %>% 
  # group_by(year) %>% 
  summarise(mean_value = mean(val_bodyMass_kg, na.rm = TRUE),
    sd = sd(val_bodyMass_kg, na.rm = TRUE),
    min = min(val_bodyMass_kg, na.rm = TRUE),
    max = max(val_bodyMass_kg, na.rm = TRUE))
weights

ggplot(weight, aes(x = year, y = val_bodyMass_kg, group = year)) +
  geom_boxplot(notch = FALSE, outlier.colour = "red", outlier.shape = 8) +
  labs( title = "SPEI body masses during late incubation 2021-2026",
    x = "Year",
    y = "body weight (kg)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12))


#pre-breed weights
band1 <- sqlFetch(con_db, "tbl_Band")
pre <- band1 %>% filter(cat_periodCapt=="arrival") %>% 
  filter(!(val_bodyMass_kg=="-9")) %>% filter(cat_sex == "female") %>%
  filter(cat_species == "SPEI")
pre1 <- pre %>% 
  summarise(mean_value = mean(val_bodyMass_kg, na.rm = TRUE),
            sd = sd(val_bodyMass_kg, na.rm = TRUE),
            min = min(val_bodyMass_kg, na.rm = TRUE),
            max = max(val_bodyMass_kg, na.rm = TRUE))
pre1
boxplot(pre$val_bodyMass_kg)



#graph of period when birds were captured
  #work in progress - want a bar per capture period but need to break brood captures into local and AHY (vertically preferably) 
period <- band %>% group_by(year, cat_periodCapt,cat_age) %>%
  summarise(count = n(), .groups = "drop_last") %>% 
  ungroup()
period

t <- period %>%
  complete(year, cat_periodCapt, cat_age, fill = list(n = 0))

df_long <- t %>%
  pivot_longer(cols = c(cat_periodCapt, cat_age),
               names_to = "variable",
               values_to = "value")
df_long

ggplot(df_long, aes(x = year, y = count, fill = variable)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, colour = "black") +
  geom_text(aes(label = value),
            position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  labs(x = "Year", y = "count") +
  theme_minimal()


ggplot(period, aes(x = year, y = count, fill = cat_periodCapt)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  labs(title = "Captured SPEI 2021-2026 by capture period", x = "year", y = "count") +
  theme_minimal() +
  labs(fill = "capture period")






####temperature loggers####
db <- "data/db_kig_nesting_waterfowl_2019-2026_compiled_20260901.accdb"
con_db<-odbcConnectAccess2007(db)
sqlTables(con_db,tableType="TABLE")$TABLE_NAME
nest <- sqlFetch(con_db, "tbl_Nest")
loggers <- sqlFetch(con_db, "tbl_iButton")
nest$Year <- as.numeric(format(as.Date(nest$dt_found, format="%m/%d/%Y"), "%Y"))
logger <- full_join(nest, loggers)

logger <- logger %>% filter(!(Year=="2019"))

#SPEI first, then comment out and get COEI
logger <- logger %>% filter(cat_species=="SPEI")
# logger <- logger %>% filter(cat_species=="COEI")

logger <- logger %>% filter(!is.na(id_iButton)) #252
logger.year <- logger %>% group_by(Year) %>%
  summarise(count = n(), .groups = "drop_last") %>% 
  ungroup()
logger.year



####annual nest loc maps####
install.packages("magick")
library(magick)

# Read in the six saved images
img1 <- image_read("output/maps/nests_2021.png") %>% image_trim()
img2 <- image_read("output/maps/nests_2022.png") %>% image_trim()
img3 <- image_read("output/maps/nests_2023.png") %>% image_trim()
img4 <- image_read("output/maps/nests_2024.png") %>% image_trim()
img5 <- image_read("output/maps/nests_2025.png") %>% image_trim()
img6 <- image_read("output/maps/nests_2026.png") %>% image_trim()

#arrange grid
row1 <- image_append(c(img1, img2))
row2 <- image_append(c(img3, img4))
row3 <- image_append(c(img5, img6))
combined <- image_append(c(row1, row2, row3), stack = TRUE)
combined

# Save the combined figure
# image_write(combined, "output/maps/nest_locs_2021-2026.png")





####COEI nests####
db <- "data/db_kig_nesting_waterfowl_2019-2026_compiled_20260923.accdb"
con_db<-odbcConnectAccess2007(db)
sqlTables(con_db,tableType="TABLE")$TABLE_NAME
coei.nest <- sqlFetch(con_db, "tbl_Nest")
coei.hatch <- sqlFetch(con_db, "qry_estHatch")
coei.band <- sqlFetch(con_db, "tbl_Band")
coei.visit <- sqlFetch(con_db, "tbl_Visit")
coei.tissue <- sqlFetch(con_db, "tbl_TissueSamples")


coei.nest$Year <- as.numeric(format(as.Date(coei.nest$dt_found, format="%m/%d/%Y"), "%Y"))
coei.nest$Year <- as.factor(coei.nest$Year)
#keep only COEI nests
coei.nest <- coei.nest %>% filter(cat_species == "COEI")
coei.nest <- coei.nest[coei.nest$id_nest != "LLK003", ]
coei.hatch <- coei.hatch %>% filter(cat_species == "COEI")
coei.hatch <- coei.hatch[coei.hatch$id_nest != "LLK003", ]

#condense fates to hatch=0, failed=1
coei.nest %>%
  distinct(cat_fate) %>%   # Keep only unique names
  pull(cat_fate)    
coei.nest <- coei.nest %>%
  mutate(cat_fate = case_when(
    cat_fate == "eggs_missing" ~ "1",
    cat_fate == "eggs_destroyed" ~ "1",
    cat_fate == "eggs_inviable" ~ "1",
    cat_fate == "eggs_abandoned" ~ "1",
    cat_fate == "active_lastcheck" ~ "0",
    cat_fate == "hatched" ~ "0",
    TRUE ~ cat_fate )) #keep other values as they are 


#COEI init dates
  #use the hatch date (corrected for COEI), or the estimated float/candle age (needs corrected for COEI)
merge <- left_join(coei.nest, coei.visit, by = "id_nest")
merge <- merge %>% filter(id_species == "COEI")
merge <- merge[merge$id_nest != "LLK003", ]
unique(merge$id_nest) #216 nests



#get the eggs ages to estimate hatch date
visit <- merge %>% mutate(float_age1 = case_when(
  cat_float1 == "SF"   ~ "2",
  cat_float1 == "SA"  ~ "5",
  cat_float1 == "SO"   ~ "8",
  cat_float1 == "SP"   ~ "11",
  cat_float1 == "FN"  ~ "13",
  cat_float1 == "FS"   ~ "15",
  cat_float1 == "FB"   ~ "18",
  cat_float1 == "FO"  ~ "21",
  cat_float1 == "FA"   ~ "22",
  cat_float1 == "PS"   ~ "23",
  cat_float1 == "PP"  ~ "24",
  TRUE ~ "NA"))

visit <- visit %>% mutate(float_age2 = case_when(
  cat_float2 == "SF"   ~ "2",
  cat_float2 == "SA"  ~ "5",
  cat_float2 == "SO"   ~ "8",
  cat_float2 == "SP"   ~ "11",
  cat_float2 == "FN"  ~ "13",
  cat_float2 == "FS"   ~ "15",
  cat_float2 == "FB"   ~ "18",
  cat_float2 == "FO"  ~ "21",
  cat_float2 == "FA"   ~ "22",
  cat_float2 == "PS"   ~ "23",
  cat_float2 == "PP"  ~ "24",
  TRUE ~ "NA"))

#add in the candle age for any nests?
visit$float_age1 <- as.numeric(visit$float_age1)
visit$float_age2 <- as.numeric(visit$float_age2)


#choose the largest value for age and estimated hatch date
age <- visit %>%  group_by(id_nest) %>%
  mutate(age = pmax(float_age1, float_age2, na.rm = TRUE)) %>% 
  ungroup()
age <- age %>% mutate(init=dt_visit - as.difftime(age, units = "days")) %>% 
  mutate(hatch=init+ as.difftime(26, units = "days"))   #use 24 for SPEI and 26 here for COEI!


#pull out just one row per nest so no longer multiple visits
#should get 216 nests like above
age <- age %>% 
  group_by(id_nest) %>%
  slice_min(rowSums(is.na(across(everything()))), with_ties = FALSE) %>%
  ungroup()


#those are the float/candle estimated hatch and init dates
  #but nests with "observed" hatch should take precedence over float/candle estimated dates
hatch <- age %>%
  mutate(hatch = if_else(cat_hatchDate %in% c("observed", "estimated", "nest_temp") & !is.na(dt_Hatch),
                         dt_Hatch,
                         hatch,
                         missing = hatch),
                         init = if_else(cat_hatchDate %in% c("observed", "estimated", "nest_temp") & !is.na(dt_Hatch),
                                        hatch - as.difftime(26, units = "days"),
                                        init,
                                        missing = init))
names(hatch)
hatch <- hatch %>% select(id_nest, dt_found, dt_lastALive, dt_lastCheck, cat_fate, init) %>% 
  mutate(across(c(dt_found, dt_lastALive, dt_lastCheck, init), 
                ~ as.numeric(format(., "%j"))))


#change names and adjust for prep for nest survival
hatch <- hatch %>% rename(FirstFound=dt_found, LastPresent=dt_lastALive, LastChecked=dt_lastCheck, Fate=cat_fate) 
#standardize to earliest FirstFound date of the season (143)
stand <- hatch %>% mutate(across(c(FirstFound, LastPresent, LastChecked), 
                ~ . - 142))

#add in AgeDay1, which is the earliest date (142) minus the init date
stand <- stand %>% mutate(AgeDay1=142-init)
names(stand)

#remove any nests that were hatched at the time of being found or not monitored with one visit
stand <- stand %>% filter(!(id_nest=="AJL090"))
stand <- stand %>% filter(!(id_nest=="DJR021"))
stand <- stand %>% filter(!(id_nest=="DJR022"))
stand <- stand %>% filter(!(id_nest=="DJR025"))
stand <- stand %>% filter(!(id_nest=="DJR026"))
stand <- stand %>% filter(!(id_nest=="ERH002"))
stand <- stand %>% filter(!(id_nest=="SAH070"))
stand <- stand %>% filter(!(id_nest=="SAH073"))
stand <- stand %>% filter(!(id_nest=="SAH074"))

#add in dates for nests that were just candled and not floated
  #age estimated code above should really take into account both float and candle...
stand$init[stand$id_nest == "AJL060"] <- 181
stand$init[stand$id_nest == "HLV057"] <- 167
stand$init[stand$id_nest == "SAH064"] <- 162
stand$AgeDay1[stand$id_nest == "AJL060"] <- 142-181
stand$AgeDay1[stand$id_nest == "HLV057"] <- 142-167
stand$AgeDay1[stand$id_nest == "SAH064"] <- 142-162


# write.csv(stand, "data/COEI/COEI_2026_inp.csv")



##membrane count##
membrane <- visit %>% group_by(id_nest) %>% 
  slice_max(order_by = n_eggMembrane, n = 1, with_ties = FALSE) %>%
  filter(!(n_eggMembrane == 0))      #get 104 here, there may have been repeat visits to nests where membranes were counted

#number of nests per year with membranes
mean(membrane$n_eggMembrane)   #3.9519
median(membrane$n_eggMembrane)   #4
min(membrane$n_eggMembrane)   #1
max(membrane$n_eggMembrane)   #7




####COEI nest survival####
library(RMark)
# Add path to MARK.exe
Sys.setenv(MarkPath = "C:/Program Files (x86)/MARK")
mark.path <- "C:/Program Files (x86)/MARK"
options(mark.path = mark.path)

coei_data <- read_csv("data/COEI/COEI_2026_inp.csv")

# Check the headers. Sometimes column name changes when reading csv files. 
head(coei_data)
summary(coei_data)
names(coei_data)

str(coei_data)
coei_data <- as.data.frame(coei_data)

setwd("C:/Users/shoepfner/Documents/spei_kig_2021-2026_annualReport/data/COEI")

#NOCC=36, LastCheck=36


run.models_1 = function()
{
  # 1. constant daily survival rate model (null)
  S.dot = mark(coei_data, nocc = 36, model = "Nest", model.name = "S.dot",
               # ddl= nest.ddl,
               model.parameters = list(S=list(formula = ~ 1)))

  S.syin = mark(coei_data, nocc = 36, model = "Nest", model.name = "S.syin",
                # ddl= nest.ddl,
                model.parameters = list(S=list(formula = ~1 + init + NestAge)))

  # Return model table and list of models
  
  return(collect.models())
}

# Fit models
model.results_coei=run.models_1()

# Look at model results
model.results_coei

model.results_coei$S.syin$results$beta


top_model_2 <- model.results_coei$S.syin   #USE this one!top model and includes covariates we care about
ffc2 <- find.covariates(top_model_2, coei_data)


ffc2 <- ffc2 %>%
  mutate(
    nestage_num = as.numeric(str_extract(var, "(?<=NestAge)\\d+")),
    value = ifelse(!is.na(nestage_num), nestage_num, value),
    age   = as.numeric(str_extract(rnames, "(?<=a)\\d+")))

#init values
#use the Julian date of the annual mean
coei_data 
coei_data %>% 
  summarise(mean = mean(init), .groups = "drop")
#148.9469

ffc2 <- ffc2 %>%
  mutate(value = ifelse(var == "init", 148.9469, value))

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


# Find the rows for your single year/site, ages 0:23
rows <- which(ffc2_init_only$age %in% 1:24)

# Sanity check: should be exactly 24 rows (ages 0 through 23)
if (length(rows) != 24) {
  stop("Expected 24 rows for ages 0:23, but found ", length(rows))
}

# Cumulative DSR product
product_result <- prod(ffull.survival$real[rows])

# Delta-method SE
se_result <- deltamethod.special(
  "prod", ffull.survival$real[rows], ffull.survival$vcv.real[rows, rows])

product_result
se_result
final_results <- data.frame(
  product = product_result,
  se = se_result)


#add in logit-scale 95% CIs
final_results <- final_results %>%
  mutate(
    logit_est = qlogis(product),
    logit_se  = se / (product * (1 - product)),
    lci = plogis(logit_est - 1.96 * logit_se),
    uci = plogis(logit_est + 1.96 * logit_se))
final_results



# save new data frame as csv to not need to run model set 1 again
# write.csv(final_results, "COEI_2026_nest_survival.csv")








####CHECK THESE SUMMARIES WITH NEW CODE####

mean(coei.hatched$Hatch_Date) #25 June
median(coei.hatched$Hatch_Date) #24 June
mean(coei.hatched$init) #30 May
range(coei.hatched$init) #21 May to 20 June
median(coei.hatched$init)  #29 May



#COEI clutch sizes
  #get max egg numbers seen per nest
##prep the tbl_Visit to combine the clutch files
coei.visit <- sqlFetch(con_db, "tbl_Visit") 
coei.visit <- coei.visit %>% filter(id_species == "COEI")  #636 visits
coei.visit <- coei.visit %>% mutate(year = lubridate::year(dt_visit))
coei.visit$year <- as.factor(coei.visit$year)
coei.visit$n_eggsTotal <- as.numeric(coei.visit$n_eggsTotal)

coei.visit %>% summarise(na_count = sum(is.na(n_eggsTotal)))
  #f visits with no egg info- bird did not flush and ignore these

#find most "true" clutch size
#need to find the visit that has the most eggs per nest
unique(coei.visit$id_nest)   #217 nests
coei.eggs <- coei.visit %>% group_by(id_yrNest) %>% 
  slice_max(order_by = n_eggsTotal, n = 1, with_ties = FALSE)

#exclude nests that had less than three eggs and an early float suggesting still in lay
  #removed 10 nests
coei.eggs_3 <- coei.eggs %>% filter(!(n_eggsTotal <= 3 & cat_float1 == "SF"))

merged <- coei.hatched %>%
  left_join(coei.eggs_3 %>% select(id_yrNest, n_eggsTotal), by = "id_yrNest")
merged$id_yrNest[which(!complete.cases(merged$n_eggsTotal))]
#NA is for clutches that were found in lay = 10, remove them
merged <- merged[!is.na(merged$n_eggsTotal), ]
merged <- merged %>% filter(!(n_eggsTotal==0))
coei.clutch <- merged


##Clutch counts
#number of eggs in a clutch per year
coei.clutch %>% group_by(Year) %>%
  summarise(n = n()) #204
#overall average
mean(coei.clutch$n_eggsTotal)   #5.304
#annual median
coei.clutch %>% group_by(Year) %>%
  summarise(median_value = median(n_eggsTotal))   #5
#annual mode
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
coei.clutch %>% group_by(Year) %>%
  summarise(mode_value = find_mode(n_eggsTotal))  #6


#plots
plot_clutch <- coei.clutch %>% group_by(n_eggsTotal) %>%
  summarise(n = n())
plot_clutch$n_eggsTotal <- as.factor(plot_clutch$n_eggsTotal)
plot_clutch$n <- as.numeric(plot_clutch$n)
ggplot(plot_clutch, aes(x = n_eggsTotal, y = n)) +
  geom_col(width = 0.7) +
  labs(x = "Clutch size",
    y = "Number of nests",
    title = "COEI clutch size distribution") +
  theme_minimal(base_size = 14) +
  geom_vline(xintercept = 5.4, linetype="dashed")
str(plot_clutch)



#nest site habitat
names(coei.hatched)

coei.site <- coei.hatched %>% group_by(cat_nestSite) %>%
  mutate(cat_nestSite = tolower(cat_nestSite))
coei.site.yr <- coei.site %>% group_by(Year, cat_nestSite) %>% 
  summarise(n=n())
coei.site.yr

ggplot(coei.site.yr, aes(x = Year, y = n, fill = cat_nestSite)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    # title = "COEI clutch sizes 2019-2025",
    x = "Year",
    y = "Count",
    fill = "nest site" ) +
  theme_minimal(base_size = 14) 


#same as above but as a percentage
coei.site_pct <- coei.site %>%
  group_by(cat_nestSite) %>%
  summarise(count = n(), .groups = "drop_last") %>% 
  mutate(pct = round(100 * count / sum(count), 2)) %>% 
  ungroup()

ggplot(coei.site_pct, aes(y = pct, fill = cat_nestSite)) +
  geom_col() +
  geom_text(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 16) +
  ylab("Percentage") +
  xlab(NULL)



###number bled in 2026
coei.band 
coei.band <- coei.band %>% filter(cat_species=="COEI")
coei.nest <- coei.nest %>% filter(cat_species=="COEI")
coei.tissue
coei.blood <- merge(coei.band, coei.tissue, by = "id_metalBand", all = TRUE) %>%
  filter(cat_species=="COEI")

coei.blood <- coei.blood %>% 
  filter(!is.na(amt_bloodVol))
unique(coei.blood$id_metalBand)  #56 COEI had blood collected, three were during pre-breeding


#how many COEI banded in 2026 were banded on plots?
coei.band.plot <- full_join(coei.nest, coei.band, by = "id_nest") %>%
  distinct(id_nest, .keep_all = TRUE)
coei.band.plot <- coei.band.plot %>% 
  filter(cat_periodCapt=="incubation")  #should be 66
coei.band.plot <- coei.band.plot %>% 
  filter(is_onPlot=="yes")    #49 banded on plot, 74%
  




####combine in with Randall's nests 1992-2019####
  #older code and not necessary for the report
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
all_nests <- read_csv("data/all_nests_2019-2026.csv")
names(all_nests)
all_nests <- all_nests %>% 
  rename(maxclutch=n_eggsTotal, Init=init, Fate=cat_fate)
all_nests$Init <- as.POSIXlt(all_nests$Init)$yday
all_nests <- all_nests %>% select(maxclutch, Init, Year, id_yrNest, Fate)
str(all_nests)
names(old_nests)

allll <- bind_rows(old_nests, all_nests)
#just the five rows that both datasets have

# write.csv(allll, "C:/Users/shoepfner/Desktop/summary_nests/all_nests_1994-2026.csv")




