#Spectacled Eider nest abundance estimates at Kigigak Island, 2003-2026
  #Sarah Hoepfner, October 2026, presented at 2026 Alaska Bird Conference
    #determining nest count estimates by using raw nest counts, effort of plots searched,
      #and use DSR to correct for nests that failed before being found
    #only looked at years 2003-2026 because same plot design
#all files should be saved within the R Project spei_kig_nestAbund_2003-2026

library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(RODBC)
library(ggplot2)
library(plotly)
library(sf)
library(mase)
library(MASS)
library(dplyr)

#select function was misbehaving for me
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

#set working directory to the project folder, wherever it is saved
setwd("C:/Users/shoepfner/Documents/spei_kig_nestAbund_2003-2026")


####ON plot nests####
#have to see if nest point is within the plot polygons
  #code in the below drop down determines which nests are On/OFF the plots 2003-2026, same filtering process for all nests
    #don't need to run it all now, but that is how the "locations" file was made
  #then add it to the cleaned data below, and then combine DSR
    #ONLY need the nestID and plot info, lots of NAs in other columns
locations <- read.csv("data/plots_combined_header_2003-2026.csv")
locations <- locations %>% filter(within_plot=="On")
#remove nests that were on the "productivity plots"
locations <- locations %>% filter(!(PHOTO < 100 | is.na(PHOTO))) #should be 1549 nests
  #want the plot id names from this file too, more accurate than original 
unique(locations$plot_name[is.na(as.numeric(locations$plot_name)) & !is.na(locations$plot_name)])


#can skip the below code about the behind the scenes of how plots were deemed ON plot



####define ON plot----
#behind the scenes code to determine if a nest is on plot or not 2003-2026
#cool code plotting all nest locations

loc_old <- read_csv("data/combined_header_data_1992-2015.csv")
names(loc_old)
loc_old <- loc_old %>% filter(SPECIES=="SPEI")   #2,759 nests

#remove any NAs
loc_old <- loc_old %>% 
  filter(!is.na(LAT))  #removes 490
loc_old <- loc_old %>% rename(val_lon_nest=LON, val_lat_nest=LAT, id_yrNest=HEADER_ID)  #2269 nests
#can only go back to 2003 since that year shows the same plot structure as it is now and use the same google earth polygons
loc_old <- loc_old %>% filter(Year >= 2003)  #1683 nests
#remove nests that are off the island
loc_old <- loc_old %>% filter(!(TRUE_LOCATION=="no")) #1668 nests

#add in recent years
loc_new <- read.csv("data/db_kig_nesting_waterfowl_2019-2026_compiled_20260923.csv")
loc_new <- loc_new %>% filter(cat_species=="SPEI")   #515 nests
loc_new$Year <- as.numeric(format(as.Date(loc_new$dt_found, format="%m/%d/%Y"), "%Y"))
names(loc_new)   #515 nests

#merge files
loc <- full_join(loc_old, loc_new, by = c("Year", "id_yrNest", "val_lat_nest", "val_lon_nest"))  #2183 nests
names(loc)   #2183 nests



# Points — nest/GPS dataset
points_sf <- st_as_sf(loc, coords = c("val_lon_nest", "val_lat_nest"), 
                      crs = 4326, remove = FALSE)
points_sf <- points_sf %>% select(id_yrNest, Year, val_lat_nest, val_lon_nest)

#read in from the Google earth file of plots
unzip("data/ykd_NestMonitoringPlots.kmz", exdir = "kmz_extracted")
polygons <- st_read("kmz_extracted/doc.kml")
st_layers("kmz_extracted/doc.kml") #48 layers
plot(st_geometry(polygons)) #quick visual check

# Ensure same CRS before joining
polygons_utm <- st_transform(polygons, crs = 32603)
  #add 5m buffer because there are gaps between plots, but not treated like that in the field
polygons_utm_buffered <- st_buffer(polygons_utm, dist = 5)  #5m buffer
polygons_buffered <- st_buffer(polygons_utm, dist = 5)
polygons <- polygons_utm_buffered

polygons <- st_transform(polygons, crs = st_crs(points_sf))
#add 5m buffer again here
polygons <- st_buffer(polygons, dist = 5)

# Spatial join: attaches polygon attributes to each point that falls inside it
joined <- st_join(points_sf, polygons, join = st_within)  #2279 nests

joined %>%
  count(id_yrNest) %>%
  filter(n > 1) %>%
  nrow()   #91 nests that have duplicated or exist on two plots because of the buffer

#remove the duplicates now, 2007_JBW085 and 2010_H003 are true duplicates
#pull one of the plots
joined <- joined %>% 
  distinct(id_yrNest, .keep_all = TRUE)   #2183 nests

loc <- loc %>%
  mutate(plot_name = joined$Name) %>%
  mutate(within_plot = ifelse(is.na(plot_name), "Off", "On"))

# write.csv(loc, "data/plots_combined_header_2003-2026.csv")


#plot the locations out and filter by different groupings to show it looks ok
locations <- loc
# locations <- read.csv("data/plots_combined_header_2003-2026.csv")
names(locations)


loc_on <- locations %>% filter(within_plot=="On")
loc_off <- locations %>% filter(within_plot=="Off")

#change colors of polygons to see overlapping boundaries better
# polygons <- polygons %>%
#   mutate(plot_index = row_number(),
#          group3 = as.factor((plot_index - 1) %% 3))  # gives 0, 1, 2 repeating

#all nests
all <- ggplot() +
  geom_sf(data = polygons, fill = NA, color = "blue") +
  geom_point(data = locations, aes(x = val_lon_nest, y = val_lat_nest), color = "red", size = 0.8) +
  theme_minimal()
ggplotly(all) #can zoom in with the plotly function

#nests ON plot
on <- ggplot() +
  geom_sf(data = polygons, fill = NA, color = "blue") +
  geom_point(data = loc_on, aes(x = val_lon_nest, y = val_lat_nest), color = "red", size = 0.8) +
  theme_minimal()
ggplotly(on)

#nests OFF plot
off <- ggplot() +
  geom_sf(data = polygons, fill = NA, color = "blue") +
  geom_point(data = loc_off, aes(x = val_lon_nest, y = val_lat_nest), color = "red", size = 0.8) +
  theme_minimal()
ggplotly(off)










####prep the data####
  #use older Excel and newer Access database files
  #only pull nests that are located in the locations file

#have to bring in the Access file, merge and filter by nests that are only On plot
newer <- read.csv("data/db_kig_nesting_waterfowl_2019-2026_compiled_20260923.csv")
#only keep nests that are in the "locations" file
locations <- read.csv("data/plots_combined_header_2003-2026.csv")
newer <- newer %>%
  inner_join(locations %>% select(id_yrNest, plot_name), by = "id_yrNest") %>% 
  filter(cat_species == "SPEI") %>%
  filter(!(plot_name == "NA")) #should be 351 ON plot SPEI nests 2019-2026
newer$Year <- as.numeric(format(as.Date(newer$dt_found, format="%m/%d/%Y"), "%Y"))
newer <- newer %>% select(id_yrNest, Year, dt_found, plot_name)
names(newer)
newer$dt_found <- as.Date(newer$dt_found, format = "%m/%d/%Y")
# newer$dt_found <- as.POSIXlt(newer$dt_found)$yday
str(newer)





#merge in the earlier database file for 2003-2015 nests on plot
older <- read.csv("data/combined_header_data_1992-2015.csv")
names(older)
older <- older %>% rename(id_yrNest=HEADER_ID) 

#only keep nests that are in the "locations" file
older <- older %>%
  inner_join(locations %>% select(id_yrNest, plot_name), by = "id_yrNest") %>% 
  filter(SPECIES == "SPEI")  %>%
  filter(!(plot_name == "NA"))  #should be 1277 ON plot SPEI nests 2003-2015

older <- older %>% 
  mutate(id_yrNest = str_remove_all(id_yrNest, "_"))

#add in the real id_yrNest, INP file has weird name scheme
  #need to do this so it matches the INP file
older <- older %>% 
  mutate(
    id_first = str_extract(NEST_NO, "^[A-Za-z]+"),
    id_remaining = str_remove(NEST_NO, "^[A-Za-z]+"),
    zeroes = strrep("0", pmax(3 - nchar(id_first), 0)),
    id_yrNest = case_when(
      Year %in% 2003:2008 ~ paste0(Year, id_first, id_remaining),
      Year %in% 2009:2011 ~ paste0(Year, id_first, zeroes, id_remaining),
      Year %in% 2012:2013 ~ paste0(Year, id_first, zeroes, id_remaining),
      Year %in% 2014:2015 ~ paste0(Year, NEST_NO),
      TRUE ~ NEST_NO))

older <- older %>% select("id_yrNest", "Year", "plot_name")
older$Year <- as.numeric(older$Year)
names(older)
str(older)



#combine the two data files for early and recent nests
combine <- bind_rows(newer, older)  #1628 nests
combine %>% group_by(Year) %>% summarise(n=n())


#add in the INP file for nests
inp <- read.csv("data/spei_kig_utq_INPnestSurvival_1994-2026_ssa_revision_final.csv")
names(inp)
#combine with the nest data for on plot nests, keep nests even if it doesn't have nest survival data
str(inp)
str(combine)
merged <- combine %>%
  left_join(inp, by = c("Year", "id_yrNest"))
sum(is.na(merged$FirstFound))    #175

names(merged)
merged <- merged %>% select(-age, -X, -Freq, -Site)

merged <- merged %>% filter(!(id_yrNest %in% c("2024LAH001", "2021CLB107")))



####Horvitz-Thompson estimator####
ht_data <- merged

###STEP 1:
  #build ht_data from nest data
ht_data <- ht_data %>%
  select(
    id_yrNest,
    Year,
    init,
    AgeDay1,
    FirstFound,
    LastPresent,
    LastChecked,
    plot_name)

ht_data$Year <- as.numeric(ht_data$Year)
ht_data$n_plots_searched <- 
                            if_else(ht_data$Year==2003,43,
                            if_else(ht_data$Year==2004,46,
                            if_else(ht_data$Year==2005,46,
                            if_else(ht_data$Year==2006,48,
                            if_else(ht_data$Year==2007,48,
                            if_else(ht_data$Year==2008,48,
                            if_else(ht_data$Year==2009,45,
                            if_else(ht_data$Year==2010,34,
                            if_else(ht_data$Year==2011,33,
                            if_else(ht_data$Year==2012,48,
                            if_else(ht_data$Year==2013,35,
                            if_else(ht_data$Year==2014,42,
                            if_else(ht_data$Year==2015,35,
                            if_else(ht_data$Year==2019,48,
                            if_else(ht_data$Year==2021,48,
                            if_else(ht_data$Year==2022,48,
                            if_else(ht_data$Year==2023,48,
                            if_else(ht_data$Year==2024,48,
                            if_else(ht_data$Year==2025,48,
                            if_else(ht_data$Year==2026,48,100))))))))))))))))))))

ht_data$init <- as.numeric(ht_data$init)
ht_data$FirstFound <- as.numeric(ht_data$FirstFound)
ht_data$LastPresent <- as.numeric(ht_data$LastPresent)
ht_data$LastChecked <- as.numeric(ht_data$LastChecked)

#count how many NAs from the nest survival input
ht_data %>% group_by(Year) %>% 
  summarise(na_count = sum(is.na(LastPresent))) 
sum(is.na(ht_data$LastPresent))  #173 out of 1626 missing... 10.6%


###STEP 2: 
  #now add in the nest survival results from fitted model
load("data/spei_kig_utq_nestSurvival_1994-2026_topModel2_results_final.RData")
  #make a copy of this and move into the nest density folder too....
row_labels <- top_model_2$design.data$S



###STEP 3:  
  #calcualte age at time of that nest's detection, age_at_detection
ht_data1 <- ht_data %>%
  mutate(age_at_detection = pmax(AgeDay1 + (FirstFound - 1), 0))
  #use pmax to set the limit at 0, no nests can be negative ages at detection (in lay)
    #2024NSL102 and 2010MW0081



###STEP 4:
  #define the lookup function
    #guard clause returns NA for nests missing dates, will be filled in later
  #need to pull out all parameters that are part of the model (Year, age, init)
compute_dsr <- function(site, year, init_val, nestage_val, betas) {
  siteyear_name <- paste0("S:Site", site, ":Year", year)
  
  b_siteyear <- betas$estimate[rownames(betas) == siteyear_name]
  b_init     <- betas$estimate[rownames(betas) == "S:init"]
  b_nestage  <- betas$estimate[rownames(betas) == "S:NestAge"]
  
  if (length(b_siteyear) != 1) {
    stop(paste("No unique match for:", siteyear_name))
  }
  
  logit_s <- b_siteyear + b_init * init_val + b_nestage * nestage_val
  plogis(logit_s)
}

compute_survival_to_detection <- function(yr, init_val, age_at_det, betas, site = "kig") {
  if (is.na(age_at_det)) return(NA)
  if (age_at_det == 0) return(1)
  
  dsrs <- sapply(0:(age_at_det - 1), function(a) {
    compute_dsr(site, yr, init_val, a, betas)
  })
  
  prod(dsrs)
}


###STEP 5: 
  #apply the lookup function to every nest with complete data
ht_data1 <- ht_data1 %>%
  rowwise() %>%
  mutate(
    p_survived_to_detection = compute_survival_to_detection(
      yr = Year,
      init_val = init,
      age_at_det = age_at_detection,
      betas = top_model_2$results$beta)) %>%
  ungroup()


#check how many nests are missing a value
sum(is.na(ht_data1$p_survived_to_detection)) #should be 173



###STEP 6:
  #pull in the average NS values per year for nests missing them
  #use the Year average of complete nests
    #not average dates but average final probabilities 
    #could make it more accurate by grouping nests by early, middle, late in the year and using their DSR?
annual_avg_pi <- ht_data1 %>%
  filter(!is.na(p_survived_to_detection)) %>%
  group_by(Year) %>%
  summarise(avg_pi = mean(p_survived_to_detection, na.rm = TRUE), .groups = "drop")

ht_data1 <- ht_data1 %>%
  left_join(annual_avg_pi, by = "Year") %>%
  mutate(
    p_survived_to_detection_filled = ifelse(
      is.na(p_survived_to_detection), avg_pi, p_survived_to_detection))

#confirm no NAs remain
sum(is.na(ht_data1$p_survived_to_detection_filled))


#check the weights of nests, use the 95% quantile for each year
ht_data1 %>%
  mutate(weight = 1 / p_survived_to_detection_filled) %>%
  # group_by(Year) %>%  #comment this to view annual/combined values
  summarise(
    quantile(weight, probs = 0.95))   #95% quantile is 1.46 across all years, very small!
#2022 has three nests with weights over 10 


###STEP 7:
#set y, the quantity being summed, 1 per detected nest
  #y always equals 1
ht_data1 <- ht_data1 %>%
  mutate(y = 1)



#need total plots and plots searched per year
  #pull the correct n_plots_searched per year directly from ht_data1

plots_searched_by_year <- ht_data1 %>%
  group_by(Year) %>%
  summarise(
    n_plots_searched = dplyr::first(n_plots_searched),
    .groups = "drop")

#get nest counts per plot per year (from nest-level data)
  # NOTE: this only includes plots with >= 1 nest. Plots searched but with
  # zero nests are NOT represented here, because ht_data1 is nest-level --
  # a plot with no nests produces no rows. This means var_plot_total below
  # is likely upwardly biased (see caveat after the code).

plot_counts <- ht_data1 %>%
  group_by(Year, plot_name) %>%
  summarise(n_nests = n(), .groups = "drop")

TOTAL_PLOTS <- 48

#summarize per-plot stats and then bring in the correct n_plots_searched
year_summary_final <- plot_counts %>%
  group_by(Year) %>%
  summarise(
    mean_plot_total_raw = mean(n_nests),   # mean across PLOTS WITH NESTS ONLY -- biased
    var_plot_total      = var(n_nests),    # variance across PLOTS WITH NESTS ONLY -- biased
    n_plots_with_nests  = n(),
    total_nests         = sum(n_nests),
    .groups = "drop") %>%
  left_join(plots_searched_by_year, by = "Year") %>%
  mutate(
    # Correct mean: total nests / TRUE number of plots searched (not just plots with nests)
    mean_plot_total = total_nests / n_plots_searched,
    fpc             = 1 - (n_plots_searched / TOTAL_PLOTS),
    annual_total    = TOTAL_PLOTS * mean_plot_total,
    # var_annual still uses var_plot_total, which is biased upward -- see caveat below
    var_annual      = (TOTAL_PLOTS^2) * (var_plot_total / n_plots_searched) * fpc,
    se_annual       = sqrt(var_annual)) %>%
  select(Year, mean_plot_total, var_plot_total, n_plots_searched, fpc,
         annual_total, var_annual, se_annual)

year_summary_final
#this dataframe gets used later on and sometimes you have re-run this above section







####STEP 8:
#can run the code or read in the results file below
  #use this instead of or alongside Step 10's se_annual when plot coverage is complete
    #aka fpc=0 for all years and all 48 plots searched, aka 2019-2026 only
  #this captures uncertainty from nest level detection (resampling which nests appear within each plot)
  #also has perturbations for accounting for uncertainty in DSR values
n_boot <- 1000

#Delta method to transform the matrix to the logit scale
  #jacobian term is the derivative of the logit function
    #correctly propagates the original uncertainty onto the new transformed scale
logit_real <- qlogis(ffull.survival$real)   # point estimates → logit scale

jacobian <- as.vector(1 / (ffull.survival$real * (1 - ffull.survival$real)))
logit_vcv <- diag(jacobian) %*% ffull.survival$vcv.real %*% diag(jacobian)
  #this later gets reduced to its k real dimensions via eigen-decomposition
    #preserves all ther real uncertainty while skipping the redundant computation


#Precompute a fixed per-year weight cap, based on the 95th percentile of
# weights from the ORIGINAL (non-perturbed) point-estimate probabilities.
# This cap stays the same across all bootstrap reps.
weight_caps <- ht_data1 %>%
  mutate(weight_raw = 1 / p_survived_to_detection_filled)
weight_caps  #now the largest value is only 5

ht_data1 %>% 
  mutate(weight_raw = 1 / p_survived_to_detection_filled) %>%
  summarise(weight_cap = quantile(weight_raw, 0.95, na.rm = TRUE), .groups = "drop")
#95% CI is 1.46


# --- Setup before the loop (replaces your logit_real/vecs/sqrt_vals setup) ---
betas_df <- top_model_2$results$beta
mean_betas <- betas_df$estimate
vcv <- top_model_2$results$beta.vcv   # check exact name via names(top_model_2$results)

eig <- eigen(vcv)
k <- sum(eig$values > 1e-8)           # drop near-zero eigenvalues, same reduced-rank logic you had
vecs <- eig$vectors[, 1:k, drop = FALSE]
sqrt_vals <- sqrt(eig$values[1:k])

boot_results <- vector("list", n_boot)



for (b in seq_len(n_boot)) {
  
  # --- (a) Perturb BETAS using reduced-rank draw ---
  z <- rnorm(k)
  perturbed_betas_vec <- mean_betas + as.vector(vecs %*% (sqrt_vals * z))
  
  perturbed_betas <- betas_df
  perturbed_betas$estimate <- perturbed_betas_vec
  
  # --- (b) Recompute survival-to-detection using perturbed betas, per nest ---
  boot_data <- ht_data1 %>%
    rowwise() %>%
    mutate(
      p_boot = compute_survival_to_detection(
        yr = Year,
        init_val = init,
        age_at_det = age_at_detection,
        betas = perturbed_betas
      )
    ) %>%
    ungroup() %>%
    mutate(p_boot = ifelse(is.na(p_boot), p_survived_to_detection_filled, p_boot))
  
  # --- (c) Cap weights ---
  boot_data <- boot_data %>%
    mutate(weight_cap = 1.46) %>%   # using 6 (no effective cap); 95th pct cap would be 1.47
    mutate(
      weight_raw    = 1 / p_boot,
      weight_capped = pmin(weight_raw, weight_cap),
      pi_capped     = 1 / weight_capped
    )
  
  # --- (d) Resample nests within Year only ---
  boot_data <- boot_data %>%
    group_by(Year) %>%
    slice_sample(prop = 1, replace = TRUE) %>%
    ungroup()
  
  # --- (e) Compute HT total per year, using capped weights ---
  boot_year_totals <- boot_data %>%
    group_by(Year) %>%
    summarise(annual_total = sum(1 / pi_capped), .groups = "drop") %>%
    mutate(boot_rep = b)
  
  boot_results[[b]] <- boot_year_totals
}

boot_df <- bind_rows(boot_results)


#save so don't have to wait on the bootsrap each time
saveRDS(boot_results, file = "output/spei_kig_nestAbund_bootsResults_2003-2026.rds")
#reload it in, uncomment
boot_results <- readRDS("output/spei_kig_nestAbund_bootsResults_2003-2026.rds")
boot_df <- bind_rows(boot_results)


# Summarise bootstrap SE and 95% CI, per year and for DSR uncertainty only
boot_var_summary <- boot_df %>%
  group_by(Year) %>%
  summarise(
    boot_median = median(annual_total),
    boot_mean   = mean(annual_total),
    boot_var    = var(annual_total),
    boot_se     = sd(annual_total),
    boot_lci    = quantile(annual_total, 0.025),
    boot_uci    = quantile(annual_total, 0.975),
    .groups = "drop")

print(boot_var_summary)
  #give the per year bootstrap mean as a sanity check against original point estimate
  #some values are extreme on the upper end so use the median





#STEP 9: Combine with plot-based analytical variance (fpc)
  #have to combine DSR uncertainy from bootstrap (boot_var) to the uncertainty from not all plots being searched (var_annual)
combined_summary <- year_summary_final %>%
  select(Year, var_annual, fpc, n_plots_searched) %>%
  left_join(boot_var_summary, by = "Year") %>%
  mutate(
    total_var = var_annual + boot_var,
    total_se  = sqrt(total_var),
    # Center on boot_mean (the HT-corrected estimate), NOT annual_total
    total_lci = pmax(boot_median - 1.96 * total_se, 0),  # floor at 0, counts can't be negative
    total_uci = boot_median + 1.96 * total_se)
combined_summary



#annual_total is the uncorrected raw count going into the HT estimator
#boot_mean is the real HT-corrected abundance, value we want! 


#save results in a csv
# write.csv(boot_results, "output/spei_kig_nestAbund_bootResults_2003-2026.csv", row.names = FALSE)
    


####average summaries####
annual_results <- combined_summary

n_years <- nrow(annual_results)

overall_average <- mean(annual_results$boot_median, na.rm = TRUE)

total_se <- sqrt(sum(annual_results$total_var, na.rm = TRUE)) / n_years

lower_95 <- overall_average -
  qt(0.975, df = n_years - 1) * total_se

upper_95 <- overall_average +
  qt(0.975, df = n_years - 1) * total_se

data.frame(
  overall_average,
  total_se,
  lower_95,
  upper_95)

#mean for 2003-2026 is 91.42 nests, LCI= 89.416, UCI= 93.426



#look at just the years of 2021-2026
shorter <- annual_results %>% filter(Year %in% c(2021,2022,2023,2024,2025,2026))

n_years <- nrow(shorter)

overall_average <- mean(shorter$boot_median, na.rm = TRUE)

total_se <- sqrt(sum(shorter$total_var, na.rm = TRUE)) / n_years

lower_95 <- overall_average -
  qt(0.975, df = n_years - 1) * total_se

upper_95 <- overall_average +
  qt(0.975, df = n_years - 1) * total_se

data.frame(
  overall_average,
  total_se,
  lower_95,
  upper_95)

#mean for 2021-2026 is 46.715 nests, LCI= 45.050, UCI= 48.381







####plots####
combined_summary <- read.csv("output/spei_kig_nestAbund_results_2003-2026.csv")
year_summary_plot <- combined_summary
# write.csv(year_summary_plot, "output/spei_kig_nestAbund_results_2003-2026.csv", row.names = FALSE)

#sample sizes per year
samples <- data.frame(
  Year = c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,
            2019,2021,2022,2023,2024,2025,2026),
  n = c(103,110,119,141,141,121,80,86,84,77,82,73,60,
        107,75,29,64,41,13,22))

year_summary_plot <- year_summary_plot %>%
  left_join(samples %>% select(n, Year),
            by = "Year")



mean(year_summary_plot$boot_median)
year_summary_plot$Year <- as.numeric(year_summary_plot$Year)
final <- ggplot(year_summary_plot, aes(x = Year, y = boot_median)) +
  geom_point(size=1) +
  geom_errorbar(aes(ymin = total_lci, ymax = total_uci), width = 0.4) +
  theme_bw() +
  labs(x = "Year", y = "nest abundance estimate") +
  scale_x_continuous(breaks = seq(min(year_summary_plot$Year), max(year_summary_plot$Year), by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        panel.grid.minor.x = element_blank())+
  geom_segment(aes(x = 2020.5, xend = 2026.5, y = 47), lty = "dashed", color = "red")+
  geom_hline(yintercept = 91.4136, linetype = "dashed", color = "black") +
  geom_text(aes(y = total_uci, label = n), 
            # position = pd, 
            vjust = -0.5, 
            size = 2.5,
            show.legend = FALSE, color = "black")
final

#save plot
# ggsave("output/spei_kig_nestAbund_resultsPlot_2003-2026.png", plot = final, width = 10, height = 5, units = "in", dpi = 300)

sum(year_summary_final$boot_median)  #1828 nests
sum(year_summary_final$total_lci)  #1629 nests
sum(year_summary_final$total_uci)  #1964 nests

#just 2021-2025
year_summary_final_short <- year_summary_final %>% filter(Year %in% c(2021,2022,2023,2024,2025,2026))
sum(year_summary_final_short$boot_median)  #280 nests
sum(year_summary_final_short$total_lci)  #264 nests
sum(year_summary_final_short$total_uci)  #297 nests




#CHECKS
#plot the HT abundance compared to nest counts
  #total nests found, and nests just on plots
nests <- ht_data1 %>% 
  group_by(Year) %>% 
  summarise(on_plot = n())
nests  #number of nests in the analysis
#number of nests total found on and off plot
nests$total <- c(141,164,146,181,185,151,100,114,110,103,107,96,70,171,105,41,83,59,22,32)
  #values from "combined_header_data_1992-2015.csv" and "db_kig_nesting_waterfowl_2019-2026_compiled_20260923.xlsx"
nests

year_summary_final <- combined_summary %>%
  left_join(nests, by = "Year")
year_summary_final
year_summary_final$Year <- as.factor(year_summary_final$Year)

ggplot(year_summary_final, aes(x = Year, y = boot_median)) +
  geom_point(size=1) +
  geom_errorbar(aes(ymin = total_lci, ymax = total_uci), width = 0.4) +
  geom_point(data = year_summary_final, aes(x = Year, y = total), 
             color = "red", size = 2, shape = 17)  +
  geom_point(data = year_summary_final, aes(x = Year, y = on_plot), 
             color = "blue", size = 1) 
#red is the total number of nests on the island found that year
#blue are nests only ON plot and included in the HT estimator



  #add in DSR by year and plot to the above graph
dsr_by_row <- data.frame(
  Year =top_model_2$design.data$S$Year,
  real = ffull.survival$real)
dsr_summary <- dsr_by_row %>%
  group_by(Year) %>%
  summarise(mean_dsr = mean(real, na.rm = TRUE))
dsr_summary$Year <- as.factor(as.character(dsr_summary$Year))

dsr_plot <- year_summary_final %>%
  left_join(dsr_summary %>% dplyr::select(Year, mean_dsr), by = "Year")

ggplot(dsr_plot, aes(x = Year, y = boot_median)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = total_lci, ymax = total_uci), width = 0.4) +
  # coord_cartesian(ylim = c(0, 500)) +
  geom_point(data = dsr_plot, 
             aes(x = Year, y = total, color = "nests ON and OFF"), 
             size = 2, shape = 17) +
  geom_point(data = dsr_plot, 
             aes(x = Year, y = on_plot, color = "nests ON plot"), 
             size = 1) +
  geom_point(data = dsr_plot, 
             aes(x = Year, y = (mean_dsr^24) * 100, color = "annual nest survival"), 
             size = 1) +
  scale_color_manual(values = c("nests ON and OFF" = "red", 
                                "nests ON plot" = "blue", 
                                "annual nest survival" = "green")) +
  labs(color = "Legend")





####summary info####


#test some errors

#what percentage of nests per year use the average DSR values?
ht_data1 %>% filter(is.na(AgeDay1)) %>% count(Year)
ht_data1 %>% 
  group_by(Year) %>% 
  summarise(
    n_missing = sum(is.na(AgeDay1)),
    n_total = n(),
    pct_missing = round(100 * n_missing / n_total, 1))
#49% of 2013 has the average DSR used, why were so many not included in Randall's NS?
  #explains the larger confidence intervals for that year
#annual mean of NAs
mean(21.4,11.8,8.4,7.8,6.4,14.9,7.5,10.5,9.5,11.7,48.8,13.7,3.3,0,1.4,10.3,1.6,2.5,0,0)
#21.4%





####results: evaluating trends####
#look at the overall rate of decline, compare to Ely and Stehn results
  #fit a negative binomial model with a quadratic effect for year
results <- read.csv("output/spei_kig_nestAbund_results_2003-2026.csv")
names(results)
#best to use the boot_median and total_lci and total_uci for values
  #total columns use the variance of the original model and bootstrap together


#simple with no modeling
year_summary_final$Year <- as.numeric(as.character(year_summary_final$Year))
first_val <- year_summary_final$boot_median[1]
last_val <- year_summary_final$boot_median[nrow(year_summary_final)]
n_years <- max(year_summary_final$Year) - min(year_summary_final$Year)

cagr <- (last_val / first_val)^(1 / n_years) - 1
cagr * 100
#-6.82% decline per year



##model the changes
#number of nests total found on and off plot
results$total <- c(141,164,146,181,185,151,100,114,110,103,107,96,70,171,105,41,83,59,22,32)
#values from "combined_header_data_1992-2015.csv" and "db_kig_nesting_waterfowl_2019-2026_compiled_20260923.xlsx"


#simple version
model <- glm(boot_median ~ Year, data = results, family = poisson)
summary(model)

#center year
results1 <- results %>%
  mutate(Year_c = Year - mean(Year))



#negative binomial model
nb_model <- glm.nb(boot_median ~ Year_c + I(Year_c^2), data = results1)
summary(nb_model)
#gives many warnings so could round the boot_median results and avoid that
# results1$boot_median_r <- round(results1$boot_median)

#look at just 2021-2026
results2 <- results1 %>% filter(Year %in% c(2021,2022,2023,2024,2025,2026))
nb_model_short <- glm.nb(boot_median ~ Year_c + I(Year_c^2), data = results2)
summary(nb_model_short)


#check overdispersion
nb_model$deviance / nb_model$df.residual
#1.25 so no reason to fret over overdispersion

#Year and Year^2 significantly different, both p are <0.01 and negative estimates
  #nest numbers are decreasing (-0.05, +/- 0.009, p<0.00001)
  #and the trend is accelerating over time (-0.004, +/- 0.001, p<0.00001)


#Year_c estimate is -0.049951, convert out of log-link
nb_model$coefficients
(exp(-0.049951) - 1) * 100  # ≈ -4.87% per year

#confidence intervals
confint(nb_model)
#convert to % change and add confidence intervals for rate of change is 
ci <- exp(confint(nb_model)) - 1
ci_table <- data.frame(
  Term = rownames(ci),
  Lower_95 = scales::percent(ci[, 1], accuracy = 0.01),
  Upper_95 = scales::percent(ci[, 2], accuracy = 0.01))
ci_table
#95% CI = -6.52% to -3.21%



#get predicted values
pred <- predict(nb_model, newdata = results1, type = "link", se.fit = TRUE)

preds <- results1 %>%
  mutate(
    fit = exp(pred$fit),
    lower = exp(pred$fit - 1.96 * pred$se.fit),
    upper = exp(pred$fit + 1.96 * pred$se.fit))

#plot fitted curve over actual data
curve <- ggplot(preds, aes(x = Year, y = boot_median)) +
  geom_point(size = 2) +
  geom_line(aes(y = fit), color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  scale_x_continuous(breaks = seq(min(preds$Year), 
                                  max(preds$Year), by = 1)) +
  labs(x = "Year", y = "Estimated nest abundance", 
       title = "Negative binomial trend (Year + Year^2)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10), 
    axis.title.x = element_text(size = 14), 
    axis.title.y = element_text(size = 14),
    panel.grid.minor.x = element_blank())
curve

#save plot
# ggsave("output/spei_kig_nestAbund_resultsCurve_2003-2026.png", plot = curve, width = 7, height = 5, units = "in", dpi = 300)




