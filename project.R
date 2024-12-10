#libraries
library(collapse)
library(ggplot2)
library(terra)
library(dplyr)
library(fixest)

####
# importing time use data, creating two subset above and below median age
#####

# import data
data <- read.csv("/Users/wilsonsherman/Library/CloudStorage/GoogleDrive-wcs@g.ucla.edu/My Drive/Coursework/Quant Methods - ENV 197 A/project/Data/ahtus_00002.csv")

# collapsing data by year
data_by_year <- collap(data, ACT_PHYSICAL+ ACT_OUTHOME ~ YEAR, FUN = list(y_mean = mean, count = length))

# filtering for sample years
data_filtered <- data %>%
  filter(SAMPLE %in% c(2018, 2008, 2003))

# counting observations
data_filtered %>%
  group_by(YEAR) %>%
  summarise (count = n())

# estimating median value 
median_AGE <- median(data$AGE, na.rm = TRUE)

# creating dummy variable
data <- mutate(data, dummy_AGE=ifelse(AGE > median_AGE, 1, 0))

# filtering for relevant columns
activity_by_state_year_month <- data_filtered %>%
  collap(ACT_PHYSICAL ~ STATE + YEAR + MONTH + AGE, FUN = list(y_mean = mean, count = length))

# renaming columns
activity_by_state_year_month <- activity_by_state_year_month %>%
  rename(ACT_PHYSICAL=y_mean.ACT_PHYSICAL) 

activity_by_state_year_month <- activity_by_state_year_month %>%
  rename(count=count.ACT_PHYSICAL) 

# collapsing by dummy variable
new_activity_w_dummy<- data_filtered %>%
  collap(ACT_PHYSICAL ~ STATE + YEAR + MONTH + dummy_AGE, FUN = list(y_mean = mean, count = length))

# renaming columns
new_activity_w_dummy<- new_activity_w_dummy %>%
  rename(ACT_PHYSICAL=y_mean.ACT_PHYSICAL) 

new_activity_w_dummy<- new_activity_w_dummy %>%
  rename(count=count.ACT_PHYSICAL) 

# creating subset one 
activity_subset_one <- new_activity_w_dummy %>%
  filter(dummy_AGE == 1)

# creating subset two
activity_subset_two <- new_activity_w_dummy %>%
  filter(dummy_AGE == 0)

####
# loading weather data
#####

# define years
years <- c(2003, 2008, 2018)

# initialize an empty list to store each year's weather stack
weather_stacks <- list()

# loop over each year
for (year in years) {
  # Set up the directory path for the current year
  dir_path <- file.path("/Users/wilsonsherman/Library/CloudStorage/GoogleDrive-wcs@g.ucla.edu/My Drive/Coursework/Quant Methods - ENV 197 A/project/Data/weather", as.character(years))
  
  # list all .bil files in the directory
  bil_files <- list.files(path = dir_path, pattern = "\\.bil$", full.names = TRUE)
  
  # load all .bil files as raster layers
  weather_rasters <- lapply(bil_files, rast)
  
  # stack the rasters
  weather_stack <- rast(weather_rasters)
  
  # print the structure of the stacked rasters for confirmation
  print(weather_stack)
  
  # store the weather stack in the list with the year as the key
  weather_stacks[[as.character(year)]] <- weather_stack
}

##### 
# loading geography data
#####

# import the shapefile
borders <- vect("/Users/wilsonsherman/Library/CloudStorage/GoogleDrive-wcs@g.ucla.edu/My Drive/Coursework/Quant Methods - ENV 197 A/project/tl_2024_us_state/tl_2024_us_state.shp")

##### 
# collapsing temperature data by state and month
#####

# define list of directories for each year
dir_paths <- list(
  "2003" = "/Users/wilsonsherman/Library/CloudStorage/GoogleDrive-wcs@g.ucla.edu/My Drive/Coursework/Quant Methods - ENV 197 A/project/Data/weather/2003",
  "2008" = "/Users/wilsonsherman/Library/CloudStorage/GoogleDrive-wcs@g.ucla.edu/My Drive/Coursework/Quant Methods - ENV 197 A/project/Data/weather/2008",
  "2018" = "/Users/wilsonsherman/Library/CloudStorage/GoogleDrive-wcs@g.ucla.edu/My Drive/Coursework/Quant Methods - ENV 197 A/project/Data/weather/2018"
)

# define the states to analyze
states <- state.name

# initialize an empty list to store the results for each year and state
all_years_state_temp <- list()

# loop over each year
for (year in years) {
  
  # loop over each state for monthly calculation
  for (state_name in states) {
    
    # filter shapefile for the current state
    state_shape <- borders[borders$NAME == state_name, ]
    
    # calculate zonal mean temperature for each raster in the stack
    monthly_means <- zonal(weather_stack, state_shape, fun = mean, na.rm = TRUE, as.polygons = TRUE)
    
    # convert the zonal output to a data frame and remove irrelevant columns
    temp_df <- as.data.frame(monthly_means)[, -c(1:15)]
    
    # transpose, convert to data frame, and add date information
    temp_df_t <- as.data.frame(t(temp_df))
    dates <- substr(rownames(temp_df_t), nchar(rownames(temp_df_t)) - 11, nchar(rownames(temp_df_t)) - 4)
    temp_df_t$date <- as.Date(dates, format = "%Y%m%d")
    
    # rename the temperature column and organize columns
    names(temp_df_t)[1] <- "temp"
    temp_df_t <- temp_df_t[, c("date", "temp")]
    
    # add year and state columns
    temp_df_t$year <- year
    temp_df_t$state <- state_name
    
    # append the result to the list
    all_years_state_temp <- c(all_years_state_temp, list(temp_df_t))
  }}

# combine all data into a single data frame
combined_temp_data <- do.call(rbind, all_years_state_temp)

# filter out rows with missing dates
combined_temp_data <- combined_temp_data[!is.na(combined_temp_data$date), ]

# add separate columns for year and month extracted from the date
combined_temp_data <- combined_temp_data %>%
  mutate(year = as.integer(format(date, "%Y")),
         month = as.integer(format(date, "%m")))

# calculate mean temperature by year, month, and state, remove Alaska and Hawaii
monthly_mean_temp <- combined_temp_data %>%
  filter(state != "Alaska" & state != "Hawaii") %>%
  group_by(year, month, state) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE), .groups = 'drop')


####
# merging outcome and temperature data
#####

# manual state FIPS code lookup
fips_lookup <- data.frame(
  fips = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12,
           13, 15, 16, 17, 18, 19, 20, 21, 22, 23,
           24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
           34, 35, 36, 37, 38, 39, 40, 41, 42, 44,
           45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
                 "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
                 "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                 "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                 "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
                 "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", 
                 "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"))

# merging activity dataframe
activity_by_state_year_month <- activity_by_state_year_month %>%
  left_join(fips_lookup, by = c("STATE" = "fips")) %>%
  filter(!STATE %in% c("3", "14", "43", "52"))

finaldata <- activity_by_state_year_month %>%
  left_join(monthly_mean_temp, 
            by = c("state_name" = "state", 
                   "MONTH" = "month", 
                   "YEAR" = "year")) %>%
  filter(!is.na(mean_temp))

# prepping final subset one data
activity_subset_one <- activity_subset_one %>%
  left_join(fips_lookup, by = c("STATE" = "fips")) %>%
  filter(!STATE %in% c("3", "14", "43", "52"))

final_subset_one <-  activity_subset_one %>%
  left_join(monthly_mean_temp, 
            by = c("state_name" = "state", 
                   "MONTH" = "month", 
                   "YEAR" = "year")) %>%
  filter(!is.na(mean_temp))

# prepping final subset two data
activity_subset_two <- activity_subset_two %>%
  left_join(fips_lookup, by = c("STATE" = "fips")) %>%
  filter(!STATE %in% c("3", "14", "43", "52"))

final_subset_two <-  activity_subset_two %>%
  left_join(monthly_mean_temp, 
            by = c("state_name" = "state", 
                   "MONTH" = "month", 
                   "YEAR" = "year")) %>%
  filter(!is.na(mean_temp))

####
# preparing dataframes for plots
#####

# calculate annual mean temperature per state
annual_mean_temp_by_state <- collap(
  monthly_mean_temp, ~ year + state, FUN = fmean, 
  cols = "mean_temp", 
  na.rm = TRUE
)

# calculate annual mean temperature
annual_mean_temp <- collap(
  monthly_mean_temp, 
  ~ year, 
  FUN = fmean, 
  cols = "mean_temp", 
  na.rm = TRUE
)

# calculate annual mean time doing physical exercise
annual_mean_physical <- collap (
  finaldata,
  ~YEAR,
  FUN = fmean,
  cols = "ACT_PHYSICAL",
  na.rm = TRUE
)

# calculate annual mean time doing physical exercise by state
# smaller dataframe
finaldata_less <- finaldata %>% select (-MONTH, -STATE, -mean_temp)

# collapse
annual_mean_physical_by_state <- collap(
  finaldata_less, 
  ~ YEAR + state_name, 
  FUN = list(
    ACT_PHYSICAL = mean,
    count = sum))

#r emoving duplicate columns
annual_mean_physical_by_state <- annual_mean_physical_by_state %>% 
  select (-count.ACT_PHYSICAL, -ACT_PHYSICAL.count) %>% 
  rename(count = count.count, ACT_PHYSICAL = ACT_PHYSICAL.ACT_PHYSICAL)

# calculate mean temp and physical time by geography
# smaller dataframe
finaldata_some <- finaldata %>% select (-MONTH, -STATE)

# collapse
temp_and_physical <- collap(
  finaldata_some, 
  ~ YEAR + state_name, 
  FUN = list(
    ACT_PHYSICAL = mean,
    mean_temp = mean,
    count = sum))

# removing duplicate columns
temp_and_physical <- temp_and_physical %>% 
  select (-mean_temp.ACT_PHYSICAL, -mean_temp.count, -ACT_PHYSICAL.count, -ACT_PHYSICAL.mean_temp, -count.ACT_PHYSICAL, -count.mean_temp) %>% 
  rename(count = count.count, ACT_PHYSICAL = ACT_PHYSICAL.ACT_PHYSICAL, mean_temp = mean_temp.mean_temp)

####
# plots
#####

# figure 1: annual mean temp
ggplot(annual_mean_temp, aes(x = year, y = mean_temp)) +
  geom_point() +
  geom_line () +
  scale_x_continuous(breaks = c(2003, 2008, 2018), labels = c("2003", "2008", "2018")) +
  labs(x = "Year",
       y = "Average Temperature (°C)") +
  theme_minimal()

# figure 2: annual mean physical
ggplot(annual_mean_physical, aes(x = YEAR, y = ACT_PHYSICAL)) +
  geom_point() +
  geom_line () +
  scale_x_continuous(breaks = c(2003, 2008, 2018), labels = c("2003", "2008", "2018")) +
  labs(x = "Year",
       y = "Average Daily Time Spent on Physical Activity (minutes)") +
  theme_minimal()


# figure 3: temperature vs physical activity
ggplot(temp_and_physical, aes(x = ACT_PHYSICAL, y = mean_temp, color = state_name)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") + 
  geom_text(aes(label = state_name), size=2, hjust = 1, vjust = -1) + 
  labs( x = "Average Daily Spent on Physical Activity (minutes)",
        y = "Average Temperature (C)") +
  theme_minimal() +
  theme(legend.position = "none")


####
# regressions
#####

# model 1 basic regression
feols(ACT_PHYSICAL ~ mean_temp, data = finaldata, cluster = ~ state_name)

# model 2 regression with geography and year control 
feols(ACT_PHYSICAL ~ mean_temp | state_name + YEAR, data = finaldata, weights = ~count, cluster = ~ state_name) 

# model 3 with one additional control (age)
feols(ACT_PHYSICAL ~ mean_temp | state_name + YEAR + AGE, data = finaldata, weights = ~count, cluster = ~ state_name) 

# model 4 with subset one
feols(ACT_PHYSICAL ~ mean_temp | state_name + YEAR, data = final_subset_one, weights = ~count, cluster = ~ state_name) 

# model 5 with subset two
feols(ACT_PHYSICAL ~ mean_temp | state_name + YEAR, data = final_subset_two, weights = ~count, , cluster = ~ state_name) 
