library(tidyverse)
library(janitor)
library(dplyr)
library(stringr)
library(lubridate)
library(hms)

datum <- read_csv("data/RAW/testsurvey.csv")

###Removes GlobalID, CreationDate, Creator, EditDate, & Editor columns###
datum <- datum[, -c(2:6)]

###Fix column names###
datum <- janitor::clean_names(datum)

###Fix Survey Start Date and time###
#Need to check why time downloads wrong 8 hours off 
#(time zone? is wrong on the CSV)
datum <- datum %>% rename(sdt = survey_start_date_and_time) #rename
datum <- datum %>%
  mutate(
    sdt = as.POSIXct(sdt, format = "%m/%d/%Y %I:%M:%S %p"),
    date = as.Date(sdt),
    start_time = as_hms(sdt)) #split columns
# %>% select (-sdt)     Removes sdt column, might be nice for later
datum <- datum %>%
  relocate(date, start_time, .after = sdt)

###Fix Reach###
datum <- unite(datum, col = "reach", reach, other_reach, 
               sep = " ", na.rm = TRUE)
datum$reach <- gsub("\\bother \\b", "", datum$reach, ignore.case = TRUE)
    # Removes the "other" from anywhere in the columns
  #Run unique to see if you need to add short names to each (BELOW)
datum$reach <- sub("Bordwell Creek$", "Bordwell Creek (BRC)", datum$reach)
  #Separate into reach and reach_short
datum <- datum %>%
  extract(reach, into = c("reach", "reach_short"),
          regex = "^(.*)\\s*\\((.*)\\)$",
          remove = FALSE)
  #Removes random spaces
datum[] <- lapply(datum, function(x) {
  if (is.character(x)) trimws(x) else x
})

###Does reach_short match site?###
#######Run 1
datum$code_match <- 
  substr(datum$reach_short, 1, 3) == substr(datum$site, 1, 3)
datum <- datum %>%
  relocate(code_match, .after = site) #Moves columns over to read easily
#######Run 2
#How to fix when site is wrong
datum$site[datum$site == "UMV-2" & datum$code_match == "FALSE"] <- "BRC-1"
#how to fix when reach is wrong
datum$reach[datum$reach == "Lower Wall Creek" & 
              datum$code_match == FALSE] <- "Upper Mountain View Creek"
datum$reach_short[datum$reach_short == "LWC" & 
                    datum$code_match == FALSE] <- "UMC"
#######Run 3
datum$code_match <- 
  substr(datum$reach_short, 1, 3) == substr(datum$site, 1, 3)
######After check remove column
datum <- datum %>% select (-code_match)

###Temperature check###
datum <- datum %>% rename(air_temp = air_temperature_c)
######Run 1
datum <- datum %>%
  mutate(temp_check = ifelse(air_temp < 5 | air_temp > 30,
                               "bad",
                               "good"))
datum <- datum %>%
  relocate(temp_check, .after = air_temp)
######Run 2
datum <- datum %>%
  mutate(air_temp = ifelse(object_id == 4,
                           (air_temp - 32) * 5 / 9,
                           air_temp))
######Run 3
datum <- datum %>%
  mutate(temp_check = ifelse(air_temp < 5 | air_temp > 30,
                             "bad",
                             "good"))
datum <- datum %>%
  relocate(temp_check, .after = air_temp)
######After check remove column
datum <- datum %>% select (-temp_check)

###Fix Weather###
datum <- unite(datum, col = "weather", weather, other_weather, 
               sep = " ", na.rm = TRUE)
# Removes the "other" from anywhere in the columns
datum$weather <- gsub("\\bother \\b", "", datum$weather, ignore.case = TRUE)
#Removes random spaces
datum[] <- lapply(datum, function(x) {
  if (is.character(x)) trimws(x) else x
})

###Fix collection method###
datum <- unite(datum, col = "method", collection_method, other_collection_method, 
               sep = " ", na.rm = TRUE)
# Removes the "other" from anywhere in the columns
datum$method <- gsub("\\bother \\b", "", datum$method, ignore.case = TRUE)
#Removes random spaces
datum[] <- lapply(datum, function(x) {
  if (is.character(x)) trimws(x) else x
})
datum <- datum %>%
  mutate(
    method = if_else(is.na(method) | method == "", "Unknown", method)
  ) #Will replace blank charter vectors with unknown

###For DO,Conductivity, water temp, & PH follow air temp for ranges check###
datum <- datum %>% rename(do = dissolved_oxygen_do,
                          water_temp = temperature,
                          ph = p_h,
                          larvae = number_of_larvae_collected)

###Habitat type Fixes###
datum <- datum %>% rename(habitat = habitat_type_classification,
                          other_habitat = other_habitat_type_classification)
datum <- unite(datum, col = "habitat", habitat, other_habitat, 
               sep = " ", na.rm = TRUE)
# Removes the "other" from anywhere in the columns
datum$habitat <- gsub("\\bother \\b", "", datum$habitat, ignore.case = TRUE)
#Removes random spaces
datum[] <- lapply(datum, function(x) {
  if (is.character(x)) trimws(x) else x
})

###Habitat info Rename###
datum <- datum %>% rename(wetted_width = wetted_width_cm,
                          max_depth = maximum_depth_cm,
                          habitat_length = length_of_habitat_m,
                          latitude = x,
                          longitude = y)

###Lowercase everything! but notes and site names (might be better do do this earlier)###
datum[c('weather', 'reach', 'turbidity','method', 'habitat')] <- 
  sapply(datum[c('weather', 'reach', 'turbidity','method', 'habitat')], function(x) tolower(x))

# Save cleaned data
write_csv(datum, "data/larval/testsurvey.csv")
