# Install and load necessary packages
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
install.packages("leaflet", repos = "http://cran.us.r-project.org")
install.packages("maps", repos = "http://cran.us.r-project.org")
install.packages("mapproj", repos = "http://cran.us.r-project.org")
install.packages("tidyr", repos = "http://cran.us.r-project.org")
library(ggplot2)
library(leaflet)
library(dplyr)
library(maps)
library(mapproj)
library(tidyr)

# Load US shootings data, not interpreting strings as factors
US_2018_shootings <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)

# Calculate total number of shootings
total_shootings <- nrow(US_2018_shootings)

# Calculate total number of deaths
total_deaths <- US_2018_shootings %>%
  summarize(total = sum(num_killed, na.rm = TRUE)) %>%
  pull(total)

# Calculate which city was most impacted by shootings, if "most impacted" means
# having the highest number of injuries and deaths caused by shootings.
most_impacted_city <- US_2018_shootings %>%
  group_by(city) %>%
  summarize(total_affected = sum(num_killed, na.rm = TRUE)
           + sum(num_injured, na.rm = TRUE)) %>%
  filter(total_affected == max(total_affected, na.rm = TRUE)) %>%
  pull(city)
  
# Calculate which city has the highest rate of fatal shootings (deaths per shooting)
highest_fatal_rate_city <- US_2018_shootings %>%
  group_by(city) %>%
  summarise(total_deaths = sum(num_killed, na.rm = TRUE)) %>%
  mutate(fatal_rate = total_deaths / sum(total_deaths, na.rm = TRUE)) %>%
  filter(fatal_rate == max(fatal_rate, na.rm = TRUE)) %>%
  pull(city)

# Calculate which date(s) had the highest number of deaths due to shootings
most_deaths_date <- US_2018_shootings %>%
  group_by(date) %>%
  summarize(total_date_deaths = sum(num_killed, na.rm = TRUE)) %>%
  filter(total_date_deaths == max(total_date_deaths, na.rm = TRUE)) %>%
  pull(date)

# Which were the 10 incidents with the most number of people affected?
most_affected_incidents <- US_2018_shootings %>%
  mutate(num_affected = num_killed + num_injured) %>%
  top_n(10, wt = num_affected) %>%
  arrange(-num_affected) %>%
  mutate(num_affected_str = paste(num_affected, "people")) %>%
  select(date, state, city, address, num_affected_str)

# Select the Parkland shooting incident data
parkland_shooting <-US_2018_shootings %>%
  filter(grepl('Parkland', city))

# Determine date of Parkland shooting incident
Parkland_shooting_date <- parkland_shooting %>%
  pull(date)

# Determine location of Parkland shooting incident
Parkland_shooting_location <- parkland_shooting %>%
  summarize(location = paste(address, city, state, sep = ", ")) %>%
  pull(location)

# Determine the number of people killed during the Parkland shooting incident
Parkland_num_killed <- parkland_shooting %>%
  pull(num_killed)

# Determine the number of people injured during the Parkland shooting incident
Parkland_num_injured <- parkland_shooting %>%
  pull(num_injured)

# Calculate the total number of people impacted by the Parkland shooting incident
Parkland_num_impacted = Parkland_num_injured + Parkland_num_killed

# Wrangle dataframe to create new column totaling number of people impacted and 
# create new column of information to include in the interactive map's pop-up
US_2018_shootings_impacted <- US_2018_shootings %>%
  mutate(num_impacted = num_injured + num_killed)
US_2018_shootings_popup <- US_2018_shootings_impacted %>%
  mutate(num_injured = paste(num_injured, "injured"),
         num_killed = paste(num_killed, "killed"),
         location = paste(city, ", ", state, sep = "")) %>%
  mutate(popup = paste(location, num_killed, num_injured, sep = "<br>"))

# Determine the top ten cities with the most number of people impacted by 
# shootings
US_2018_shootings_top_cities <- US_2018_shootings_impacted %>%
  group_by(city) %>%
  summarize(Injured = sum(num_injured,na.rm = TRUE),
            Killed = sum(num_killed, na.rm = TRUE)) %>%
  arrange(Injured + Killed) %>%
  top_n(10) %>%
  gather(key = Type, value = num_people, -city)
