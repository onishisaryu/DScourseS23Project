#################
# Load packages #
#################
library(rStrava)
library(tidyverse)
library(psych)
library(xtable)
library(corrplot)
library(modelsummary)
library(ggplot2)

####################
# get data via API #
####################
app_name <- 'Econ Project' 
app_client_id  <- '103449' 
app_secret <- Sys.getenv("API_KEY")
stoken <- httr::config(token = strava_oauth(app_name, 
                                            app_client_id, 
                                            app_secret, 
                                            app_scope="activity:read_all",
                                            cache = TRUE))
# scrape activities
activities <- get_activity_list(stoken)

#################
# Data cleaning #
#################
# Filter data
run <- 
  compile_activities(activities) %>% 
  filter(location_country == "United States") %>% 
  filter(sport_type == "Run") %>%
  filter(has_heartrate == "TRUE") %>%
  select(c("name","id","start_date",
           "suffer_score",
           "distance","elapsed_time",
           "average_speed",
           "total_elevation_gain",
           "average_heartrate","max_heartrate",
           "average_cadence")) %>%
  mutate(average_cadence   = as.numeric(average_cadence)) %>%
  mutate(average_heartrate = as.numeric(average_heartrate))%>%
  mutate(max_heartrate     = as.numeric(max_heartrate)) %>%
  mutate(suffer_score      = as.numeric(suffer_score)) %>% 
  mutate(elapsed_time      = as.numeric(elapsed_time)) %>% 
  mutate(elapsed_time      = elapsed_time / 60) %>% 
  rename(time_minutes      = elapsed_time) %>% 
  na.omit()
run_mod    <- run %>% select(-c("name","id","start_date"))

##################
# Data summaries #
##################
cor(run_mod)
datasummary_skim(run_mod,output='latex')

cor_matrix <- as.matrix(cor(run_mod))
cor_xtable <- xtable(cor_matrix, caption = "Correlation Matrix")
print(cor_xtable, include.rownames = TRUE, include.colnames = TRUE, booktabs = TRUE)
