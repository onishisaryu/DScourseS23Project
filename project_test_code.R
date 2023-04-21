library(rStrava)
# library(devtools)
devtools::install_github('fawda123/rStrava',force=T)
library(tidyverse)
library(modelsummary)
library(ggplot2)

# using API to generate token
app_name = 'Econ Project' 
app_client_id  = '103449' 
app_secret = Sys.getenv("API_KEY")
stoken = httr::config(token = strava_oauth(app_name,
                                            app_client_id,
                                            app_secret,
                                            app_scope="activity:read_all"))
stoken <- httr::config(token = strava_oauth(app_name, 
                                            app_client_id, 
                                            app_secret, 
                                            app_scope="activity:read_all",
                                            cache = TRUE))

# scrape activities
activities = rStrava::get_activity_list(stoken)
# filter activities with latitude and longitude
us_acts = compile_activities(activities) %>% 
  filter(start_latlng2 < -97.176918, start_latlng2 > -97.547822) %>% 
  filter(start_latlng1 < 35.348324, start_latlng1 > 35.145318) %>% 
  filter(distance > 5) %>%
  filter(sport_type == "Run")

run <- us_acts %>% filter(has_heartrate == "TRUE") 
run_data <- select(run,c("name","id","distance","elapsed_time",
                            "elev_high","elev_low","total_elevation_gain",
                            "average_heartrate","max_heartrate",
                            "average_cadence","average_speed",
                            "suffer_score")) %>%
  mutate(average_cadence = as.numeric(average_cadence)) %>%
  mutate(average_heartrate = as.numeric(average_heartrate))%>%
  mutate(max_heartrate = as.numeric(max_heartrate)) %>%
  mutate(suffer_score = as.numeric(suffer_score)) %>%
  na.omit()
run_mod = run_data %>% select(-c("name","id"))


mod <- list()
# intercept-only model
mod[['intercept_only']] <- lm(suffer_score ~ 1, data=run_mod)
# model with all predictors
mod[['all']] <- lm(suffer_score ~ ., data=run_mod)
all <- mod[['all']]
# backward stepwise regression
mod[['backward']] <- step(all, direction='backward', scope=formula(all), trace=0)
backward <- mod[['backward']]
# results of backward stepwise regression
backward$anova
#view final model
backward$coefficients
summary(all)
summary(backward)
par(mfrow = c(2, 2))
plot(backward)

single <- list()
distance <- single[['distance only']]        <- lm(suffer_score ~ distance, data=run_mod)
elev_high <- single[['elevation only']]       <- lm(suffer_score ~ elev_high, data=run_mod)
ave_hr <- single[['average hr only']]      <- lm(suffer_score ~ average_heartrate, data=run_mod)
ave_cad <- single[['average cadence only']] <- lm(suffer_score ~ average_cadence, data=run_mod)
par(mfrow = c(2, 2))
with(run_mod,plot(suffer_score,average_heartrate))
with(run_mod,plot(suffer_score,average_cadence))
with(run_mod,plot(suffer_score,distance))


plot(predict(backward),                                # Draw plot using Base R
     run_data$suffer_score,
     xlab = "Predicted Values",
     ylab = "Observed Values")
abline(a = 0,                                        # Add straight line
       b = 1,
       col = "red",
       lwd = 2)

library(corrplot)
library(magrittr)
cor_matrix <- cor(run_mod[,c(1,2,3,4,5,6,7,8,9)])
corrplot(cor(run_data[,c(3,4,5,6,7,8,9,10,11,12)]),method='circle')

run_norm <- run_data %>% select(-c("name","id")) %>%
  mutate(across(c(average_heartrate, 
                  max_heartrate, 
                  average_cadence, 
                  average_speed),
                ~ (.-mean(.))/sd(.)))
norm_mod <- list()
# intercept-only model
norm_mod[['intercept_only']] <- lm(suffer_score ~ 1, data=run_norm)
# model with all predictors
norm_mod[['all']] <- lm(suffer_score ~ ., data=run_norm)
all_norm <- norm_mod[['all']]
# backward stepwise regression
norm_mod[['backward']] <- step(all_norm, direction='backward', scope=formula(all), trace=0)
backward_norm <- norm_mod[['backward']]
# distance*speed
dist_interaction <- norm_mod[["dist_interaction"]] <- lm(suffer_score ~ distance*average_heartrate + distance*average_cadence + average_speed, data = run_mod)
norm_mod[['backward2']] <- step(dist_interaction, direction='backward', scope=formula(all), trace=0)

## scrape activity stream
activity_title <- "Solo Tempo"
id_numbers     <- run_fltrd %>%
  filter(name == activity_title) %>%
  pull(id)
test_strms <- get_activity_streams(activities, stoken, id = id_numbers)
plot_spdsplits(run_fltrd, stoken, acts = 1, units = 'metric', fill = 'darkgreen')


track_16x400 <- get_streams(stoken, id = id_numbers, request = "activities",
                            types = "heartrate",
                            resolution = "medium"
)

tiz_16x400 <- track_16x400 %>% as.data.frame() #%>% 
  #mutate(zone=case_when("low"~heartrate < 150, "med"~heartrate >= 150 & heartrate < 165, "high"~heartrate >= 165))
tiz_16x400 <- track_16x400 %>% select(c(altitude,cadence,distance,grade_smooth,heartrate,moving, time,velocity_smooth) %>%
                                        mutate(zone = case_when(heartrate < 150 ~ "low",heartrate >= 150 & heartrate < 165 ~ "med",heartrate >= 165 ~ "high"))
tiz_16x400$zone <- factor(tiz_16x400$zone, levels = c("low", "med", "high"))

ggplot(data=tiz_16x400,aes(zone,fill = zone))+
  geom_bar()+  
  scale_fill_manual(values = setNames(c("green","yellow","red"), 
                                      levels(tiz_16x400$zone)))



# trimmed16x400 <- track_16x400 %>% 
#   dplyr::filter(distance >= 3.5) %>% 
#   dplyr::filter(distance<= 13) %>%
#   as.data.frame() %>% 
#   setNames(colnames(track_16x400))
# 
# ggplot(trimmed16x400, aes(x = distance, y = heartrate, color = heartrate)) + 
#   geom_line() +
#   scale_color_gradient(low = "green", high = "red")+
#   labs(title = "Heart Rate vs Distance", x = "Distance (m)", y = "Heart Rate (bpm)")