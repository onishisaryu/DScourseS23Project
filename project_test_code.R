library(rStrava)
#library(devtools)
#devtools::install_github('fawda123/rStrava',force=T)
library(tidyverse)
library(modelsummary)
library(ggplot2)
library(corrplot)

# using API to generate token
app_name = 'Econ Project' 
app_client_id  = '103449' 
app_secret = Sys.getenv("API_KEY")
stoken <- httr::config(token = strava_oauth(app_name, 
                                            app_client_id, 
                                            app_secret, 
                                            app_scope="activity:read_all",
                                            cache = TRUE))
# stoken = httr::config(token = strava_oauth(app_name,
#                                             app_client_id,
#                                             app_secret,
#                                             app_scope="activity:read_all"))


# scrape activities
activities <- get_activity_list(stoken)
# filter activities
us_acts <- compile_activities(activities) %>% 
  filter(location_country == "United States")  
class(run)
unique(us_acts$has_heartrate)

run <- us_acts %>% filter(sport_type == "Run") %>%
  filter(has_heartrate == "TRUE") %>%
  select(c("name","id","start_date",
           "distance","elapsed_time",
           "total_elevation_gain",
           "average_heartrate",
           "max_heartrate",
           "average_cadence",
            "average_speed",
            "suffer_score")) %>%
  mutate(average_cadence = as.numeric(average_cadence)) %>%
  mutate(average_heartrate = as.numeric(average_heartrate))%>%
  mutate(max_heartrate = as.numeric(max_heartrate)) %>%
  mutate(suffer_score = as.numeric(suffer_score)) %>% 
  mutate(elapsed_time = elapsed_time / 60) %>% 
  rename(time_minutes = elapsed_time) %>% 
  mutate(time_minutes = as.numeric(time_minutes)) %>% 
  na.omit()
run_mod    <- run %>% select(-c("name","id","start_date"))
mod <- list()
# mod[['intercept_only']] <- lm(suffer_score ~ 1, data=run_mod)
initial   <- mod[['initial']] <- lm(suffer_score ~ ., data=run_mod)
back1 <- mod[['back1']] <- step(initial, direction='backward')
# log the suffer_score variable
# inital_log   <- mod[['inital_log']] <- lm(log(suffer_score) ~ ., data=run_mod)
# back2 <- mod[['back2']] <- step(inital_log, direction='backward')
mod[['back2']] <- step(all, direction = "backward", k = log(nrow(run_mod)), trace = 0)
# results of backward stepwise regression
summary(initial)
library(car)
vif(initial)
vif(back1)
modelsummary(mod)
run_mod_v2 <- run %>% select(-c("name","id","start_date")) %>%
                   mutate(timehr_ave = time_minutes * average_heartrate) %>%
                   mutate(timehr_max = time_minutes * max_heartrate) %>% 
                   mutate(timehr_all = time_minutes * average_heartrate * max_heartrate )
                   # mutate(speed_avehr = average_speed * average_heartrate) %>%
                   # mutate(elevation_avehr = total_elevation_gain * average_heartrate)
initialV2   <- lm(suffer_score ~ ., data=run_mod_v2)
back2       <- step(initialV2, direction='backward')
summary(back2)
#correlation table
cor_matrix <- cor(run_mod) %>% 
              as.data.frame() %>% 
              print()
# cor_tab    <- cor_matrix %>% 
#               select(suffer_score) %>%
#               arrange(desc(suffer_score)) %>% 
#               print()
#   # filter(row.names() != "suffer_score") %>%


# Create backward stepwise linear regression
rm(mod)




quartz()
par(mfrow = c(2, 2))
plot(back1)


# Plot the residuals over the outcome variable
residuals <- resid(back1)
plot_data <- data.frame(x = run_mod$suffer_score, y = residuals)
ggplot(plot_data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "Suffer Score", y = "Residuals")

dev.off()
par(mfrow = c(1, 2))
p1 <- ggplot(data=run_mod,aes(log(distance),suffer_score))+geom_point()
p2 <- ggplot(data=run_mod,aes((distance - mean(distance)) / sd(distance),suffer_score))+geom_point()
grid.arrange(p1, p2, ncol = 2)


run_mod2 <- scale(run_mod) %>% as.data.frame() %>% select(-("time_avehr"))
mod2 <- list()
mod2[['intercept_only']] <- lm(suffer_score ~ 1, data=run_mod2)
all2    <- mod2[['all']] <- lm(suffer_score ~ ., data=run_mod2)
back1.2 <- mod2[['back1']] <- step(all, direction='backward', scope=formula(all), trace=0)
back2.2 <- mod2[['back2']] <- step(all, direction = "backward", k = log(nrow(run_mod2)), trace = 0)


# plot interaction mod predictions against actual
plot(predict(timexmhr),
     run$suffer_score,
     xlab = "Predicted Values",
     ylab = "Observed Values")
abline(a = 0,
       b = 1,
       col = "red",
       lwd = 2)


library(magrittr)

corrplot(cor_matrix_reg,method='circle')

run_norm <- run_mod %>%
  mutate(across(c(average_heartrate, 
                  max_heartrate, 
                  average_cadence, 
                  average_speed),
                ~ (.-mean(.))/sd(.)))
cor_matrix_norm <- cor(run_norm) %>% print()
corrplot(cor_matrix_norm,method='circle')

# create a new data frame with the variables of interest
quartz()
df <- run[c("distance", "elapsed_time", "total_elevation_gain", "average_heartrate")]
df_long <- tidyr::gather(df, key = "variable", value = "value")
ggplot(df_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin() +
  scale_fill_discrete(guide = FALSE) +
  xlab("") +
  ylab("") +
  theme_minimal()
top_10_suffer_scores <- head(arrange(run, desc(suffer_score)), 10) %>% print()
top_10_time          <- head(arrange(run, desc(elapsed_time)), 10) %>% print()
top_10_average_hr    <- head(arrange(run, desc(average_heartrate)), 10) %>% print()
top_10_total_elev    <- head(arrange(run, desc(total_elevation_gain)), 10) %>% print()

## scrape activity stream
activity_title <- "Solo Tempo"
id_numbers     <- run %>%
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

# perform backward stepwise multiple regression
library(olsrr)
ols_step_best_subset(initial)
ols_step_all_possible(initial)
