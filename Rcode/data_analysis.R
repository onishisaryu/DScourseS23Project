################################
#   Load additional packages   #
################################
library(car)
library(MASS)

################################
# Stepwise multiple regression #
################################
initial <- lm(suffer_score ~ ., data=run_mod)
step1   <- step(initial, direction='backward')
summary(step1)

p1<- 
  ggplot(run_mod, 
       aes(x = predict(step1), y = suffer_score, color = suffer_score)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = F) +
  labs(title = "Predicted vs Actual Suffer Score, model 1", x = "Predicted", y = "Actual") +
  scale_color_gradient(low = "green", high = "red")

################################
#   Stepwise with quadratic    #
################################
formula <- as.formula("suffer_score ~ distance + time_minutes + average_speed + total_elevation_gain + average_heartrate + max_heartrate + average_cadence + I(distance^2) + I(time_minutes^2) + I(average_speed^2) + I(total_elevation_gain^2) + I(average_heartrate^2) + I(max_heartrate^2) + I(average_cadence^2)")
full.model <- lm(formula, data = run_mod)
step2 <- stepAIC(full.model, direction = "backward")
summary(step2)

p2 <-
  ggplot(run_mod, 
       aes(x = predict(step2), y = suffer_score, color = suffer_score)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = F) +
  labs(title = "Predicted vs Actual Suffer Score, model 2", x = "Predicted", y = "Actual") +
  scale_color_gradient(low = "green", high = "red")

################################
#     Adding interactions      #
################################
formula <- 
  as.formula("suffer_score ~ 
             distance + I(distance^2) +
             time_minutes + I(time_minutes^2) + 
             average_speed + I(average_speed^2) + 
             total_elevation_gain + 
             average_heartrate + I(average_heartrate^2) + 
             max_heartrate + I(max_heartrate^2) + 
             average_cadence + 
             distance:time_minutes + 
             distance:average_heartrate + 
             distance:max_heartrate + 
             distance:average_cadence + 
             time_minutes:average_heartrate + 
             time_minutes:max_heartrate +
             time_minutes:average_cadence")
model <- lm(formula, data = run_mod)

# Perform stepwise regression
step3 <- stepAIC(model, direction = "backward")
summary(step3)

p3  <-
  ggplot(run_mod, aes(x = predict(step3), y = suffer_score, color = suffer_score)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = F) +
  labs(title = "Predicted vs Actual Suffer Score, model 3", x = "Predicted", y = "Actual") +
  scale_color_gradient(low = "green", high = "red")

################################
#       Final summary         #
################################
final <- list()
final[['model 1']] <- step1
final[['model 2']] <- step2
final[['model 3']] <- step3
modelsummary(final, stars=T, statistic = 'p.value')#,output = 'latex')

ggsave("model1_pred.png",p1)
ggsave("model2_pred.png",p2)
ggsave("model3_pred.png",p3)

# plotdata <- 
#   with(run_mod, 
#   data.frame(
#   Suffer_Score = suffer_score,
#   Resid_model1 = residuals(step1),
#   Resid_model2 = residuals(step2),
#   Resid_model3 = residuals(step3)
# ))
# data_long <- plotdata %>%
#   gather(key = "model", value = "residual", - Suffer_Score)
# ggplot(data_long, aes(x = Suffer_Score, y = residual, color = model)) +
#   geom_point() +
#   geom_hline(yintercept = 0, linetype = "dashed")
