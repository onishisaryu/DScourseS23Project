############################
# Load additional packages #
############################
library(car)
library(MASS)

################################
# Stepwise multiple regression #
################################
initial <- lm(suffer_score ~ ., data=run_mod)
b1      <- step(initial, direction='backward')

# results
m1 <- list()
m1[['Initial']] <- initial
m1[['Stepwise a']] <- b1
modelsummary(m1)

vif(initial)
vif(b1)

ggplot(run_mod, aes(x = pmax(predict(b1),0), y = suffer_score, color = average_heartrate)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = T) +
  labs(title = "Predicted vs Actual Suffer Score", x = "Predicted", y = "Actual") +
  scale_color_gradient(low = "green", high = "red")

###########################
# Stepwise with quadratic #
###########################
formula <- as.formula("suffer_score ~ distance + time_minutes + average_speed + total_elevation_gain + average_heartrate + max_heartrate + average_cadence + I(distance^2) + I(time_minutes^2) + I(average_speed^2) + I(total_elevation_gain^2) + I(average_heartrate^2) + I(max_heartrate^2) + I(average_cadence^2)")
full.model <- lm(formula, data = run_mod)
step.model <- stepAIC(full.model, direction = "backward")
summary(step.model)

m2 <- list()
m2[['Initial']] <- full.model
m2[['Quadratic Stepwise']] <- step.model

modelsummary(m2)


ggplot(run_mod, aes(x = pmax(predict(step.model),0), y = suffer_score, color = average_heartrate)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = T) +
  labs(title = "Predicted vs Actual Suffer Score", x = "Predicted", y = "Actual") +
  scale_color_gradient(low = "green", high = "red")
