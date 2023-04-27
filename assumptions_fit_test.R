library(psych)
library(lmtest)
# good for descriptive statistics
t1 <- describe(run_mod) %>% 
  select(c("n","mean","median","sd","min","max","kurtosis")) %>% 
  print()

# linearity and co-linearity test
cor_matrix <- cor(run_mod) %>% 
  print()
print(back1$coefficients)
plot(back1$residuals)
dwtest(back1)
# predicted vs actual
dev.off()
par(mfrow=c(2,1))
p1 <- ggplot(run, aes(x = pmax(predict(back1),0), y = run$suffer_score)) +
  geom_point()
p2 <- ggplot(run, aes(x = predict(back2), y = run$suffer_score)) +
     geom_point()
p3 <- ggplot(run, aes(x = exp(predict(back2)), y = run$suffer_score)) +
  geom_point()
p1 <- p1 + geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = FALSE) + #polynomial regression
  labs(title = "Predicted vs Actual Suffer Score", x = "Predicted", y = "Actual")
library(gridExtra)
grid.arrange(p1, p2, ncol = 1)
dev.off()
p1 <- p1 + annotate("text", x = Inf, y = -Inf, vjust = -43,
                 hjust = 1, label = paste0("R-squared = ",
                                           round(summary(back1)$r.squared, 3)))

p1
cor( pmax(predict(back1),0), run_mod$suffer_score)^2

#geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  # 
## data labels on all
p + geom_text(aes(label = name), hjust = 1, vjust = 1.5) 
## data labels on points with residuals > 1.5 sd
p + geom_label_repel(
    data = run, 
    aes(label = ifelse(abs(predict(back1) - run$suffer_score) > 1.5*sd(back1$residuals), 
                                      run$name, "")),nudge_x = -2, nudge_y = 0.0)  
predicted <- pmax(predict(back1),0)
residuals <- residuals(back1)
data <- data.frame(predicted = predicted, residuals = residuals)
p = ggplot(data, aes(x = predicted, y = residuals)) +
  geom_point() +
  labs(x = "Predicted Values", y = "Residuals") +
  ggtitle("Residuals vs. Predicted Values")
ggsave("pred_actual.png",p1,width = 8, height = 6)
plot(back,1)
