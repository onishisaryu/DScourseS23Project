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
p <- ggplot(run, aes(x = predict(back1), y = run$suffer_score)) +
     geom_point() +
     geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = T) + #polynomial regression
     labs(title = "Predicted vs Actual Suffer Score", x = "Predicted", y = "Actual")
  #geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  # 
## data labels on all
p + geom_text(aes(label = name), hjust = 1, vjust = 1.5) 
## data labels on points with residuals > 1.5 sd
p + geom_label_repel(
    data = run, 
    aes(label = ifelse(abs(predict(back1) - run$suffer_score) > 1.5*sd(back1$residuals), 
                                      run$name, "")),nudge_x = -2, nudge_y = 0.0)  
