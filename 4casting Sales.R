#  
#  Exercise on forecasting sales 
#  Chapter 4 in Saminas (2015, pp.88-99)
#

library(stats)
library(ggplot2)
library(ggpubr)
library(ggrepel)

df <- data.frame(x=c(1:7), 
                 y=c(185500,154703,132503,117564,106395,98171,92322))

# Linear Trend
# y = b + a*x 
m1 <- lm(y ~ x, data=df)
m1$coefficients
# Forecast of Turnover for x=8
f1 <- m1$coefficients[[1]]+m1$coefficients[[2]]*8

# Polynomial Trend (Degree = 2)
# y = b + a1*x + a2*x^2
m2 <- lm(y ~ poly(x, 2, raw=TRUE), data=df)
m2$coefficients
# Forecast of Turnover for x=8
f2 <- m2$coefficients[[1]]+m2$coefficients[[2]]*8+m2$coefficients[[3]]*8^2

# Min and max points for Y-axis of the charts to be built next
mn <- min(m1$model$y,m1$fitted.values,m2$model$y,m2$fitted.values,f1,f2)-5000
mx <- max(m1$model$y,m1$fitted.values,m2$model$y,m2$fitted.values,f1,f2)+5000
  
g1 <-     # Graph with liner trend + forecast
  df %>%
  ggplot(aes(x=x, y=y)) + geom_point() +                  # known points    
  stat_smooth(method='lm', formula = y ~ x) +             # linear trend line 
  geom_point(data = data.frame(x=8, y=f1), col = "red") + # forecast point
  labs(title = "Linear Trend") + ylim(c(mn,mx)) +
  geom_label_repel(aes(label = y),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  geom_label_repel(data = data.frame(x=8, y=f1),
                   aes(label = round(y)),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = "black", color = "red")

g2 <-     # Graph with polynomial trend + forecast
  df %>%
  ggplot(aes(x=x, y=y)) + 
  geom_point() +               # known points    
  stat_smooth(method='lm', formula = y ~ poly(x,2)) +     # polynomial smooth line 
  geom_point(data = data.frame(x=8, y=f2), col = "red") + # forecast point
  labs(title = "Polynomial Trend (Degree = 2)") + ylim(c(mn,mx)) +
  geom_label_repel(aes(label = y),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = "black") +
  geom_label_repel(data = data.frame(x=8, y=f2),
                   aes(label = round(y)),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = "black", color = "red")

ggarrange(g1, g2, nrow = 1, ncol = 2, widths = c(1,1), align = "h")
ggsave(filename = "4cast Sales.png", width = 4*2.026, height = 4)

# Forecast values for x=8
f1;f2
