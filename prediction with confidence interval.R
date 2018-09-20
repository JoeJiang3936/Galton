library(HistData)
data(GaltonFamilies)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tibble)

Galton_heights <- GaltonFamilies%>%filter(gender == 'male' & childNum == 1)%>%
  mutate(son = childHeight)%>%select(father, son)



#regreesion line with confidence intervals
Galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

#plots prediction (fit) with confidence intervals
model <- lm(son ~ father, data = Galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = Galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), fill = 'yellow', col = "red", alpha=0.5) + 
  geom_point(data = Galton_heights, aes(x = father, y = son))

