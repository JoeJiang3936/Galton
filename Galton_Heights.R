library(HistData)
data(GaltonFamilies)
library(ggplot2)
library(dplyr)

head(GaltonFamilies)

Galton_heights <- GaltonFamilies%>%filter(childNum == 1 & gender == "male")%>%
  mutate(son = childHeight)%>%select(father, son)

Galton_heights%>%summarize(mean(father), sd(father), mean(son), sd(son))

Galton_heights%>%ggplot(aes(father, son)) +
  geom_point

Galton_heights%>%summarize(cor(father, son))

