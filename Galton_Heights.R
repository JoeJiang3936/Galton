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

B <- 10000
N <- 25

#monte carlo simulation of sampling 25 father son pairs and calculate correlation
#between them. Notice the row sampling function sample_n()

R <- replicate (B, {
  sample_n(Galton_heights, N, replace = TRUE)%>%
    summarize(r = cor(father, son))%>%.$r

}
                )


# histongram of R distribution


data.frame(R)%>%ggplot(aes(R)) + geom_histogram(bindwidth=.01, color="red")


# conditional average height of sons from fathers about 72 inches tall 
conditonal_avg <- Galton_heights%>%filter(round(father) == 72)%>%
  summarize(son_avg = mean(son))%>%.$son_avg

conditonal_avg

#Boxplot of father and son strata heights

Galton_heights%>%mutate(father_strata = factor(round(father)))%>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()
  



