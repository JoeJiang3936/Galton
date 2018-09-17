library(HistData)
data(GaltonFamilies)
library(ggplot2)
library(dplyr)

head(GaltonFamilies)

Galton_heights <- GaltonFamilies%>%filter(childNum == 1 & gender == "male")%>%
  mutate(son = childHeight)%>%select(father, son)

Galton_heights%>%summarize(mean(father), sd(father), mean(son), sd(son))

Galton_heights%>%ggplot(aes(father, son)) +
  geom_point()

Galton_heights%>%summarize(cor(father, son))

#monte carlo simualtion

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

#plot father_strata heights with conditonal avg heigts of sons

Galton_heights%>%mutate(father_strata = round(father))%>%
  group_by(father_strata)%>%summarize(conditional_son_avg = mean(son))%>%
  ggplot(aes(father_strata, conditional_son_avg)) +
  geom_point()

#plot with standard father vs son strata heights and line with slope of correlation r

r <- Galton_heights%>%summarize(r = cor(father, son))%>%.$r

Galton_heights%>%mutate(father_strata = round(father))%>%
  group_by(father_strata)%>%
  summarize(conditional_son_avg = mean(son))%>%
  mutate(z_father = scale(father_strata), z_son = scale(conditional_son_avg))%>%
  ggplot(aes(z_father, z_son)) +
  geom_point() +
  geom_abline(slope = r, intercept = 0)

#add regression line


#calcualte b and m, the intercept and slope
mu_x <- mean(Galton_heights$son)
mu_y <- mean(Galton_heights$father)
sigma_x <- sd(Galton_heights$son)
sigma_y <- sd(Galton_heights$father)
r <- cor(Galton_heights$father, Galton_heights$son)
m <- r*sigma_y/sigma_x
b <- mu_y - m*mu_x

Galton_heights%>%ggplot(aes(father, son)) +
  geom_point() +
  geom_abline(slope = m, intercept = b)


# with standard units
mu_x <- mean(Galton_heights$son)
mu_y <- mean(Galton_heights$father)
sigma_x <- sd(Galton_heights$son)
sigma_y <- sd(Galton_heights$father)
r <- cor(Galton_heights$father, Galton_heights$son)
m <- r*sigma_y/sigma_x
b <- mu_y - m*mu_x

Galton_heights%>%ggplot(aes(scale(father), scale(son))) +
  geom_point() +
  geom_abline(slope = m, intercept = 0)





