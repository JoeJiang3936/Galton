#monte carlo simulation of lse from Galtonheights data

library(HistData)
data(GaltonFamilies)
library(ggplot2)
library(dplyr)
library(gridExtra)

Galton_heights <- GaltonFamilies%>%filter(gender == 'male' & childNum == 1)%>%
  mutate(son = childHeight)%>%select(father, son)

B <- 1000
N <- 50

lse <- replicate(B, {
  sample_n(Galton_heights, N, replace = TRUE)%>%lm(son~father, data = .)%>%.$coef
}
)

lse <- data.frame(beta0 = lse[1,], beta1 = lse[2,])

lse%>%summarize(se0 = sd(beta0), se1 = sd(beta1))


p1 <- lse%>%ggplot(aes(beta0)) + geom_histogram(binwidth = 5, col = "red")
p2<- lse%>%ggplot(aes(beta1)) + geom_histogram(binwidth = .1, col = "black")
grid.arrange(p1, p2, ncol = 2)

cor_beta0_1 <- lse%>%summarize(cor(beta0, beta1))
lse%>%ggplot(aes(beta0, beta1)) + geom_point()

##transformation and association between beta0 and beta1

Galton_heights_standard <- GaltonFamilies%>%filter(gender == 'male' & childNum == 1)%>%
  mutate(son = childHeight, father = father -mean(father))%>%select(father, son)

B <- 1000
N <- 50

lse_standard <- replicate(B, {
  sample_n(Galton_heights_standard, N, replace = TRUE)%>%lm(son~father, data = .)%>%.$coef
}
)

lse_standard <- data.frame(beta0 = lse_standard[1,], beta1 = lse_standard[2,])

cor_standard <- lse_standard%>%summarize(cor(beta0, beta1))
cor_standard
lse_standard%>%ggplot(aes(beta0, beta1)) + geom_point(alpha = .25)