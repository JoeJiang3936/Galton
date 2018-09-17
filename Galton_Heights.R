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

