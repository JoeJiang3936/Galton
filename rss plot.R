library(HistData)
data(GaltonFamilies)
library(ggplot2)
library(dplyr)


Galton_heights <- GaltonFamilies%>%filter(childNum == 1 & gender == "male")%>%
  mutate(son = childHeight)%>%select(father, son)

rss <- function(beta0, beta1, data){
    resid <- Galton_heights$son - (beta0+beta1*Galton_heights$father)
    return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(Galton_heights))
results <- data.frame(beta1 = beta1, rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)