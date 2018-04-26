library(ggplot2)
### setwd("~/Documents/meetings/jagiellonian/data")

load("gopherdat2.RData")
Gdat <- transform(Gdat,
         fyear=reorder(factor(year),prev))

Gdat2 <- Gdat

g1 <- ggplot(data=Gdat, 
             mapping=aes(x=prev,y=shells/Area,
                         colour=fyear))

g2 <- g1 + geom_point() +
  geom_smooth(method="lm",
              aes(group=1),
              colour="black") +
 ## facet_grid(year~Site)
 facet_wrap(~fyear,ncol=1)

g1 %+% Gdat2

theme_set(theme_bw())


g2 + theme_classic()

