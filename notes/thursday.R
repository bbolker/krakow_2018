## read from file
lizards <- read.csv("lizards.csv")
## OR load from package
## (this version has slightly different variable names!)
## data("lizards",package="brglm")
summary(lizards)

library(ggplot2); theme_set(theme_bw())
ggplot(lizards,
       aes(x=light,
           y=grahami/(grahami+opalinus)))+
  geom_boxplot()+
  stat_sum(colour="red",alpha=0.5) +
  scale_size(range=c(3,8))
## + facet_grid(time~height)  ## sort of silly

## compute total and fraction grahami
lizards <- transform(lizards,tot=grahami+opalinus)
lizards <- transform(lizards,gfrac=grahami/tot)

## switch first level of factor to "shady"
## (in this case, it was already that way because "shady" is alphabetically
##  before "sunny")
lizards <- transform(lizards,light=relevel(light,"shady"))

## basic binomial GLM
m1 <- glm(gfrac~light,  ## proportion ~ predictors
          data=lizards,
          family=binomial,
          weights=tot   ## specify total N for each observation
          )
library(aods3)
gof(m1)  ## looks overdispersed ... but ...

newdata <- data.frame(light=c("shady","sunny"))
predict(m2,newdata,type="response")

## more complex model (additive)
## (interactive model [light*height*diameter*time] would have one observation per parameter)
m2 <- update(m1,gfrac~light+height+diameter+time)
gof(m2)  ## this is fine

plot(m2)  ## diagnostics look OK
predict(m2,type="response")

## example: compute average effects for each diameter class ...
library(emmeans)
emmeans(m2, ~ light | diameter)


## use of offsets ...
load("gopherdat2.RData")
glm(shells~prev+offset(log(Area)),Gdat,family=poisson)

