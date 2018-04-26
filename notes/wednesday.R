aids <- read.csv("aids.csv")
aids <- transform(aids,
             time=year-min(year)+(quarter-1)/4)
library(ggplot2)
ggplot(aids, aes(x=time,y=log(cases))) +
  geom_point() + 
  geom_smooth(method="lm")

ggplot(aids, aes(x=time,y=cases)) +
  geom_point() + 
  geom_smooth(method="glm",
      formula = y ~ poly(x,2),
      method.args=list(family=quasipoisson))

library(MASS)
ggplot(aids, aes(x=time,y=cases)) +
  geom_point() + 
  geom_smooth(method="glm.nb",
              formula = y ~ poly(x,2))+
  geom_smooth(method="glm",
              formula = y ~ poly(x,2),
    method.args=list(family=quasipoisson),
    colour="red",linetype=2)

g4 <- glm.nb(cases~poly(time,2),data=aids)

library(bbmle)
g5 <- mle2(cases~dnbinom(mu=exp(logmu),size=exp(log.k)),
          parameters=list(logmu~poly(time,2)),
          data=aids,
          start=list(logmu=4,log.k=3))


coef(g3)
coef(g4)
g1 <- glm(cases~time,data=aids,
    family=poisson)
print(g1)
lm(cases~time,data=aids)
par(mfrow=c(2,2))
plot(g1)
g2 <- glm(cases~time+I(time^2),data=aids,
          family=poisson)
g3 <- glm(cases~poly(time,2),data=aids,
          family=poisson)
plot(g3,id.n=20)
summary(g2)
anova(g1,g3,test="Chisq") 
## likelihood ratio test
summary(g3)

## check ratio of residual dev/residual df
library(aods3)
gof(g3)
g3Q <- update(g3,family=quasipoisson)
summary(g3Q)
g1Q <- update(g1,family=quasipoisson)
anova(g3Q,g1Q,test="F")

g3SQ <- update(g3Q,
      data=aids[-1,])
plot(g3SQ)
