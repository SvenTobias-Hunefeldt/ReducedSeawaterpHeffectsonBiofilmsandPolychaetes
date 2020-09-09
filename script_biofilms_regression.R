setwd("~/Documents/UNI/PhD/THESIS/LAB work/Hatchery/Biofilm/R")
dir()

library(vegan)
library(car)
library(emmeans)


## 1 
# ---------------- Logistic regression: Settled vs. Not Settled -------- ##

set<-read.csv("Exp-settlement_biofilm_Oct2018.csv", header = T)
head(set)

set=subset(set,total.larvae>=10)

set=subset(set, age.slide!="0")# quitar los controles

set$pH.water<-factor(set$pH.water, levels = c('7', '7.2', '7.4', '7.7', '7.9', '8.1'),
                     ordered = TRUE)
set$age.slide<-factor(set$age.slide, levels = c('6', '9', '12'),
                     ordered = TRUE)


# regression

settled<-set$settle.slides+set$settle.cont  # Total settled?

SvN<-cbind(settled,set$not.settle)

mod<-glm(SvN~pH.water*age.slide, family = binomial, data = set)

summary(mod)

anova(mod, test = 'Chisq')


## post-hoc: Have to test interaction, lots of comparisons!

emmeans(mod, list(pairwise~pH.water), adjust = 'Tukey')

emmeans(mod, list(pairwise~age.slide), adjust = 'Tukey')

emmeans(mod, list(pairwise~pH.water:age.slide), adjust = 'Tukey')



##  - 2 --------------------------------------------------------
#   - ----------- Logistic regression: settled slides vs settled containers

set<-read.csv("Exp-settlement_biofilm_Oct2018.csv", header = T)
head(set)

set1=subset(set,total.larvae>=10)

set1=subset(set1, age.slide!="0")# quitar los controles

set1$pH.water<-factor(set1$pH.water, levels = c('7', '7.2', '7.4', '7.7', '7.9', '8.1'),
                     ordered = TRUE)
set1$age.slide<-factor(set1$age.slide, levels = c('6', '9', '12'),
                      ordered = TRUE)


## 2 --  Logistic regression: Settled slides vs. Settled containers ##

SvC<-cbind(set1$settle.slides,set1$settle.cont)# slides vs containers

mod1<-glm(SvC~pH.water*age.slide, family = binomial, data = set1)

summary(mod1)

anova(mod1, test = 'Chisq')


## post-hoc: Have to test interaction, lots of comparisons!
emmeans(mod1, list(pairwise~pH.water), adjust = 'Tukey')

emmeans(mod1, list(pairwise~age.slide), adjust = 'Tukey')

emmeans(mod1, list(pairwise~pH.water:age.slide), adjust = 'Tukey')



##  - 3 --------------------------------------------------------
#   - ----------- Logistic regression: settled slides vs not settled 

set<-read.csv("Exp-settlement_biofilm_Oct2018.csv", header = T)
head(set)

set2=subset(set,total.larvae>=10)

set2=subset(set2, age.slide!="0")# quitar los controles

set2$pH.water<-factor(set2$pH.water, levels = c('7', '7.2', '7.4', '7.7', '7.9', '8.1'),
                      ordered = TRUE)
set2$age.slide<-factor(set2$age.slide, levels = c('6', '9', '12'),
                       ordered = TRUE)


## --  Logistic regression: Settled slides vs. not Settled  ##

SvNT<-cbind(set2$settle.slides,set2$not.settle)# slides vs not settled

mod2<-glm(SvNT~pH.water*age.slide, family = binomial, data = set2)

summary(mod2)

anova(mod2, test = 'Chisq')


## post-hoc: Have to test interaction, lots of comparisons!
emmeans(mod2, list(pairwise~pH.water), adjust = 'Tukey')

emmeans(mod2, list(pairwise~age.slide), adjust = 'Tukey')

emmeans(mod2, list(pairwise~pH.water:age.slide), adjust = 'Tukey')

##  - 4 --------------------------------------------------------
#   - ----------- Logistic regression: settled slides vs THE REST 

set<-read.csv("Exp-settlement_biofilm_Oct2018.csv", header = T)
head(set)

set3=subset(set,total.larvae>=10)

set3=subset(set3, age.slide!="0")# quitar los controles

set3$pH.water<-factor(set3$pH.water, levels = c('7', '7.2', '7.4', '7.7', '7.9', '8.1'),
                      ordered = TRUE)
set3$age.slide<-factor(set3$age.slide, levels = c('6', '9', '12'),
                       ordered = TRUE)


## --  Logistic regression: Settled slides vs. THE REST  ##

rest<-set3$settle.cont+set3$not.settle  # 

SvTR<-cbind(set3$settle.slides,rest)

mod3<-glm(SvTR~pH.water*age.slide, family = binomial, data = set3)

summary(mod3)

anova(mod3, test = 'Chisq')


## post-hoc: Have to test interaction, lots of comparisons!
emmeans(mod3, list(pairwise~pH.water), adjust = 'Tukey')

emmeans(mod3, list(pairwise~age.slide), adjust = 'Tukey')

emmeans(mod3, list(pairwise~pH.water:age.slide), adjust = 'Tukey')








