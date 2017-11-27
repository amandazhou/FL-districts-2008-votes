## QSS 30.06 "Race, Incarceration and Politics"
## Homework 8
## Amanda Zhou May 26, 2017

library(tidyverse)

#Sums the total population for each district and calculates the mean and large standard deviation
q1 <- fl %>%
  group_by(cd113) %>%
  summarize(DistrictPop = sum(TotPop))

mean(q1$DistrictPop)
sd(q1$DistrictPop)

q2 <- fl %>%
  group_by(cd113) %>%
  summarize(VAPpop = sum(VAP))

mean(q2$VAPpop)
sd(q2$VAPpop)

q3 <- fl %>%
  summarize(totalpop = sum(TotPop),
            totalblack = sum(BlackPop)) %>%
  mutate(percent = totalblack/totalpop *100)

q3$percent

q3 <- fl %>%
  group_by(cd113) %>%
  summarize(totalpop = sum(TotPop),
            totalblack = sum(BlackPop)) %>%
  mutate(percent = totalblack/totalpop *100)

length(q3$percent[q3$percent > 50])/27 *100

q4 <- fl %>%
  summarize(totalpop = sum(TotPop),
            totalhisp = sum(HispPop)) %>%
  mutate(percent = totalhisp/totalpop *100)

q4$percent

q4 <- fl %>%
  group_by(cd113) %>%
  summarize(totalpop = sum(TotPop),
            totalhisp = sum(HispPop)) %>%
  mutate(percent = totalhisp/totalpop *100)

length(q4$percent[q4$percent > 50])/27 *100

q5<-fl%>%
  summarise(mccainsum = sum(mccain),
            obamasum = sum(obama)) %>%
  mutate(mccainvote = mccainsum/(obamasum+mccainsum))

q5$mccainvote *100

q5<-fl%>%
  group_by(cd113) %>%
  summarise(mccainsum = sum(mccain),
            obamasum = sum(obama)) %>%
  mutate(mccainvote = mccainsum/(obamasum+mccainsum) *100)

length(q5$mccainvote[q5$mccainvote > 50])/27 *100

q6 <- fl %>%
  filter(VAP > 200) %>%
  mutate(blackshare = BlackVAP/(VAP), 
         McCainShare = mccain/(mccain+obama))

ggplot(data = q6, aes(x=blackshare,y=McCainShare)) + geom_point() + geom_smooth()+theme_bw() + 
  xlab("Black Share of the Voting Age Population") + ylab("McCain Share of the Vote")

q7 <- fl %>%
  filter(VAP > 200) %>%
  mutate(blackshare = BlackVAP/(VAP), 
         turnout = (mccain + obama)/VAP)

ggplot(data = q7, aes(x=blackshare,y=turnout)) + geom_point() + geom_smooth() + scale_y_continuous(breaks= seq(0,1,.1), limits = c(0,1) ) +
  theme_bw() + ylab("Turnout") + xlab("Black Share of the Vote")


q7 <- fl %>%
  filter(VAP > 200) %>%
  mutate(hispshare = HispVAP/(VAP), 
         turnout = (mccain + obama)/VAP)

ggplot(data = q7, aes(x=hispshare,y=turnout)) + geom_point() + geom_smooth() +theme_bw() + scale_y_continuous(breaks= seq(0,1,.1), limits = c(0,1) ) + 
  ylab("Turnout") + xlab("Hispanic Share of the Vote")