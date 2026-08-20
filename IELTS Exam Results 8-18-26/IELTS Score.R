#Libraries
library(tidytuesdayR)
library(tidyverse)
library(skimr)

tuesdata <-tt_load(2026, week = 33)

demo_by_first_language <- tuesdata$demo_by_first_language
demo_by_nationality <- tuesdata$demo_by_nationality
demo_by_reasons <- tuesdata$demo_by_reasons
performance_by_first_language <- tuesdata$performance_by_first_language
performance_by_nationality <- tuesdata$performance_by_nationality

rm(tuesdata)

skim_without_charts(demo_by_first_language)
skim_without_charts(demo_by_nationality)
skim_without_charts(demo_by_reasons)
skim_without_charts(performance_by_first_language)
skim_without_charts(performance_by_nationality)

demo_by_first_language %>% 
  ggplot(aes(x=band,y=percent))+
  geom_col()+
  facet_wrap(~language)+
  theme_classic()+
  labs(x="Score Band", y="Percent")

demo_by_nationality %>% 
  ggplot(aes(x=band,y=percent))+
  geom_col()+
  theme_classic()+
  labs(x="Score Band", y="Percent")+
  facet_wrap(~nationality)

demo_by_reasons %>% 
  ggplot(aes(x=band,y=percent))+
  geom_col()+
  theme_classic()+
  labs(x="Score Band", y="Percent")+
  facet_wrap(~reason)

performance_by_first_language %>% 
  ggplot(aes(x=part,y=score))+
  geom_col()+
  theme_classic()+
  labs(x="Exam Section",y="Mean Score")+
  facet_wrap(~language)+
  theme(axis.text.x = element_text(angle=45,hjust = 1,vjust=1))

performance_by_nationality %>% 
  ggplot(aes(x=part,y=score))+
  geom_col()+
  theme_classic()+
  labs(x="Exam Section",y="Mean Score")+
  facet_wrap(~nationality)+
  theme(axis.text.x = element_text(angle=45,hjust = 1,vjust=1))
