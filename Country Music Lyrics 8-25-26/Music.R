library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(skimr)

# Data Load ----
tuesdata <- tidytuesdayR::tt_load(2026, week = 34)

country_lyrics <- tuesdata$country_lyrics
top_all_writers <- tuesdata$top_all_writers
top_primary_writers <- tuesdata$top_primary_writers
top_producers <- tuesdata$top_producers
rm(tuesdata)

#Exploratory Data Analysis ----
skim_without_charts(country_lyrics)
skim_without_charts(top_all_writers)
skim_without_charts(top_primary_writers)
skim_without_charts(top_producers)

#Split Strings for lyrics and titles ----
title_frequencies<-country_lyrics %>% unnest_tokens(split_title,song) %>% 
  count(split_title,sort=TRUE) %>% 
  left_join(parts_of_speech,by = c("split_title"= "word"),multiple="first")
  

lyric_frequencies<-country_lyrics %>% unnest_tokens(split_lyrics,lyrics) %>% 
  count(split_lyrics,sort=TRUE) %>% 
  left_join(parts_of_speech, by = c("split_lyrics"="word"),multiple = "first")

title_frequencies %>% 
  filter(n>10) %>% 
  ggplot(aes(x=reorder(split_title,n),y=n))+
  geom_col(aes(fill=pos))+
  coord_flip()+
  labs(x="Word",y="Occurences",title="Most Common Words in Country Song Titles",
       fill="Part of Speech"
       )+
  theme_classic()

#Viz for word occurences ----
lyric_frequencies %>% filter(n>750) %>% 
  ggplot(aes(x=reorder(split_lyrics,n),y=n,fill=pos))+
  geom_col()+
  coord_flip()+
  labs(x="Word",y="Occurences",fill="Part of Speech",
       title="Most Common Words in Country Song Lyrics")+
  theme_classic()

title_frequencies %>% 
  drop_na() %>% 
  group_by(pos) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=reorder(pos,-count),y=count))+
  geom_col()+
  theme_classic()+
  labs(x="Part of Speech",y="Occurences",title="Part of Speech Occurences in Country Song Titles")+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1))

lyric_frequencies %>% 
  drop_na() %>%
  group_by(pos) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=reorder(pos,-count),y=count))+
  geom_col()+
  theme_classic()+
  labs(x="Part of Speech",y="Occurences",title="Part of Speech Occurences in Country Song Lyrics")+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1))

#Popular Words by year----
country_lyrics %>% unnest_tokens(split_song,song) %>% 
  group_by(entered_top_30_in,split_song) %>% 
  summarize(count = n()) %>% 
  filter(count>5) %>% 
  ggplot(aes(x=entered_top_30_in,y=count,color=split_song))+
  geom_point()+
  geom_line()+
  theme_classic()+
  labs(x="Year",y="Occurences",color="Word",
       title="Most Popular Word in Country Song Titles by Year")+
  scale_y_continuous(breaks=seq(6,16,2), limits=(c(6,16)))
