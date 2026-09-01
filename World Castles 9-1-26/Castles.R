library(tidyverse)
library(tidytuesdayR)
library(skimr)
library(leaflet)

#Data Load ----
world_castles_df<-tt_load(2026,week=35) %>% pluck(1)

#Exploratory Analysis ----
skim_without_charts(world_castles_df)

#Basic Charts ----
world_castles_df %>% 
  group_by(country) %>% 
  summarize(total_visits = sum(pageviews)) %>% 
  slice_max(total_visits, n=10) %>% 
  ggplot(aes(x=reorder(country,total_visits),y=total_visits))+
  geom_col()+
  coord_flip()+
  theme_minimal()+
  labs(y="Total Vistors to Wiki Page", x="Country")+
  scale_y_continuous(labels = scales::comma)


world_castles_df %>% 
  #Top Ten From Before
  filter(country %in% c("England","India","France","Germany","China","Italy",
                        "Scotland","United States","Japan","Spain")) %>% 
  group_by(country, category) %>% 
  summarise(total_visits = sum(pageviews)) %>% 
  group_by(country) %>% 
  mutate(total_visits_2 = sum(total_visits)) %>% 
  ungroup() %>% 
  mutate(country = reorder(country, -total_visits_2)) %>% 
  ggplot(aes(x = country, y = total_visits, fill = str_to_title(category))) +
  geom_col(position = "stack") +
  theme_minimal() +
  labs(x = "Country", y = "Total Wiki Page Visits", fill = "Castle Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(labels = scales::comma)


#World Map ----
#Static Map
world_castles_df %>% 
  ggplot(aes(x=lon,y=lat))+
  annotation_borders("world", colour = "gray85", fill = "gray95") +
  geom_point(aes(color=category),alpha=0.6,size=1)+
  coord_quickmap()+
  labs(color="Castle Type",x=NULL,y=NULL)+
  theme_minimal()

#Movable Map
pal <- colorFactor(palette = "Set2", domain = world_castles_df$category)

leaflet(world_castles_df) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, 
                   radius = 3, color = ~pal(category), 
                   popup = ~paste(name, "<br>",country, "<br>", str_to_title(category))) %>% 
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~category,
    title = "Castle Type"
  )
