library(tidyverse)
library(skimr)
library(tidytuesdayR)

#I do not know much about Palomar Galaxy so I am making due with what I can infer
#and easily find.

#Data Import ----
tuesdata <-tt_load(2026, week = 32)

palomar_emission_lines <- tuesdata$palomar_emission_lines
palomar_survey <- tuesdata$palomar_survey

rm(tuesdata)

#Convert right ascension into degrees and declination into degrees and decimals
#Also join all into one data frame
palomar<-left_join(palomar_survey,palomar_emission_lines,by = "galaxy_name") %>% 
  mutate(
    ra_parts = str_split(ra_j2000, " "),
    ra_j2000 = map_dbl(
      ra_parts,
      ~ as.numeric(.x[1]) * 15 +
        as.numeric(.x[2]) * 0.25 +
        as.numeric(.x[3]) / 240
    ),
    dec_parts = str_split(dec_j2000, " "),
    dec_j2000 = map_dbl(
      dec_parts,
      ~ {
        sign <- ifelse(str_detect(.x[1], "^\\-"), -1, 1)
        sign * (
          abs(as.numeric(.x[1])) +
            as.numeric(.x[2]) / 60 +
            as.numeric(.x[3]) / 3600
        )
      }
    ),
    ngc = ifelse(str_detect(galaxy_name, "^NGC "), 1, 0)
  )

skim(palomar)

#galaxy map of sorts ----
palomar %>%
  drop_na(activity_type) %>% 
  ggplot(aes(x = ra_j2000, y = dec_j2000)) +
  geom_point(
    aes(color = activity_type),
    size = 2,
    alpha = 0.6
  ) +
  scale_x_reverse(
    limits = c(360, 0),
    breaks = seq(0, 360, by = 30)
  )+
  theme_classic() +
  labs(
    title = "Palomar Galaxy Distribution",
    x = "Right Ascension (degrees)",
    y = "Declination (degrees)",
    color = "Activity Type"
  )

#Activity type from 2 values ----
palomar %>% 
  drop_na(activity_type) %>% 
  ggplot(aes(y=log_oiii_hb,x=log_nii_ha))+
  geom_point(
    aes(color = activity_type),
    size = 2,
    alpha = 0.6
  )+
  theme_classic()+
  labs(
    color = "Activity Type"
  )

#Plot for NGC vs IC names
palomar %>% 
  drop_na(activity_type) %>% 
  ggplot(aes(y = log_oiii_hb, x = log_nii_ha)) +
  geom_point(
    aes(
      color = activity_type,
      shape = factor(ngc)
    ),
    size = 2.5,
    alpha = 0.7
  ) +
  scale_shape_manual(
    values = c("0" = 17, "1" = 16),
    labels = c("0" = "IC", "1" = "NGC")
  ) +
  theme_classic()+
  labs(x="log(nii/ha)",y="log(oiii/hb)",shape="NGC vs IC",color = "Activity Type")