library(tidyverse)
library(here)
library(lubridate)

nyt_dat <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
  as_tibble() %>%
  mutate(county = tolower(county),
         state = tolower(state),
         pop = NA)

nyt_dat$pop[nyt_dat$county == "eagle"] <- 55127
nyt_dat$pop[nyt_dat$county == "garfield"] <- 60061
nyt_dat$pop[nyt_dat$county == "pitkin"] <- 17767
nyt_dat$pop[nyt_dat$county == "gunnison"] <- 6594
nyt_dat$pop[nyt_dat$county == "delta"] <- 8920
nyt_dat$pop[nyt_dat$county == "mesa"] <- 154210
nyt_dat$pop[nyt_dat$county == "summit"] <- 31011
nyt_dat$pop[nyt_dat$county == "lake"] <- 696535
nyt_dat$pop[nyt_dat$county == "rio blanco"] <- 6336
nyt_dat$pop[nyt_dat$county == "baltimore"] <- 827370
nyt_dat$pop[nyt_dat$county == "middlesex"] <- 825062
nyt_dat$pop[nyt_dat$county == "district of columbia"] <- 705749
nyt_dat$pop[nyt_dat$county == "san francisco"] <- 883305

nyt_dat %>%
  filter(state == "colorado") %>%
  filter(county %in% c("pitkin", "garfield", "eagle", "gunnison", "delta", "mesa", "summit", "lake", "rio blanco")) %>% 
  mutate(`cases per 100 people` = 100 * cases / pop) %>%
  ggplot(aes(x=date, y = `cases per 100 people`, color = county)) + 
  geom_line(size=.8) +
  theme_minimal() +
  scale_color_brewer(type = "qual", palette = 6)

nyt_dat %>%
  filter(state %in% c("maryland", "colorado", "massachusetts", "district of columbia", "california")) %>%
  filter(county %in% c("baltimore", "garfield", "middlesex", "district of columbia", "san francisco")) %>% 
  mutate(`cases per 100 people` = 100 * cases / pop) %>%
  ggplot(aes(x=date, y = `cases per 100 people`, color = county)) + 
  geom_line(size = 1) +
  theme_minimal() +
  scale_color_brewer(type = "qual", palette = 6)

nyt_dat %>%
  filter(state %in% c("maryland", "colorado", "massachusetts", "district of columbia", "california")) %>%
  filter(county %in% c("baltimore", "garfield", "middlesex", "district of columbia", "san francisco")) %>% 
  mutate(`Deaths Per 100000 People` = 100000 * deaths / pop) %>%
  ggplot(aes(x=date, y = `Deaths Per 100000 People`, color = county)) + 
  geom_line(size = 1) +
  theme_minimal() +
  scale_color_brewer(type = "qual", palette = 6)

nyt_dat %>%
  filter(state %in% c("maryland", "colorado", "massachusetts", "district of columbia", "california")) %>%
  filter(county %in% c("baltimore", "garfield", "middlesex", "district of columbia", "san francisco")) %>% 
  mutate(span = date %--% max(date),
         time_period = floor(as.duration(span)/ dweeks(1))) %>%
  group_by(county, time_period) %>%
  summarise(cases = max(cases),
            pop = min(pop),
            date_label = min(date),
            days_in_week = n() ) %>%
  group_by(county) %>%
  arrange(date_label) %>%
  mutate(new_cases = cases - lag(cases)) %>%
  mutate(new_cases_per_100 = 100 * new_cases / pop) %>% 
  filter(new_cases_per_100 > -.05) %>%
  ggplot(aes(x = date_label, y = new_cases_per_100, color = county)) +
  geom_line(size = 1) +
  geom_point() +
  # theme_minimal() +
  ylab("Weekly New Cases per 100") +
  xlab("Date") +
  scale_color_brewer(palette = 6, type = "qual")
  
