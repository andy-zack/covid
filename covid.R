library(tidyverse)
library(here)
library(lubridate)

vaccines <- read_csv("https://raw.githubusercontent.com/BloombergGraphics/covid-vaccine-tracker-data/master/data/historical-usa-doses-administered.csv") %>%
  filter(! id %in% c("new-york-city", "chicago"))

nyt_dat <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
  as_tibble() %>%
  mutate(county = tolower(county),
         state = tolower(state),
         pop = NA)

nyt_dat$pop[nyt_dat$county == "eagle"] <- 55127
nyt_dat$pop[nyt_dat$county == "garfield"] <- 60061
nyt_dat$pop[nyt_dat$county == "pitkin"] <- 17767
nyt_dat$pop[nyt_dat$county == "gunnison"] <- 6594
nyt_dat$pop[nyt_dat$county == "mesa"] <- 154210
nyt_dat$pop[nyt_dat$county == "summit"] <- 31011
nyt_dat$pop[nyt_dat$county == "rio blanco"] <- 6336
nyt_dat$pop[nyt_dat$county == "baltimore"] <- 827370
nyt_dat$pop[nyt_dat$county == "middlesex"] <- 825062
nyt_dat$pop[nyt_dat$county == "district of columbia"] <- 705749
nyt_dat$pop[nyt_dat$county == "san francisco"] <- 883305

nyt_dat %>%
  filter(state == "colorado") %>%
  filter(county %in% c("pitkin", "garfield", "eagle", "gunnison", "mesa", "summit", "rio blanco")) %>% 
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
  theme_minimal() +
  scale_color_brewer(palette = 6, type = "qual")

nyt_dat %>%
  filter(state %in% c("maryland", "colorado", "massachusetts", "district of columbia", "california")) %>%
  filter(county %in% c("baltimore", "garfield", "middlesex", "district of columbia", "san francisco")) %>% 
  group_by(county) %>%
  mutate(new_cases = cases - lag(cases)) %>% 
  mutate(adj_new_cases = new_cases / pop) %>%
  filter(adj_new_cases >=0) %>%
  ggplot(aes(x=date, y = adj_new_cases, color = county)) + 
  geom_point(alpha = .1) + 
  geom_smooth(span=.15, method = "loess", se = FALSE)
  
# make vaccines graph
vaccine_df <- vaccines %>%
  group_by(date) %>%
  summarise(cumulative_shots = sum(value)) %>%
  arrange(date) %>%
  mutate(daily_shots = cumulative_shots - lag(cumulative_shots, default = 0)) %>%
  mutate(day_num = 1 + as.numeric(difftime(date, min(vaccines$date), units = "days"))) %>%
  filter(daily_shots >=0)

vaccine_df <- vaccines %>% 
  group_by(date) %>% 
  summarise(cumulative_shots = sum(value)) %>% 
  arrange(date) %>% 
  mutate(day_num = 1 + as.numeric(difftime(date, min(vaccines$date), units = "days"))) %>% 
  filter( cumulative_shots - lag(cumulative_shots) >0 | day_num ==1) %>% 
  filter( cumulative_shots - lag(cumulative_shots) >0 | day_num ==1) %>% 
  filter( cumulative_shots - lag(cumulative_shots) >0 | day_num ==1) %>% 
  mutate(daily_shots = (cumulative_shots - lag(cumulative_shots)) / (day_num - lag(day_num)))

lm_fit <- lm(daily_shots ~ day_num, data = vaccine_df[1:50,])
lm_slope <- coef(lm_fit)[[2]]
lm_intercept <- coef(lm_fit)[[1]]

est_df <- tibble(date = seq(max(vaccine_df$date) +1, max(vaccine_df$date) + 60, by="days")) %>%
  mutate(day_num = 1 + as.numeric(difftime(date, min(vaccines$date), units = "days")))

vaccine_plot_df <- vaccine_df %>%
  bind_rows(est_df) %>%
  mutate(daily_est = lm_intercept + (day_num * lm_slope)) %>%
  mutate(my_est = (day_num * lm_intercept) + (.5 *day_num * day_num * lm_slope))

vaccine_plot_df %>%
  ggplot(aes(x=date, y = cumulative_shots)) +
  geom_vline(color = "black", linetype="dotted", xintercept = vaccine_plot_df %>% filter(my_est > 200000000) %>% filter(row_number() == 1) %>% pull(date)) +
  geom_point(alpha = .4) +
  geom_line(aes(y=my_est), color = "blue") +
  scale_y_continuous(label = scales::comma_format()) +
  theme_minimal() +
  ylab("Cumulative Shots") +
  xlab("") +
  ggtitle(paste("50% adults on", vaccine_plot_df %>% filter(my_est > 200000000) %>% filter(row_number() == 1) %>% pull(date)))

dates <- rep(date("1990-01-01"), nrow(vaccine_df))
for (i in 51:nrow(vaccine_df)) {
  
  lm_fit <- lm(daily_shots ~ day_num, data = vaccine_df[49:i,])
  lm_slope <- coef(lm_fit)[[2]]
  lm_intercept <- coef(lm_fit)[[1]]
  
  est_df <- tibble(date = seq(max(vaccine_df$date[50:i]) +1, max(vaccine_df$date[50:i]) + 160, by="days")) %>%
    mutate(day_num = 1 + as.numeric(difftime(date, min(vaccines$date), units = "days")))
  
  date_2M <- vaccine_df[50:i,] %>%
    bind_rows(est_df) %>%
    mutate(my_est = (day_num * lm_intercept) + (.5 *day_num * day_num * lm_slope)) %>%
    mutate(my_est = (day_num * lm_intercept) + (.5 *day_num * day_num * lm_slope)) %>%
    filter(my_est > 200000000) %>% filter(row_number() == 1) %>% pull(date)
  
  dates <- c(dates, date_2M)
}

dates <- dates[dates != date("1990-01-01")]

dates %>%
  as_tibble() %>%
  mutate(rn = row_number()) %>%
  ggplot(aes(y=value, x = rn)) +
  geom_line() +
  theme_minimal() +
  xlab("model run date") +
  ylab("day we will hit 2M vaccines")
