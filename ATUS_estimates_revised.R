library(tidyverse)
library(haven)

resp  <- read_dta('/Users/juliaheinzel/Documents/Back-up/Princeton/Fall 2025/591E/Report/Data/atusresp_2024.dta')
cps   <- read_dta('/Users/juliaheinzel/Documents/Back-up/Princeton/Fall 2025/591E/Report/Data/atuscps_2024.dta')
act   <- read_dta('/Users/juliaheinzel/Documents/Back-up/Princeton/Fall 2025/591E/Report/Data/atusact_2024.dta')

atus <- resp %>%
  left_join(cps, by = c("tucaseid", "tulineno"))

vol <- act %>%
  filter(tutier1code >= 150000 & tutier1code < 160000) %>%
  group_by(tucaseid) %>%
  summarise(daily_vol_minutes = sum(tuactdur, na.rm = TRUE))

atus <- atus %>%
  left_join(vol, by = "tucaseid") %>%
  mutate(daily_vol_minutes = replace_na(daily_vol_minutes, 0))

atus <- atus %>%
  mutate(monthly_vol_hours = 30 * daily_vol_minutes / 60)

atus <- atus %>%
  mutate(
    hh_size = case_when(
      trsppres == 3 & trchildnum == 0 ~ 1,
      trsppres != 3 & trchildnum == 0 ~ 2,
      trsppres != 3 & trchildnum != 0 ~ 2 + trchildnum,
      TRUE ~ 1
    ),
    in_poverty = case_when(
      hh_size == 1 & hefaminc <= 6 ~ 1,
      hh_size == 2 & hefaminc <= 7 ~ 1,
      hh_size == 3 & hefaminc <= 8 ~ 1,
      hh_size == 4 & hefaminc <= 9 ~ 1,
      hh_size == 5 & hefaminc <= 10 ~ 1,
      hh_size == 6 & hefaminc <= 11 ~ 1,
      hh_size == 7 & hefaminc <= 11 ~ 1,
      hh_size == 8 & hefaminc <= 12 ~ 1,
      TRUE ~ 0
    )
  )

result <- atus %>%
  filter(
    gestfips == 41,
    prtage > 18,
    prtage < 65,
    in_poverty == 1,
    monthly_vol_hours >= 80
  ) %>%
  summarise(oregon_poverty_volunteers = sum(tufinlwgt, na.rm = TRUE))

result

atus <- resp %>%
  left_join(cps, by = c("tucaseid", "tulineno"))

train <- act %>%
  filter(tutier2code >= 60301 & tutier2code < 60400) %>%
  group_by(tucaseid) %>%
  summarise(daily_training_minutes = sum(tuactdur, na.rm = TRUE))

atus <- atus %>%
  left_join(train, by = "tucaseid") %>%
  mutate(daily_training_minutes = replace_na(daily_training_minutes, 0))

atus <- atus %>%
  mutate(monthly_training_hours = 30 * daily_training_minutes / 60)

atus <- atus %>%
  mutate(
    hh_size = case_when(
      trsppres == 3 & trchildnum == 0 ~ 1,
      trsppres != 3 & trchildnum == 0 ~ 2,
      trsppres != 3 & trchildnum != 0 ~ 2 + trchildnum,
      TRUE ~ 1
    ),
    in_poverty = case_when(
      hh_size == 1 & hefaminc <= 6 ~ 1,
      hh_size == 2 & hefaminc <= 7 ~ 1,
      hh_size == 3 & hefaminc <= 8 ~ 1,
      hh_size == 4 & hefaminc <= 9 ~ 1,
      hh_size == 5 & hefaminc <= 10 ~ 1,
      hh_size == 6 & hefaminc <= 11 ~ 1,
      hh_size == 7 & hefaminc <= 11 ~ 1,
      hh_size == 8 & hefaminc <= 12 ~ 1,
      TRUE ~ 0
    )
  )

oregon_training <- atus %>%
  filter(
    gestfips == 41,         # Oregon
    prtage > 18,
    prtage < 65,
    in_poverty == 1,        # built earlier
    monthly_training_hours >= 80
  ) %>%
  summarise(num = sum(tufinlwgt, na.rm = TRUE))

oregon_training

