#First Run
install.packages("tidyverse")
install.packages("fpp3")
install.packages("ggtime")
install.packages("seasonal")

#Step 1 
library(tidyverse)
library(fpp3)
library(ggtime)
library(seasonal)

### Moving Average-Based
# Additive Decomposition
us_retail_employment |>
  model(classical_decomposition(Employed, type = "additive")) |>
  components() |>
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition of total US retail employment")

#Multiplicative Decomposition
us_retail_employment |>
  model(classical_decomposition(Employed, type = "multiplicative")) |>
  components() |>
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition of total US retail employment")

### ARIMA-adjacent Model w/ Limitations
#X-11 Decomp Data Setup
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)

#X-11 Decomp
x11_dcmp <- us_retail_employment |>
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) |>
  components()

autoplot(x11_dcmp)

x11_dcmp |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Employed, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

x11_dcmp |>
  gg_subseries(seasonal)

#X-13 Decomp
seats_dcmp <- us_retail_employment |>
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) |>
  components()
autoplot(seats_dcmp)

### STL

#Data Prep
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)

### Basic STL
us_retail_employment |>
  model(STL(Employed)) |>
  components() |>
  autoplot() + labs(title = "STL decomposition: US retail employment")

### STL w/ Specials 1 (Infinite Window)
us_retail_employment |>
  model(STL(Employed ~ season(window = "periodic"))) |>
  components() |>
  autoplot() + labs(title = "STL decomposition: US retail employment")

### STL w/ Specials 2 (Seasonal Window)
us_retail_employment |>
  model(STL(Employed ~ season(window = 49))) |>
  components() |>
  autoplot() + labs(title = "STL decomposition: US retail employment")

### STL w/ Specials 3 (Seasonal, Trends)
us_retail_employment |>
  model(STL(Employed ~ season(window = 15) + trend(window=15))) |>
  components() |>
  autoplot() + labs(title = "STL decomposition: US retail employment")

### STL w/ Specials 4 (Seasonal, Trends, Robust)
us_retail_employment |>
  model(STL(Employed ~ season(window = 15) + trend(window = 15), robust = TRUE)) |>
  components() |>
  autoplot() + labs(title = "STL decomposition: US retail employment")

### TS Features

# Single Calculation
tourism |>
  features(Trips, list(mean = mean)) |>
  arrange(mean)

# Multiple Calculations
tourism |> features(Trips, quantile)

# ACF (Autocorrelations)
tourism |> features(Trips, feat_acf)

# STL 
tourism |>
features(Trips, feat_stl)

tourism |>
  features(Trips, feat_stl) |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year,
             col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State))

tourism |>
  features(Trips, feat_stl) |>
  filter(
    seasonal_strength_year == max(seasonal_strength_year)
  ) |>
  left_join(tourism, by = c("State", "Region", "Purpose"), multiple = "all") |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State, Region, Purpose))