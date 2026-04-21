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

#### Basic Workflow
# Data Prep
gdppc <- global_economy |>
  mutate(GDP_per_capita = GDP / Population) |>
  select(Year, Country, GDP, Population, GDP_per_capita)

gdppc

# Data Visualization

gdppc |>
  filter(Country == "Sweden") |>
  autoplot(GDP_per_capita) +
  labs(title = "GDP per capita for Sweden", y = "$US")

# Model Estimation
fit <- gdppc |>
  model(trend_model = TSLM(GDP_per_capita ~ trend()))
# Note: TLSM is a specific model (regression based)
# See others below

fit

# (Placeholder) Evaluate Model Performance

# Produce Forecasts
fit |> forecast(h = "3 years")

# Plot forecast w/ historical data
fit |>
  forecast(h = "3 years") |>
  filter(Country == "Sweden") |>
  autoplot(gdppc) +
  labs(y = "$US", title = "GDP per capita for Sweden")

#### Simple Methods

## MEAN, NAIVE, SEASONAL NAIVE (Beer Benchmark)

# Set training data from 1992 to 2006

train <- aus_production |>
  filter_index("1992 Q1" ~ "2006 Q4")

# Fit the models

beer_fit <- train |>
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )

# Generate forecasts for 14 quarters

beer_fc <- beer_fit |> forecast(h = 14)

# Plot forecasts against actual values

beer_fc |>
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(aus_production, "2007 Q1" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

# Residual Analysis

augment(beer_fit)

## MEAN, NAIVE, DRIFT (Stock Benchmark)

# Re-index based on trading days
google_stock <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2015) |>
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE)

# Filter the year of interest
google_2015 <- google_stock |> filter(year(Date) == 2015)

# Fit the models
google_fit <- google_2015 |>
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )

# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock |>
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit |>
  forecast(new_data = google_jan_2016)

# Plot the forecasts
google_fc |>
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, colour = "black") +
  labs(y = "$US",
       title = "Google daily closing stock prices",
       subtitle = "(Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast"))

# Residual Analysis (NAIVE)
aug <- google_2015 |>
  model(NAIVE(Close)) |>
  augment()
autoplot(aug, .innov) +
  labs(y = "$US",
       title = "Residuals from the naïve method")
aug |>
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")
#Note length of right tail

aug |>
  ACF(.innov) |>
  autoplot() +
  labs(title = "Residuals from the naïve method")

# Combined Diagnostic Graphs
google_2015 |>
  model(NAIVE(Close)) |>
  gg_tsresiduals()

# Autocorrelation Tests
# AKA Portmanteau Test

# Box-Pierce
aug |> features(.innov, box_pierce, lag = 10)

# Ljung-Box
aug |> features(.innov, ljung_box, lag = 10)