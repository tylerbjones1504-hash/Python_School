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

### Subsetting A Time Series

## Two methods to subset a time series

# Extracts data from 1995 onward
aus_production |> filter(year(Quarter) >= 1995)
aus_production |> filter_index("1995 Q1" ~ .)

# Extracts last 20 observations
aus_production |>
  slice(n()-19:0)

# Group / Slice
aus_retail |>
  group_by(State, Industry) |>
  slice(1:12) |> print (n=100)

### Train / Test

## Beer

# Subset the data
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
beer_train <- recent_production |>
  filter(year(Quarter) <= 2007)

# Train the model
beer_fit <- beer_train |>
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )

# Generate a forecast
beer_fc <- beer_fit |>
  forecast(h = 10)

# Plot Method outcomes vs. actual
beer_fc |>
  autoplot(
    aus_production |> filter(year(Quarter) >= 1992),
    level = NULL
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

# Check for accuracy
accuracy(beer_fc, recent_production)

## Google Data

# Re-index based on trading days
google_stock <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2015) |>
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE)

# Filter the year of interest
google_2015 <- google_stock |> filter(year(Date) == 2015)
google_jan_2016 <- google_stock |>
  filter(yearmonth(Date) == yearmonth("2016 Jan"))

# Train the model
google_fit <- google_2015 |>
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = RW(Close ~ drift())
  )

# Generate a forecast
google_fc <- google_fit |>
  forecast(google_jan_2016)

# Plot Method outcomes vs. actual
google_fc |>
  autoplot(bind_rows(google_2015, google_jan_2016),
           level = NULL) +
  labs(y = "$US",
       title = "Google closing stock prices from Jan 2015") +
  guides(colour = guide_legend(title = "Forecast"))

# Check for accuracy
accuracy(google_fc, google_stock)


### Evaluating Accuracy

# Plot Google Naive (Best model)
google_fc |>
  filter(.model == "Naïve") |>
  autoplot(bind_rows(google_2015, google_jan_2016), level=80)+
  labs(y = "$US",
       title = "Google closing stock prices")

# Quantile Scores
google_fc |>
  filter(.model == "Naïve", Date == "2016-01-04") |>
  accuracy(google_stock, list(qs=quantile_score), probs=0.10)

# Winkler Score
google_fc |>
  filter(.model == "Naïve", Date == "2016-01-04") |>
  accuracy(google_stock,
           list(winkler = winkler_score), level = 80)

# Continuous Ranked Probability Score
google_fc |>
  accuracy(google_stock, list(crps = CRPS))

# Scale-free comparisons
google_fc |>
  accuracy(google_stock, list(skill = skill_score(CRPS)))

# TS Crossvalidation Accuracy
google_2015_tr <- google_2015 |>
  stretch_tsibble(.init = 3, .step = 1) |>
  relocate(Date, Symbol, .id)
google_2015_tr

# TSCV accuracy
google_2015_tr |>
  model(RW(Close ~ drift())) |>
  forecast(h = 1) |>
  accuracy(google_2015)

# Training set accuracy
google_2015 |>
  model(RW(Close ~ drift())) |>
  accuracy()