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

### Prediction Intervals

## Refresh on the data

# Re-index based on trading days
google_stock <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2015) |>
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE)

# Filter the year of interest
google_2015 <- google_stock |> filter(year(Date) == 2015)

# Generate Prediction Interval

google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10) |>
  hilo() #hilo 80/95% by default

# Plot
google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10) |>
  autoplot(google_2015) +
  labs(title="Google daily closing stock price", y="$US" )

# Bootstrapping Residuals [e.g. simulating data collection by sampling
# original dataset w/ replacement. Also addresses normality.]

# 5 sample paths across 30 days
# Note: By design, your samples will likely look different
fit <- google_2015 |>
  model(NAIVE(Close))
sim <- fit |> generate(h = 30, times = 5, bootstrap = TRUE)
sim

# Plot the bootstrapping
google_2015 |>
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
            data = sim) +
  labs(title="Google daily closing stock price", y="$US" ) +
  guides(colour = "none")

# Forecasting [Note 5000 Sample Paths]
fc <- fit |> forecast(h = 30, bootstrap = TRUE)
fc

autoplot(fc, google_2015) +
  labs(title="Google daily closing stock price", y="$US" )

# Control number of bootstrap samples
google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10, bootstrap = TRUE, times = 1000) |>
  hilo() 

# Explore: https://ebsmonash.shinyapps.io/BootstrapDistributions/?showcase=0

### Forecasting using transformations

# Bias in the transformation
fc <- prices |>
  filter(!is.na(eggs)) |>
  model(RW(log(eggs) ~ drift())) |>
  forecast(h = 50) |>
  mutate(.median = median(eggs))

fc |>
  autoplot(prices |> filter(!is.na(eggs)), level = 80) +
  geom_line(aes(y = .median), data = fc, linetype = 2, col = "blue") +
  labs(title = "Annual egg prices",
       y = "$US (in cents adjusted for inflation) ")
# Dashed line = Bias = median; solid line = Bias-Adjusted = mean

### Forecasting with Decomposition

## Naive forecast of seasonally adjusted data from STL decomp
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade")

dcmp <- us_retail_employment |>
  model(STL(Employed ~ trend(window = 7), robust = TRUE)) |>
  components() |>
  select(-.model)

dcmp |>
  model(NAIVE(season_adjust)) |>
  forecast() |>
  autoplot(dcmp) +
  labs(y = "Number of people",
       title = "US retail employment")

## Forecasts - naive fc of sa data; SNAIVE of seasonal component
fit_dcmp <- us_retail_employment |>
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))

fit_dcmp |>
  forecast() |>
  autoplot(us_retail_employment)+
  labs(y = "Number of people",
       title = "US retail employment")

fit_dcmp |> gg_tsresiduals()
# Note the autocorrelations