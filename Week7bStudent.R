#First Run
install.packages("tidyverse")
install.packages("fpp3")
install.packages("ggtime")
install.packages("seasonal")
### Something New...
install.packages("GGally")

#Step 1 
library(tidyverse)
library(fpp3)
library(ggtime)
library(seasonal)
library("GGally")

# 13

aus_cafe <- aus_retail |>
  filter(Industry == "Cafes, restaurants and takeaway food services",
         year(Month) %in% 2004:2018) |>
  summarise(Turnover = sum(Turnover))
aus_cafe |> autoplot(Turnover)

# 14

fit <- aus_cafe |>
  model(
    K1 = TSLM(log(Turnover) ~ trend() + fourier(K = 1)),
    K2 = TSLM(log(Turnover) ~ trend() + fourier(K = 2)),
    K3 = TSLM(log(Turnover) ~ trend() + fourier(K = 3)),
    K4 = TSLM(log(Turnover) ~ trend() + fourier(K = 4)),
    K5 = TSLM(log(Turnover) ~ trend() + fourier(K = 5)),
    K6 = TSLM(log(Turnover) ~ trend() + fourier(K = 6))
  )
glance(fit) |> select(.model, r_squared, adj_r_squared, CV, AICc)

# 15

cafe_plot <- function(...) {
  fit |>
    select(...) |>
    forecast() |>
    autoplot(aus_cafe) +
    labs(title = sprintf("Log transformed %s, trend() + fourier(K = %s)", model_sum(select(fit, ...)[[1]][[1]]), deparse(..1))) +
    geom_label(
      aes(x = yearmonth("2007 Jan"), y = 4250, label = paste0("AICc = ", format(AICc))),
      data = glance(select(fit, ...))
    ) +
    geom_line(aes(y = .fitted), colour = "red", augment(select(fit, ...))) +
    ylim(c(1500, 5100))
}

# 16

cafe_plot(K = 1)

cafe_plot(K = 2)

cafe_plot(K = 3)

cafe_plot(K = 4)

cafe_plot(K = 5)

cafe_plot(K = 6)

# 17

recent_production <- aus_production |> filter(year(Quarter) >= 1992)
recent_production |> model(TSLM(Beer ~ trend() + season())) |> 
  forecast() |> autoplot(recent_production)

# 18

fit_consBest <- us_change |>
  model(
    TSLM(Consumption ~ Income + Savings + Unemployment)
  )

future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) |>
    mutate(Income = 1, Savings = 0.5, Unemployment = 0),
  Decrease = new_data(us_change, 4) |>
    mutate(Income = -1, Savings = -0.5, Unemployment = 0),
  names_to = "Scenario"
)

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change |> autoplot(Consumption) +
  labs(y = "% change in US consumption") +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")

# 19

marathon <- boston_marathon |>
  filter(Event == "Men's open division") |>
  select(-Event) |>
  mutate(Minutes = as.numeric(Time) / 60)
marathon |> autoplot(Minutes) + labs(y = "Winning times in minutes")

# 20

fit_trends <- marathon |>
  model(
    # Linear trend
    linear = TSLM(Minutes ~ trend()),
    # Exponential trend
    exponential = TSLM(log(Minutes) ~ trend()),
    # Piecewise linear trend
    piecewise = TSLM(Minutes ~ trend(knots = c(1940, 1980)))
  )

# 21

fit_trends |>
  forecast(h = 10) |>
  autoplot(marathon)

fc_trends <- fit_trends |> forecast(h = 10)
marathon |>
  autoplot(Minutes) +
  geom_line(
    data = fitted(fit_trends),
    aes(y = .fitted, colour = .model)
  ) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(
    y = "Minutes",
    title = "Boston marathon winning times"
  )

# 22

fit_trends |>
  select(piecewise) |>
  gg_tsresiduals()