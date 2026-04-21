#First Run
install.packages("tidyverse")
install.packages("fpp3")
install.packages("ggtime")

#Step 1 
library(tidyverse)
library(fpp3)
library(ggtime)

# Global Economy Scaling

global_economy |> distinct(Country) |> print(n=263)

# In-Class Exercises

global_economy %>%
  filter(Country=="United States")%>%
  autoplot(GDP)

global_economy %>%
  filter(Country=="New Zealand")%>%
  autoplot(GDP)

global_economy %>%
  filter(Country=="United States")%>%
  autoplot(GDP/Population)









## Retail CPI
## Access the data
print_retail <- aus_retail |>
  filter(Industry == "Newspaper and book retailing") |>
  group_by(Industry) |>
  index_by(Year = year(Month)) |>
  summarise(Turnover = sum(Turnover))

## Plot
print_retail |> autoplot(Turnover)

## Add in CPI Data
aus_economy <- global_economy |>
  filter(Code == "AUS")

## Join
#Note "Adjusted_turnover", pivot_longer
print_retail |>
  left_join(aus_economy, by = "Year") |>
  mutate(Adjusted_turnover = Turnover / CPI * 100) |>
  pivot_longer(c(Turnover, Adjusted_turnover), values_to = "Turnover") |>
  mutate(name = factor(name, levels = c("Turnover", "Adjusted_turnover"))) |>
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(title = "Turnover: Australian print media industry", y = "$AU")

## Food Retailing
food <- aus_retail |>
  filter(Industry == "Food retailing") |>
  summarise(Turnover = sum(Turnover))

#Plot
food |> autoplot(Turnover) +
  labs(y = "Turnover ($AUD)")
# SQRT
food %>% autoplot(sqrt(Turnover))
  labs(y="Sqrt Turnover($AUD)")

#CubeRoot

  food %>% autoplot((Turnover^(1/3)))
  labs(y="Cube root Turnover($AUD)")

#Log
food |> autoplot(log(Turnover)) +
  labs(y = "Log Turnover ($AUD)")

#Inverse
food |> autoplot(-1 / Turnover) +
  labs(y = "Inverse Turnover ($AUD)")
#Box Cox transformation
food |>
  features(Turnover, features = guerrero)

food |>
  autoplot(box_cox(Turnover, 5)) +
  labs(y = "Box-Cox transformed turnover")

#3-2
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)
us_retail_employment

#Always good to start by looking at the data
us_retail_employment |>
  autoplot(Employed) +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")

#STL is common model. "Seasonal, Trend and LOESS Decomposition"
us_retail_employment |>
  model(stl = STL(Employed))

# Store the STL model in a table so we can see what is going on
dcmp <- us_retail_employment |>
  model(stl = STL(Employed))
components(dcmp)

#Now Plot



#Note gray bar at left is showing scale
components(dcmp) |> autoplot()

#Another example
us_retail_employment |>
  autoplot(Employed, color = "gray") +
  autolayer(components(dcmp), trend, color = "#D55E00") +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")

#Check out seasonal
components(dcmp) |> gg_subseries(season_year)

us_retail_employment |>
  autoplot(Employed, color = "gray") +
  autolayer(components(dcmp), season_adjust, color = "#0072B2") +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")

#Moving Averages
global_economy |> filter(Country == "Australia") |>
  autoplot(Exports) + 
  labs(y="% of GDP", title= "Total Australian exports")

#5 Year Moving Average
format_num <- function(x) ifelse(is.na(x), "", format(x, nsmall = 2))
options(knitr.kable.NA = '')

aus_exports <- global_economy |>
  filter(Country == "Australia") |>
  transmute(Exports, `5-MA` = slider::slide_dbl(Exports, mean,.before = 2, .after = 2, .complete = TRUE)) 

out <- dplyr::bind_rows(
  head(aus_exports, 8),
  tail(aus_exports, 8)
) |>
  as_tibble() |>
  mutate_if(is.numeric, format_num) 

out[6,]=matrix(rep("...",3),nrow=1)

out <- dplyr::bind_rows(out[1:6,], 
                        out[11:16,]) |> 
  knitr::kable(booktabs=TRUE, digits=6)
out

#Moving Average Smoothing (3-MA)
aus_exports <- global_economy |>
  filter(Country == "Australia") |>
  transmute(Exports, `3-MA` = slider::slide_dbl(Exports, mean,.before = 1, .after = 1, .complete = TRUE))

aus_exports |> 
  autoplot(Exports) +
  autolayer(aus_exports,`3-MA`, color = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports: 3-MA") +
  guides(colour = guide_legend(title = "series")) 

#Moving Average Smoothing (5-MA)
aus_exports <- global_economy |>
  filter(Country == "Australia") |>
  transmute(Exports, `5-MA` = slider::slide_dbl(Exports, mean,.before = 2, .after = 2, .complete = TRUE))

aus_exports |> 
  autoplot(Exports) +
  autolayer(aus_exports,`5-MA`, color = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports: 5-MA") +
  guides(colour = guide_legend(title = "series")) 

#Moving Average Smoothing (7-MA)
aus_exports <- global_economy |>
  filter(Country == "Australia") |>
  transmute(Exports, `7-MA` = slider::slide_dbl(Exports, mean,.before = 3, .after = 3, .complete = TRUE))

aus_exports |> 
  autoplot(Exports) +
  autolayer(aus_exports,`7-MA`, color = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports: 7-MA") +
  guides(colour = guide_legend(title = "series")) 

#Moving Average Smoothing (15-MA)
aus_exports <- global_economy |>
  filter(Country == "Australia") |>
  transmute(Exports, `15-MA` = 
              slider::slide_dbl(Exports, mean,.before = 7, .after = 7, .complete = TRUE))

aus_exports |> 
  autoplot(Exports) +
  autolayer(aus_exports,`15-MA`, color = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports: 15-MA") +
  guides(colour = guide_legend(title = "series")) 

out <- dply

#Classical Decomp Additive
us_retail_employment |>
  model(classical_decomposition(Employed, type = "additive")) |>
  components() |>
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition of total US retail employment")

#Classical Decomp Multiplicative
us_retail_employment |>
  model(classical_decomposition(Employed, type = "multiplicative")) |>
  components() |>
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition of total US retail employment")

