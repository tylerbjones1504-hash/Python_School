library(tidyverse)
gapminder=read_csv("gapminder.csv")

gapminder %>%
  filter(country %in% c("Sri Lanka",
                        "Turkey"))%>%
  filter(year==2010)%>%
  select(country, infant_mortality)

gapminder %>%
  filter(country %in% c("Sri Lanka",
                        "Turkey"))%>%
  ggplot()+
  geom_line(aes(x=year, y=infant_mortality, color=country))

gapminder %>%
  filter(country %in% c("Poland",
                        "South Korea"))%>%
  ggplot()+
  geom_line(aes(x=year, y=infant_mortality, color=country))

#visualize infant mortality by western rich countries vs the developing world like India, China, Brazil, etc
#we can use the %in% operator to filter for multiple countries at once also use a color aesthetic to differentiate the countries in the plot
#do 7 total countries and a line plot of infant mortality over time. Include US UK Ghana Peru Venesuela, Iran, and Poland
#Use some sort of format that makes it more visually appealing like a theme or something. Also add a title and axis labels to make it more informative.
gapminder %>%
  filter(country %in% c("United States",
                        "United Kingdom",
                        "Ghana",
                        "Peru",
                        "Venezuela",
                        "Iran",
                        "Poland"))%>%
  ggplot()+
  geom_line(aes(x=year, y=infant_mortality, color=country))+
  labs(title="Infant Mortality Over Time",
       x="Year",
       y="Infant Mortality (per 1000 live births)")+
  theme_minimal()
#after, focus on a specific year, is there a division among these countris in terms
#of infant mortaity and life expectancy

gapminder %>%
  filter(country %in% c("United States",
                        "United Kingdom",
                        "Ghana",
                        "Peru",
                        "Venezuela",
                        "Iran",
                        "Poland"))%>%
  filter(year==2010)%>%
  ggplot()+
  geom_point(aes(x=infant_mortality, y=life_expectancy, color=country))+
  labs(title="Infant Mortality vs Life Expectancy in 2010",
       x="Infant Mortality (per 1000 live births)",
       y="Life Expectancy (years)")+
  theme_minimal()
#facet wrap it over years. Group by continent and see if there are any patterns in infant mortality and life expectancy across different continents. Use a scatter plot with infant mortality on the x-axis and life expectancy on the y-axis, and color the points by continent. Facet wrap by year to see how these relationships change over time.
#Include all countries for this visualization, not just the 7 we have been focusing on. This will allow us to see if there are any global patterns in infant mortality and life expectancy across different continents and how they change over time.
#only do 1 year
gapminder %>%
  filter(year==2010)%>%
  ggplot()+
  geom_point(aes(x=infant_mortality, y=life_expectancy, color=continent))+
  labs(title="Infant Mortality vs Life Expectancy in 2010",
       x="Infant Mortality (per 1000 live births)",
       y="Life Expectancy (years)")+
  theme_minimal()

#do first vs last year i only see one chart in the plot, so maybe facet wrap by year to see the difference between the two years more clearly
#i only see one plot. put them side by side
gapminder %>%
  filter(year %in% c(1960, 2010))%>%
  ggplot()+
  geom_point(aes(x=infant_mortality, y=life_expectancy, color=continent))+
  labs(title="Infant Mortality vs Life Expectancy in 1952 and 2010",
       x="Infant Mortality (per 1000 live births)",
       y="Life Expectancy (years)")+
  theme_minimal()+
  facet_wrap(~year)

#do a scatter plot by continent of infant mortality. over time facet wrap continent.smaller dots
gapminder %>%
  ggplot()+
  geom_point(aes(x=year, y=infant_mortality), size=1)+
  labs(title="Infant Mortality Over Time by Continent",
       x="Year",
       y="Infant Mortality (per 1000 live births)")+
  theme_minimal()+
  facet_wrap(~continent)

library(ggrepel)
library(GGally)
library(scales)
library(ggthemes)
library(ggtext)

gapminder%>%
  mutate(gdp_per_capita=gdp/population)%>%
  filter(year==1960)%>%
  ggplot()+
  geom_density(aes(x=gdp_per_capita, fill=continent), bw=.1)+
  scale_x_continuous(trans="log10",
                     labels=label_number(),
                     limits=c(50,20000))+
  geom_rug(aes(x=gdp_per_capita), size=.5)

install.packages("ggplot2")  # if needed
library(ggplot2)

jitter_pos <- position_jitter(width = 0.3, height = 0)
ggplot(df, aes(x, y)) +
  geom_point(position = jitter_pos)

  
gapminder%>%
  mutate(gdp_per_capita=gdp/population)%>%
  filter(year==1960)%>%
  filter(gdp_per_capita>9000)%>%
  select(country)

gapminder%>%
  mutate(gdp_per_capita=gdp/population)%>%
  filter(year==1960)%>%
  ggplot()+
  geom_boxplot(aes(gdp_per_capita,y= continent))

gapminder%>%
  mutate(gdp_per_capita=gdp/population,
         continent=factor(continent,
                          levels =c("Europe",
                                    "Americas",
                                    "Oceania",
                                    "Asia",
                                    "Africa")))%>%
  filter(year==1960)%>%
  ggplot(aes(x=gdp_per_capita, y=continent))+
  geom_boxplot(aes(fill=continent),
               outlier.color = NA)+
  geom_point(position = jitter_pos)+
  geom_text_repel(aes(label=country),
                  position = jitter_pos)+
  scale_x_continuous(trans="log10")

#define identify a group of rich countries
#create a new variable called group to show rich vs poor
gapminder%>%
  mutate(gdp_per_capita=gdp/population,
         group=ifelse(gdp_per_capita>9000, "rich", "poor"))%>%
  filter(year==1960)%>%
  ggplot(aes(x=gdp_per_capita, y=continent))+
  geom_boxplot(aes(fill=group),
               outlier.color = NA)+
  geom_point(position = jitter_pos)+
  geom_text_repel(aes(label=country),
                  position = jitter_pos)+
  scale_x_continuous(trans="log10")

