rm=(list=ls())
library(tidyverse)
df=read_csv("college.csv")

df_isna=is.na(df)

head(df_isna)
tail(df_isna)

colSums(df_isna)
rowSums(df_isna)

df = filter(df,
       city =="Cincinnati",
       highest_degree == "Graduate"
       )

select(df, name, sat_avg)

df2 = filter(df, control == "Public",
             city =="Cincinnati",
             )
select(df3, name, sat_avg)

df3 = select(df2, name,sat_avg)

select(filter(df,
              control == "Public",
              city =="Cincinnati"),
       name,
       sat_avg)
df %>% 
  filter(control=="Public",city=="New York")%>%
  select(name, sat_avg)  %>%
  filter(sat_avg>1000) %>%
  select(name)

df %>%
  filter(control =="Private",city=="Chicago")%>%
  filter(admission_rate<.4)%>%
  select(name)
mutate(df, total_tuition = tuition*undergrads)
names(df)

df %>%
  filter(control == "Public",
    city == "Los Angeles") %>%
  mutate(total_tuition = tuition*undergrads)%>%
  filter(total_tuition>10000000)%>%
  select(name)