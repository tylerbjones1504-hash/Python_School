rm(list = ls())

library(tidyverse)
df=read.csv("college.csv")
library(tidyverse)
filter()
select()

df %>%
  filter(control=="Public")%>%
  mutate(total_tuition=undergrads*tuition)%>%
  group_by(state)%>%
  summarize(avg_total_tuition=mean(total_tuition))%>%
  filter(avg_total_tuition>30000000)%>%
  select(state)

names(df)
head(df)

state.abb
state.name
state_info=as_tibble(data.frame(state.abb, state.name))
df
left_join(df,state_info)

df2=right_join(df,
               state_info,
               by=c("state"="state.abb"))

df_noOH=df%>%
  filter(state!="OH")

state_info_noIN= state_info%>%
  filter(state.abb!="IN")

df_noOH
state_info_noIN

df2=left_join(df_noOH,
              state_info_noIN,
              by=c("state"="state.abb"))

df2%>%
  filter(state%in%c("IN","DC"))%>%
  select(state.name)

df2=right_join(df_noOH,
               state_info_noIN,
               by=c("state"="state.abb"))

df2%>%
  select(state.name)%>%
  mutate(isna=is.na(state.name))%>%
  summarize(count_na=sum(isna))

df2=inner_join(df_noOH,
               state_info_noIN,
               by=c("state"="state.abb"))

df %>%
  filter(control=="Public")%>%
  mutate(total_tuition=undergrads*tuition)%>%
  group_by(state)%>%
  summarize(avg_total_tuition=mean(total_tuition))%>%
  filter(avg_total_tuition>100000000)%>%
  select(state)%>%
  left_join(state_info,
            by=c("state"="state.abb")) %>%
  select(state.name)

df2=df %>%
  filter(control=="Private")%>%
  group_by(state)%>%
  summarize(avg_tuition=mean(tuition))%>%
  filter(avg_tuition>35000)
  
df3=left_join(df2,state_info,by = c("state"="state.abb"))

df2=filter(df1)

df %>%
  filter(control=="Public")%>%
  group_by(state)%>%
  summarise(count=n())%>%
  filter(count>20)%>%
  left_join(state_info,by = c ("state"="state.abb"))%>%
  select(state.name)


df2=left_join(df,state_info, c("state"="state.abb") )  






