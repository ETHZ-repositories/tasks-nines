
# load packages
library(tidyverse)

# load data
intro_survey <- read_csv("intro-survey.csv")

# create tidy format
intro_survey_tidy <- intro_survey %>%
  select(-expect,-general)%>%
  pivot_longer(c(-cgroup,-id),names_to="variable", values_to="grade")

# count grades per variable
intro_survey_tidy%>%
  group_by(variable,grade)%>%
  summarize(count=n())%>%
  ggplot(aes(x=grade, y=count, fill=variable))+
  geom_bar(stat="identity")+
  facet_grid(~variable)+
  theme_minimal()+
  theme(
    legend.position = "none"
  )

# mean grade per variable and group
intro_survey_tidy%>%
  group_by(variable,cgroup)%>%
  summarize(
    grade_mean = mean(grade, na.rm=T)
  )%>%
  ggplot(aes(x=cgroup, y=grade_mean, fill=cgroup))+
  geom_bar(stat="identity")+
  facet_grid(~variable)+
  theme_minimal()

# mean grade per variable and group
intro_survey_tidy%>%
  group_by(cgroup)%>%
  summarize(
    grade_mean = mean(grade, na.rm=T)
  )%>%
  ungroup()%>%
  mutate(
    cgroup = fct_reorder(cgroup, grade_mean)
  )%>%
  ggplot(aes(x=cgroup, y=grade_mean, fill=cgroup))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(
    legend.position = "none"
  )
