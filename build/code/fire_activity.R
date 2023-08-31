
library(pacman)
p_load(tidyverse,readxl,lubridate,janitor,MMWRweek)

raw_dat <- read_excel("build/inputs/2007-2021 PLs by Erin.xlsx",sheet = "National") %>%
  clean_names()

sa_ea <- map_dfr(c("SA","EA"),
                 function(x){
                   temp <- read_excel("build/inputs/2007-2021 PLs by Erin.xlsx",sheet = x) %>%
                     clean_names() %>%
                     select(date,new_fire,uncont_lf) %>%
                     mutate(new_fire=as.numeric(new_fire),
                            gacc=x)
                 }) %>%
  group_by(date) %>%
  summarize(across(c(new_fire,uncont_lf),~sum(.,na.rm = T)))


plot_dat <- raw_dat %>% 
  left_join(sa_ea) %>%
  mutate(across(c(new_fire,uncont_lf),~ifelse(is.na(.),0,.)),
         ia_new_fires = ia_new_fires - new_fire,
         uncontained_lf = uncontained_lf - uncont_lf,
         year=year(date),
         doy=as.numeric(format(date,'%j')),
         epiweek=epiweek(date))

plot_dat %>%
  group_by(year,epiweek) %>%
  summarize(ia_new_fires=mean(ia_new_fires,na.rm=T)) %>%
  ggplot(aes(x=epiweek,y=ia_new_fires,color=factor(year))) +
  geom_line()

plot_dat %>%
  group_by(year,epiweek) %>%
  summarize(uncontained_lf=mean(uncontained_lf,na.rm=T)) %>%
  ggplot(aes(x=epiweek,y=uncontained_lf,color=factor(year))) +
  geom_line()


plot_dat %>%
  group_by(epiweek) %>%
  summarize(sd=sd(uncontained_lf,na.rm=T),
            uncontained_lf=mean(uncontained_lf,na.rm=T)) %>%
  mutate(low=uncontained_lf-1.96*sd,
         high=uncontained_lf+1.96*sd,
         low=ifelse(low<0,0,low)) %>%
  ggplot(aes(x=epiweek,y=uncontained_lf,ymin=low,ymax=high)) +
  geom_pointrange()

plot_dat %>%
  group_by(epiweek) %>%
  summarize(sd=sd(ia_new_fires,na.rm=T),
            ia_new_fires=mean(ia_new_fires,na.rm=T)) %>%
  mutate(low=ia_new_fires-1.96*sd,
         high=ia_new_fires+1.96*sd,
         low=ifelse(low<0,0,low)) %>%
  ggplot(aes(x=epiweek,y=ia_new_fires,ymin=low,ymax=high)) +
  geom_pointrange()

plot_dat %>%
  select(epiweek,year,`Initial Attack Fires`=ia_new_fires,`Extended Attack Fires`=uncontained_lf) %>%
  pivot_longer(-c(epiweek,year)) %>%
  group_by(epiweek,name) %>%
  summarize(mean=median(value,na.rm=T),
            low=quantile(value,.25,na.rm = T),
            high=quantile(value,.75,na.rm=T)) %>%
  mutate(year=2020,
         measure_date=MMWRweek2Date(year,epiweek)) %>%
  ggplot(aes(x=measure_date,y=mean,ymin=low,ymax=high)) +
  geom_pointrange() +
  scale_x_date(date_labels = "%b") +
  labs(x=NULL,y="Number of Fires") +
  theme_bw(base_size = 14) +
  facet_wrap(~name,scales = "free_y",ncol=1)

ggsave("figures/ia_ea_fires.png")
