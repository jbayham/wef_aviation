
library(pacman)
p_load(tidyverse,readxl,lubridate,janitor,MMWRweek,patchwork)

raw_dat <- read_excel("build/inputs/2007-2021 PLs by Erin.xlsx",sheet = "National") %>%
  clean_names()

soi_dat <- read_csv("build/inputs/soi_data.csv") %>%
  mutate(across(c(start,end),mdy))

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


p1 <- plot_dat %>%
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

################################
soi_expanded <- soi_dat %>%
  group_split(id) %>%
  map_dfr(function(df){
    drange = seq.Date(df$start,df$end,by=1)
    
    df %>%
      #select(-c(start,end)) %>%
      slice(rep(1, each = length(drange))) %>%
      mutate(drange)
  })

ggplot(soi_expanded,aes(x=drange,y=reorder(id,start),fill=factor(owner))) +
  geom_tile()

soi_byday <- soi_expanded %>%
  count(drange) %>%
  right_join(enframe(seq.Date(as_date("2022-01-01"),as_date("2022-12-31"),by=1),name=NULL,value="drange")) %>%
  mutate(n=ifelse(is.na(n),0,n))

p2 <- ggplot(soi_byday,aes(x=drange,y=n)) +
  geom_col(width = 1) +
  scale_x_date(date_labels = "%b") +
  theme_bw(base_size=14) +
  labs(x=NULL,y="# LAT/VLAT")

p1/p2 + plot_layout(heights = c(2,1))

ggsave("figures/ia_ea_fires.png")
