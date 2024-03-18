library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family="Times"))
library(egg)
library(gridExtra)

japan_pop_raw <- read.csv("../data/japan_pop.csv") %>%
  mutate(
    prefecture=gsub("-.*", "", prefecture),
    pop=as.numeric(gsub(",", "", pop))
  ) %>%
  arrange(prefecture)

japan_rsv <- read.csv("../data/japan_rsv.csv") %>%
  mutate(
    week=ifelse(week==53, 52, week)
  ) %>%
  group_by(prefecture, year, week) %>%
  summarize(
    cases=round(mean(cases)),
    long=long[1],
    lat=lat[1]
  ) %>%
  merge(japan_pop_raw)

japan_rsv_max <- japan_rsv %>%
  filter(year < 2020) %>%
  group_by(prefecture) %>%
  summarize(
    max=max(cases)
  )

japan_rsv2 <- japan_rsv %>%
  merge(japan_rsv_max) %>%
  mutate(
    relcases=pmin(cases/max, 2)
  ) %>%
  merge(japan_pop_raw)

japan_humidity <- read.csv("../data_processed/data_processed_humidity.csv") %>%
  rename(prefecture=key) %>%
  merge(japan_pop_raw)

japan_humidity_summ <- lapply(split(japan_humidity, japan_humidity$prefecture), function(x) {
  island <- x$island[1]
  
  x <- x %>%
    mutate(
      trueyear=year
    )
  
  if (island!="Ryukyu") {
    x <- x %>%
      mutate(
        week=week-26,
        year=ifelse(week <= 0, year-1, year),
        week=week%%52,
        week=ifelse(week==0, 52, week)
      )
  }
  
  out <- x %>%
    filter(year > 2012, year < 2020) %>%
    group_by(prefecture, island, year) %>%
    summarize(
      mean=mean(humidity)
    )
  
  out
}) %>%
  bind_rows

g1 <- ggplot(japan_rsv2) +
  geom_raster(aes(year+(week-1)/52, reorder(prefecture, lat), fill=relcases)) +
  geom_vline(xintercept = 2013:2024, lty=2, col="white") +
  scale_x_continuous(breaks=2013:2024, expand=c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_viridis_c("Relative cases", 
                       breaks=c(0, 1, 2),
                       labels=c("0", "1", ">2")) +
  theme(
    axis.title = element_blank(),
    legend.position = "bottom"
  )

japan_rsv_ac <- lapply(split(japan_rsv, japan_rsv$prefecture), function(x) {
  x <- x %>% 
    arrange(year, week) %>%
    mutate(time=1:n()) %>%
    filter(week/52+year < 2016+26/52)
  
  lfit <- loess(log(cases+1)~time, data=x)
  
  resid <- log(x$cases+1) - predict(lfit)
  
  ac <- sapply(1:(length(resid)-52), function(z) {
    cor(resid[z:(z+51)], resid[(z+1):(z+52)])
  })
  
  sd <- sapply(1:(length(resid)-52), function(z) {
    sd(resid[(z+1):(z+52)])
  })
  
  data.frame(
    prefecture=x$prefecture[1],
    island=x$island[1],
    ac=ac,
    sd=sd,
    time=x$time[-c(1:52)],
    week=x$week[-c(1:52)],
    year=x$year[-c(1:52)],
    long=x$long[1],
    lat=x$lat[1]
  )  
}) %>%
  bind_rows %>%
  mutate(
    island=factor(island, levels=c("Hokkaido", "Honshu", "Shikoku", "Kyushu", "Ryukyu"))
  )

japan_rsv_ac_summ <- lapply(split(japan_rsv_ac, japan_rsv_ac$prefecture), function(x) {
  data.frame(
    prefecture=x$prefecture[1],
    cor_ac=cor(x$ac, x$time, method="kendall"),
    cor_sd=cor(x$sd, x$time, method="kendall"),
    long=x$long[1],
    lat=x$lat[1]
  )
}) %>%
  bind_rows

g2 <- ggplot(japan_rsv_ac_summ) +
  geom_vline(xintercept=0, lty=2) +
  # geom_errorbarh(aes(xmin=lwr, xmax=upr, y=reorder(prefecture, lat)), height=0) +
  geom_point(aes(cor_ac, reorder(prefecture, lat), shape="AR(1)", fill=cor_ac), size=3)  +
  geom_point(aes(cor_sd, reorder(prefecture, lat), shape="s.d.", fill=cor_sd), size=3)  +
  scale_x_continuous("Early warning signals", limits=c(-1, 1), expand=c(0.01, 0)) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', midpoint = 0, guide = FALSE) +
  scale_shape_manual(values=c(21, 24)) +
  theme(
    # panel.grid = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

japan_rsv3 <- japan_rsv2 %>%
  group_by(island, year, week) %>%
  summarize(
    total=sum(cases)
  ) %>%
  group_by(island) %>%
  mutate(
    island=factor(island, levels=c("Hokkaido", "Honshu", "Shikoku", "Kyushu", "Ryukyu"))
  )

g3 <- ggplot(japan_rsv3) +
  geom_vline(xintercept = 2013:2024, lty=3, col="gray80", lwd=0.5) +
  # annotate("rect", xmin=2016+0.4, xmax=2017+0.4, min=-Inf, ymax=Inf, alpha=0.2) +
  # geom_line(data=data_processed_travel3, aes(year+(month-1)/12, travel/pop*10), col="#E02938", lty=1) +
  geom_line(aes(year+(week-1)/52, total), col="#224B95") +
  scale_x_continuous("Year", limits=c(2013, 2024.2), breaks=c(2013, 2015, 2017, 2019, 2021, 2023), expand=c(0, 0)) +
  scale_y_continuous("Cases", expand=c(0, 0)) +
  facet_wrap(~island, nrow=1, scale="free") +
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(size=1),
    axis.title.x = element_blank()
  )

japan_rsv_summ <- lapply(split(japan_rsv2, japan_rsv2$prefecture), function(x) {
  island <- x$island[1]
  
  x <- x %>%
    mutate(
      trueyear=year
    )
  
  if (island!="Ryukyu") {
    x <- x %>%
      mutate(
        week=week-26,
        year=ifelse(week <= 0, year-1, year),
        week=week%%52,
        week=ifelse(week==0, 52, week)
      )
  }
  
  out <- x %>%
    filter(year > 2012, year < 2020) %>%
    group_by(prefecture, island, year) %>%
    mutate(
      prop=(cases)/sum(cases)
    ) %>%
    summarize(
      cog=sum(week*cases)/sum(cases),
      intensity=1/(-sum(prop*log(prop), na.rm=TRUE)),
      size=sum(cases),
      trough=min(cases[trueyear<2020])
    ) %>%
    mutate(
      cog=ifelse(island!="Ryukyu", cog+26, cog)
    )
}) %>%
  bind_rows %>%
  merge(
    japan_pop_raw
  ) %>%
  mutate(
    season=paste0(year,"-",year+1),
    island=factor(island, levels=c("Hokkaido", "Honshu", "Shikoku", "Kyushu", "Ryukyu"))
  )

g4 <- ggplot(japan_rsv_summ %>% filter(prefecture != "Okinawa")) +
  geom_boxplot(aes(season, cog), show.legend = FALSE, fill="#CCECF8") +
  scale_x_discrete("Season") +
  scale_y_continuous("Center of gravity") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.text.x = element_text(hjust=1, angle=45),
    axis.title.x = element_blank()
  )

g5 <- ggplot(japan_rsv_summ) +
  geom_boxplot(aes(season, trough/pop), show.legend = FALSE, fill="#CCECF8") +
  scale_x_discrete("Season") +
  scale_y_continuous("Epidemic trough") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.text.x = element_text(hjust=1, angle=45),
    axis.title.x = element_blank()
  )

data_processed_travel <- read.csv("../data_processed/data_processed_travel.csv")

data_processed_travel2 <- data_processed_travel %>%
  mutate(
    travel=total*prop
  ) %>%
  group_by(year, prefecture) %>%
  summarize(
    travel=sum(travel)
  )

japan_rsv_summ_comb <- japan_rsv_summ %>%
  merge(data_processed_travel2) %>%
  merge(japan_humidity_summ)

g6 <- ggplot(japan_rsv_summ_comb) +
  geom_point(aes(pop, travel/pop, size=pop, fill=log10(travel/pop)), shape=21) +
  scale_x_log10("Population size") +
  scale_y_log10("Annual visitors per capita") +
  scale_size_area("Population size\n(millions)",
                  breaks=c(1:5*2*1e6),
                  labels=c(1:5*2),
                  max_size=4) +
  scale_fill_viridis_c("Visitors\nper capita",
                       breaks=log10(c(0.03, 0.1, 0.3, 1, 3)),
                       labels=c(0.03, 0.1, 0.3, 1, 3),
                       guide=FALSE) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    legend.position = "bottom"
  )

g7 <- ggplot(japan_rsv_summ_comb) +
  geom_point(aes(mean, cog, size=pop, fill=log10(travel/pop)), shape=21) +
  scale_x_continuous("Specific humidity (kg/kg)") +
  scale_y_continuous("Center of gravity") +
  scale_size_area("Population size\n(millions)",
                  breaks=c(1:5*2*1e6),
                  labels=c(1:5*2),
                  guide=FALSE,
                  max_size=4) +
  scale_fill_viridis_c("Annual visitors\nper capita",
                       breaks=log10(c(0.03, 0.1, 0.3, 1, 3)),
                       labels=c(0.03, 0.1, 0.3, 1, 3)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    legend.position = "bottom"
  )


g1a <- ggarrange(g1, g2, labels=c("A", "B"), nrow=1, widths=c(3, 1))
g3a <- ggarrange(g3, labels=c("C"))
# g4a <- ggarrange(g4, g5, g6, g7, labels=c("D", "E", "F", "G"), nrow=1)

gcomb1 <- arrangeGrob(g1a, g3a, nrow=2, heights=c(3.5, 1))

ggsave("figure1.pdf", gcomb1, width=14, height=8)
ggsave("figure1.png", gcomb1, width=14, height=8)
