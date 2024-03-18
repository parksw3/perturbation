library(dplyr)
library(rstan)
library(ggplot2); theme_set(theme_bw(base_family="Times"))
library(egg)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

japan_pop_raw <- read.csv("../data/japan_pop.csv") %>%
  mutate(
    prefecture=gsub("-.*", "", prefecture),
    pop=as.numeric(gsub(",", "", pop))
  )

japan_pop <- japan_pop_raw %>%
  group_by(island) %>%
  summarize(
    pop=sum(pop)
  )

japan_rsv <- read.csv("../data/japan_rsv.csv") %>%
  mutate(
    week=ifelse(week==53, 52, week)
  ) %>%
  group_by(prefecture, year, week) %>%
  summarize(
    cases=round(mean(cases))
  ) %>%
  merge(japan_pop_raw) %>%
  group_by(island, year, week) %>%
  summarize(
    cases=sum(cases)
  ) %>%
  arrange(island, year, week) %>%
  mutate(
    island=factor(island, levels=c("Hokkaido", "Honshu", "Shikoku", "Kyushu", "Ryukyu"))
  )

japan_humidity <- read.csv("../data_processed/data_processed_humidity.csv") %>%
  rename(prefecture=key) %>%
  merge(japan_pop_raw)

japan_humidity2 <- japan_humidity %>%
  filter(year < 2020) %>%
  group_by(island, week) %>%
  summarize(
    humidity=mean(humidity)
  )

island <- c("hokkaido", "honshu", "shikoku", "kyushu", "ryukyu")
type <- c("constant", "import")

allfits <- expand.grid(island, type)

fitlist <- vector('list', nrow(allfits))
betalist <- vector('list', nrow(allfits))

for (i in 1:nrow(allfits)) {
  print(i)
  aa <- allfits[i,]
  
  file <- paste0("../stanfit/stanfit_tsir_", aa[[1]], "_", aa[[2]], ".rda")
  
  load(file)
  
  ff <- get(paste0("stanfit_tsir_", aa[[1]], "_", aa[[2]]))
  
  ss <- summary(ff)
  
  fitlist[[i]] <- data.frame(
    week=rep(1:52, 7),
    year=rep(2013:2019, each=52),
    est=ss$summary[grepl("I\\[", rownames(ss$summary)),6]*ss$summary[grepl("rho", rownames(ss$summary)),6],
    lwr=ss$summary[grepl("I\\[", rownames(ss$summary)),4]*ss$summary[grepl("rho", rownames(ss$summary)),4],
    upr=ss$summary[grepl("I\\[", rownames(ss$summary)),8]*ss$summary[grepl("rho", rownames(ss$summary)),8],
    island=firstup(as.character(aa[[1]])),
    type=aa[[2]]
  )
  
  betalist[[i]] <- data.frame(
    week=1:52,
    est=ss$summary[grepl("beta\\[", rownames(ss$summary)),6],
    lwr=ss$summary[grepl("beta\\[", rownames(ss$summary)),4],
    upr=ss$summary[grepl("beta\\[", rownames(ss$summary)),8],
    island=firstup(as.character(aa[[1]])),
    type=aa[[2]]
  )  
}

fitdata <- fitlist %>%
  bind_rows %>%
  merge(japan_rsv) %>%
  mutate(
    island=factor(island, levels=c("Hokkaido", "Honshu", "Shikoku", "Kyushu", "Ryukyu")),
    type=factor(type,
                levels=c("import", 'constant'),
                labels=c("With imported cases", "Without imported cases"))
  )

betadata_island <- betalist %>%
  bind_rows %>%
  mutate(
    island=factor(island, levels=c("Hokkaido", "Honshu", "Shikoku", "Kyushu", "Ryukyu")),
    type=factor(type,
                levels=c("import", 'constant'),
                labels=c("With imported cases", "Without imported cases"))
  )

g1 <- ggplot(fitdata %>% filter(type=="With imported cases")) +
  geom_vline(xintercept = 2013:2020, lty=3, col="gray50", lwd=0.5) +
  # geom_ribbon(data=fitdata %>% filter(type=="Without imported cases"),
  #             aes(year+(week-1)/52, ymin=lwr, ymax=upr), fill="#40B0A6", alpha=0.5) +
  geom_ribbon(aes(year+(week-1)/52, ymin=lwr, ymax=upr), fill="#E02938", alpha=0.5) +
  geom_point(aes(year+(week-1)/52, cases), shape=1) +
  # geom_line(data=fitdata %>% filter(type=="Without imported cases"),
  #           aes(year+(week-1)/52, est), col="#40B0A6", lwd=0.7)  +
  geom_line(aes(year+(week-1)/52, est), col="#E02938", lwd=0.7)  +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Cases") +
  facet_wrap(~island, scale="free_y", nrow=1) +
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank()
  )

fitdata_r2 <- fitdata %>%
  group_by(island, type) %>%
  summarize(
    r2=signif(cor(log(cases+1), log(est+1)),2)
  )

g2 <- ggplot(fitdata) +
  geom_point(aes(cases+1, est+1, col=type, shape=type)) +
  geom_text(data=fitdata_r2 %>% filter(type=="With imported cases"), x=-Inf, y=Inf, aes(label=r2), hjust=-0.1, vjust=1.3, col="#E02938") +
  geom_text(data=fitdata_r2 %>% filter(type=="Without imported cases"), x=-Inf, y=Inf, aes(label=r2), hjust=-0.1, vjust=2.6, col="#40B0A6") +
  facet_wrap(~island, scale="free", nrow=1) +
  scale_x_log10("Observed cases+1") +
  scale_y_log10("Predicted cases+1") +
  scale_color_manual(values=c("#E02938", "#40B0A6")) +
  scale_shape_manual(values=1:2) +
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(size=1),
    legend.title = element_blank(),
    legend.position = "right"
  )

g3 <- ggplot(betadata_island %>% filter(type=="With imported cases")) +
  geom_errorbar(aes(week, ymin=lwr, ymax=upr), width=0) +
  geom_point(aes(week, est)) +
  scale_x_continuous("Week") +
  scale_y_continuous("Transmission rate", limits=c(7, 38)) +
  facet_wrap(~island, scale="free_y", nrow=1) +
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1)
  )

betadata_island_humidity <- betadata_island %>%
  merge(japan_humidity2) %>%
  filter(type=="With imported cases")

g4 <- ggplot(betadata_island_humidity) +
  geom_point(aes(humidity, est)) +
  geom_smooth(aes(humidity, est), method="loess", col="#E02938", fill="#E02938") +
  scale_x_continuous("Specific humidity (kg/kg)") +
  scale_y_continuous("Transmission rate", limits=c(12, 32)) +
  facet_wrap(~island, scale="free_y", nrow=1) +
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    legend.position = "bottom"
  )

gcomb <- ggarrange(g1, g2, g3, g4, ncol=1, labels=c("A", "B", "C", "D"),
                   draw=FALSE)

ggsave("figure_stanfit.pdf", gcomb, width=12, height=8)
save("betadata_island", file="betadata_island.rda")
