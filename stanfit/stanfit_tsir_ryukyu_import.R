library(dplyr)
library(rstan)

ii <- "Ryukyu"

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
  arrange(island, year, week)

japan_birth <- read.csv("../data_processed/japan_birth_weekly.csv") %>%
  merge(japan_pop_raw) %>%
  group_by(island, time) %>%
  summarize(
    birth=sum(birth)
  )

cases <- filter(japan_rsv, island==ii, year<2020)$cases
birth <- filter(japan_birth, island==ii, time<2020)$birth

rhomean <- sum(cases)/sum(birth)

model <- stan_model("../stanmodel/tsir_import.stan")

standata <- list(
  N=364,
  week=filter(japan_rsv, island==ii, year<2020)$week,
  cases=cases,
  birth=birth,
  pop=filter(japan_pop, island==ii)$pop,
  alpha=0.97,
  rhomean=rhomean
)

stanfit_tsir_ryukyu_import <- sampling(model,
                                       data = standata,
                                       seed=101,
                                       iter=4000,
                                       control=list(max_treedepth=15,
                                                    adapt_delta=0.99))

save("stanfit_tsir_ryukyu_import", file="stanfit_tsir_ryukyu_import.rda")
