library(dplyr)
library(rstan)

ii <- "Kagawa"

japan_pop_raw <- read.csv("../data/japan_pop.csv") %>%
  mutate(
    prefecture=gsub("-.*", "", prefecture),
    pop=as.numeric(gsub(",", "", pop))
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
  arrange(prefecture, year, week)

japan_birth <- read.csv("../data_processed/japan_birth_weekly.csv") %>%
  merge(japan_pop_raw) %>%
  arrange(prefecture, time)

cases <- filter(japan_rsv, prefecture==ii, year<2020)$cases
birth <- filter(japan_birth, prefecture==ii, time<2020)$birth

rhomean <- sum(cases)/sum(birth)

model <- stan_model("../stanmodel/tsir_constant.stan")

standata <- list(
  N=364,
  week=filter(japan_rsv, prefecture==ii, year<2020)$week,
  cases=cases,
  birth=birth,
  pop=filter(japan_pop_raw, prefecture==ii)$pop,
  alpha=0.97,
  rhomean=rhomean
)

stanfit_tsir_kagawa_constant <- sampling(model,
                                         data = standata,
                                         seed=101)

save("stanfit_tsir_kagawa_constant", file="stanfit_tsir_kagawa_constant.rda")

ss <- summary(stanfit_tsir_kagawa_constant)

plot(cases)
lines(ss$summary[grepl("I\\[", rownames(ss$summary)),6]*ss$summary[grepl("rho", rownames(ss$summary)),6])
