library(dplyr)
source("../R/fit_gradient.R")

japan_pop <- read.csv("../data/japan_pop.csv") %>%
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
  )

japan_birth <- read.csv("../data_processed/japan_birth_weekly.csv")

reslist <- vector('list', 47)

for (i in 1:47) {
  print(i)
  prefect <- japan_pop$prefecture[i]
  
  ff <- filter(japan_rsv, prefecture==prefect, year < 2020)
  
  cases <- ff$cases+1
  
  birth <- filter(japan_birth, prefecture==prefect, floor(time) < 2020)$birth
  
  if (prefect=="Okinawa") {
    cutoff <- 2016
  } else {
    cutoff <- 2016.4
  }
  
  out <- fit_gradient(cases=cases,
                      birth=birth,
                      week=ff$week,
                      N=japan_pop$pop[i],
                      cutoff=cutoff)
  
  plot(out$deout$I/out$rho, type="l", col=2)
  lines(cases)
  # plot(out$llvec2, log="y")
  
  out$prefecture <- prefect
  
  reslist[[i]] <- out
}

fit_gradient_all <- reslist

save("fit_gradient_all", file="fit_gradient_all.rda")
