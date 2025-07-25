library(dplyr)
library(rstan)
source("../script/script_data.R")

ii <- "Shikoku"

cases <- filter(japan_rsv_island, island==ii)$cases
pop <- filter(japan_pop_island, island==ii)$pop

model <- stan_model("../stanmodel/sirs_npi_tweak2.stan")

standata <- list(
  N=572,
  Nnonpi=364,
  week=filter(japan_rsv_island, island==ii)$week,
  cases=cases,
  mu=1/80/52,
  pop=pop,
  gamma=1,
  type=(filter(japan_rsv_island, island==ii)$year + filter(japan_rsv_island, island==ii)$week/52 > 2016.5)+1
)

stanfit_sirs_shikoku_npi2 <- sampling(model,
                                    data = standata,
                                    seed=102,
                                    chain=4,
                                    cores=4,
                                    iter=4000,
                                    control=list(max_treedepth=15,
                                                 adapt_delta=0.9))

check_hmc_diagnostics(stanfit_sirs_shikoku_npi2)
get_num_divergent(stanfit_sirs_shikoku_npi2)
get_num_max_treedepth(stanfit_sirs_shikoku_npi2)
get_low_bfmi_chains(stanfit_sirs_shikoku_npi2)

save("stanfit_sirs_shikoku_npi2", file="stanfit_sirs_shikoku_npi2.rda")

ss <- summary(stanfit_sirs_shikoku_npi2)

max(ss$summary[which(!is.na(ss$summary[,10])),10]) ## 1.004
min(ss$summary[which(!is.na(ss$summary[,10])),9]) ## 744

plot(cases)
lines(ss$summary[grepl("C\\[", rownames(ss$summary)),6])
lines(ss$summary[grepl("C\\[", rownames(ss$summary)),4])
lines(ss$summary[grepl("C\\[", rownames(ss$summary)),8])

plot(ss$summary[grepl("npieff\\[", rownames(ss$summary)),6], type="l")
lines(ss$summary[grepl("npieff\\[", rownames(ss$summary)),4])
lines(ss$summary[grepl("npieff\\[", rownames(ss$summary)),8])

plot(ss$summary[grepl("beta\\[", rownames(ss$summary)),6], type="l")
lines(ss$summary[grepl("beta2\\[", rownames(ss$summary)),6], type="l", col=2)

plot(ss$summary[grepl("S\\[", rownames(ss$summary)),6]/pop)
