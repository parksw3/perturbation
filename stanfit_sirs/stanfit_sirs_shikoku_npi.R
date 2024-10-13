library(dplyr)
library(rstan)
source("../script/script_data.R")

ii <- "Shikoku"

cases <- filter(japan_rsv_island, island==ii)$cases
birth <- filter(japan_birth_island, island==ii)$birth
pop <- filter(japan_pop_island, island==ii)$pop

model <- stan_model("../stanmodel/sirs_npi_tweak.stan")

standata <- list(
  N=572,
  Nnonpi=364,
  week=filter(japan_rsv_island, island==ii)$week,
  cases=cases,
  mu=1/80/52,
  pop=pop,
  gamma=1
)

stanfit_sirs_shikoku_npi <- sampling(model,
                                    data = standata,
                                    seed=104,
                                    chain=4,
                                    iter=4000,
                                    cores=4,
                                    control=list(
                                      adapt_delta=0.99,
                                      max_treedepth=15
                                    ))

check_hmc_diagnostics(stanfit_sirs_shikoku_npi)
get_num_divergent(stanfit_sirs_shikoku_npi)
get_num_max_treedepth(stanfit_sirs_shikoku_npi)
get_low_bfmi_chains(stanfit_sirs_shikoku_npi)

save("stanfit_sirs_shikoku_npi", file="stanfit_sirs_shikoku_npi.rda")

ss <- summary(stanfit_sirs_shikoku_npi)

max(ss$summary[which(!is.na(ss$summary[,10])),10]) # 1.013
min(ss$summary[which(!is.na(ss$summary[,10])),9]) # 305

plot(cases)
lines(ss$summary[grepl("C\\[", rownames(ss$summary)),6])
lines(ss$summary[grepl("C\\[", rownames(ss$summary)),4])
lines(ss$summary[grepl("C\\[", rownames(ss$summary)),8])

plot(ss$summary[grepl("npieff\\[", rownames(ss$summary)),6], type="l")
lines(ss$summary[grepl("npieff\\[", rownames(ss$summary)),4])
lines(ss$summary[grepl("npieff\\[", rownames(ss$summary)),8])

plot(ss$summary[grepl("beta\\[", rownames(ss$summary)),6], type="l")

plot(ss$summary[grepl("S\\[", rownames(ss$summary)),6]/pop)
