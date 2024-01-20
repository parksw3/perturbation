library(tidyr)
library(dplyr)

japan_birth <- read.csv("../data/japan_birth.csv")

japan_birth_clean <- japan_birth %>%
  gather(key, value, -prefecture) %>%
  mutate(
    prefecture=gsub(".* ", "", prefecture),
    key=as.numeric(gsub("X", "", key))
  ) %>%
  rename(
    year=key,
    birth=value
  )

japan_rsv <- read.csv("../data/japan_rsv.csv")  

time <- 2013+0:571/52

japan_birth_weekly <- lapply(split(japan_birth_clean, japan_birth_clean$prefecture), function(x) {
  approx_birth <- approx(x=x$year+1, y=x$birth/52, xout=time)
  b <- approx_birth$y
  
  b[is.na(b)] <- tail(b[!is.na(b)],1)
  
  data.frame(
    prefecture=x$prefecture[1],
    time=time,
    birth=b
  )  
}) %>%
  bind_rows

write.csv(japan_birth_weekly, file="japan_birth_weekly.csv")
