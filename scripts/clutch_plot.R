# plot SC clutch condition time series

library(tidyverse)
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

# load
dat <- read.csv("./data/SC_clutch_code_TS.csv")

# remove immature
dat <- dat %>%
  select(-SUM_POPFEMALE_CLUTCH0)

names(dat)[1] <- "year"

dat$total <- apply(dat[,2:7], 1, sum)
dat$full <- apply(dat[,6:7], 1, sum)

dat$prop <- dat$full / dat$total

ggplot(dat, aes(year, prop)) +
  geom_point() +
  geom_line() +
  ylab("Proportion full or 3/4 full")

ggsave("./figs/clutch_TS.png", width = 6, height = 4, units = 'in')
