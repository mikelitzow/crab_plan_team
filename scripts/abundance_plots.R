# plot abundance time series

library(tidyverse)
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

maleSC <- read.csv("./data/male_SC_abundance.csv")

head(maleSC)

# mature male SC
ggplot(maleSC, aes(year, Mature_male)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = Mature_male - Mature_CI, ymax = Mature_male + Mature_CI), alpha = 0.2, fill = cb[3]) +
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(breaks=c(50,100,200, 400, 600, 800), minor_breaks = NULL) +
  coord_trans(y = "pseudo_log") +
  scale_x_continuous(breaks = seq(1980, 2020, 5), minor_breaks = NULL) +
  labs(title = "Abundance and 95% CI", y = "Millions")

ggsave("./figs/mature male SC abundance TS.png", width = 6, height = 4, units = 'in')

# preferred male SC
ggplot(maleSC, aes(year, Preferred_male)) +
  geom_line() +
  geom_point() +
  # geom_ribbon(aes(ymin = Mature_male - Mature_CI, ymax = Mature_male + Mature_CI), alpha = 0.2, fill = cb[3]) +
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(breaks=c(30, 50, 100, 200, 400), minor_breaks = NULL) +
  coord_trans(y = "pseudo_log") +
  scale_x_continuous(breaks = seq(1980, 2020, 5), minor_breaks = NULL) +
  labs( y = "Millions")

ggsave("./figs/preferred male SC abundance TS.png", width = 6, height = 4, units = 'in')

femaleSC <- read.csv("./data/female_SC_abundance.csv")

head(femaleSC)

# mature female SC
ggplot(femaleSC, aes(year, Mature_female)) +
  geom_line() +
  geom_point() +
  # geom_ribbon(aes(ymin = Mature_female - MatureCI, ymax = Mature_female + MatureCI), alpha = 0.2, fill = cb[3]) +
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(breaks=c(100, 200, 500, 1000, 2000), minor_breaks = NULL) +
  coord_trans(y = "pseudo_log") +
  scale_x_continuous(breaks = seq(1980, 2020, 5), minor_breaks = NULL) +
  labs( y = "Millions")

ggsave("./figs/mature female SC abundance TS.png", width = 6, height = 4, units = 'in')
