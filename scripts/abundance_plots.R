# plot abundance time series

library(tidyverse)
cb <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

sc <- read.csv("./data/snow_crab_abundance.csv")

head(sc)

# remove CVs
drop <- grep("CV", names(sc))
sc <- sc[,-drop]

# and convert to millions
sc[,2:ncol(sc)] <- sc[,2:ncol(sc)]/1e6

# clean up names
names(sc)[1] <- "year"

xtra <- data.frame(matrix(nrow = 1, ncol = ncol(sc)))
colnames(xtra) <- names(sc)
xtra["year"] <- 2020

sc <- rbind(sc, xtra) %>%
  arrange(year)


# add NBS
nbs <- read.csv("./data/NBS_SC_abundance.csv")

# remove CVs
drop <- grep("CV", names(nbs))
nbs <- nbs[,-drop]

# and convert to millions
nbs[,2:ncol(nbs)] <- nbs[,2:ncol(nbs)]/1e6

# clean up names
names(nbs)[1] <- "year"
# and jitter
nbs$year <- nbs$year + 0.2

# combine for mature male
d1 <- sc %>%
  select(year, NUM_MALE_GE95, CI_NUM_MALE_GE95) %>%
  rename(est = NUM_MALE_GE95, CI = CI_NUM_MALE_GE95) %>%
  mutate(area = "EBS", LCI = est - CI, UCI = est + CI)

d2 <- nbs %>%
  select(year, NUM_MALE_GE68, CI_NUM_MALE_GE68) %>%
  rename(est = NUM_MALE_GE68, CI = CI_NUM_MALE_GE68) %>%
  mutate(area = "NBS", LCI = est - CI, UCI = est + CI)


d2$LCI <- if_else(d2$LCI < 0, 0, d2$LCI)

# mature male SC
ggplot(d1, aes(year, est)) +
  geom_line(color = cb[6]) +
  geom_point(color = cb[6]) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), color = cb[6]) +
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(breaks=c(0,5, 50,100,200, 400, 800), minor_breaks = NULL) +
  coord_trans(y = "pseudo_log") +
  scale_x_continuous(breaks = seq(1980, 2020, 5), minor_breaks = NULL) +
  labs(title = "Abundance and 95% CI", y = "Millions") +
  geom_point(data = d2, aes(year, est), color = cb[4]) + 
  geom_errorbar(data = d2, aes(x = year, ymin = LCI, ymax = UCI), color = cb[4]) +
  annotate(geom = "text", label = "Eastern Bering (> 94 mm carapace width)", x = 1992, y = 20, color = cb[6], size = 4) +
  annotate(geom = "text", label = "Northern Bering (> 67 mm carapace width)", x = 1992, y = 10, color = cb[4], size = 4)

ggsave("./figs/mature male SC abundance TS.png", width = 6, height = 4, units = 'in')

# combine for legal male
d1 <- sc %>%
  select(year, NUM_MALE_GE78, CI_NUM_MALE_GE78) %>%
  rename(est = NUM_MALE_GE78, CI = CI_NUM_MALE_GE78) %>%
  mutate(area = "EBS", LCI = est - CI, UCI = est + CI)

d2 <- nbs %>%
  select(year, NUM_MALE_GE78, CI_NUM_MALE_GE78) %>%
  rename(est = NUM_MALE_GE78, CI = CI_NUM_MALE_GE78) %>%
  mutate(area = "NBS", LCI = est - CI, UCI = est + CI)


d2$LCI <- if_else(d2$LCI < 0, 0, d2$LCI)

# legal male SC
ggplot(d1, aes(year, est)) +
  geom_line(color = cb[6]) +
  geom_point(color = cb[6]) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), color = cb[6]) +
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(breaks=c(0,5, 50,100,200, 400, 800), minor_breaks = NULL) +
  coord_trans(y = "pseudo_log") +
  scale_x_continuous(breaks = seq(1980, 2020, 5), minor_breaks = NULL) +
  labs(title = "Abundance and 95% CI", y = "Millions") +
  geom_point(data = d2, aes(year, est), color = cb[4]) + 
  geom_errorbar(data = d2, aes(x = year, ymin = LCI, ymax = UCI), color = cb[4]) +
  annotate(geom = "text", label = "Eastern Bering", x = 1985, y = 20, color = cb[6], size = 4) +
  annotate(geom = "text", label = "Northern Bering", x = 1985, y = 10, color = cb[4], size = 4)

ggsave("./figs/legal male SC abundance TS.png", width = 6, height = 4, units = 'in')

# difference in # of individuals
d1$est[d1$year == 2019] - d1$est[d1$year == 2021] # 419 million!

# combine for preferred male
d1 <- sc %>%
  select(year, NUM_MALE_GE102, CI_NUM_MALE_GE102) %>%
  rename(est = NUM_MALE_GE102, CI = CI_NUM_MALE_GE102) %>%
  mutate(area = "EBS", LCI = est - CI, UCI = est + CI)

d2 <- nbs %>%
  select(year, NUM_MALE_GE102, CI_NUM_MALE_GE102) %>%
  rename(est = NUM_MALE_GE102, CI = CI_NUM_MALE_GE102) %>%
  mutate(area = "NBS", LCI = est - CI, UCI = est + CI)


d2$LCI <- if_else(d2$LCI < 0, 0, d2$LCI)


# preferred male SC
ggplot(d1, aes(year, est)) +
  geom_line(color = cb[6]) +
  geom_point(color = cb[6]) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), color = cb[6]) +
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(breaks=c(0, 1, 5, 10, 30, 50, 100, 200, 400), minor_breaks = NULL) +
  coord_trans(y = "pseudo_log") +
  scale_x_continuous(breaks = seq(1980, 2020, 5), minor_breaks = NULL) +
  labs(title = "Abundance and 95% CI", y = "Millions") +
  geom_point(data = d2, aes(year, est), color = cb[4]) + 
  geom_errorbar(data = d2, aes(x = year, ymin = LCI, ymax = UCI), color = cb[4]) +
  annotate(geom = "text", label = "Eastern Bering", x = 1985, y = 20, color = cb[6], size = 4) +
  annotate(geom = "text", label = "Northern Bering", x = 1985, y = 12, color = cb[4], size = 4)

ggsave("./figs/preferred male SC abundance TS.png", width = 6, height = 4, units = 'in')

# difference in # of individuals
d1$est[d1$year == 2019] - d1$est[d1$year == 2021] # 30▲ million!

# combine for mature female
d1 <- sc %>%
  select(year, NUM_MATURE_FEMALE, CI_NUM_MATURE_FEMALE) %>%
  rename(est = NUM_MATURE_FEMALE, CI = CI_NUM_MATURE_FEMALE) %>%
  mutate(area = "EBS", LCI = est - CI, UCI = est + CI)

d2 <- nbs %>%
  select(year, NUM_FEMALE_MATURE, CI_NUM_FEMALE_MATURE) %>%
  rename(est = NUM_FEMALE_MATURE, CI = CI_NUM_FEMALE_MATURE) %>%
  mutate(area = "NBS", LCI = est - CI, UCI = est + CI)

d2$LCI <- if_else(d2$LCI < 10, 10, d2$LCI) # note that I'm limiting LCI minimum to 10!!
d1$LCI <- if_else(d1$LCI < 30, 30, d1$LCI) # note that I'm limiting LCI minimum to 30!!

# mature female SC
ggplot(d1, aes(year, est)) +
  geom_line(color = cb[6]) +
  geom_point(color = cb[6]) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), color = cb[6]) +
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(breaks=c(0, 1, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000), minor_breaks = NULL) +
  coord_trans(y = "pseudo_log") +
  scale_x_continuous(breaks = seq(1980, 2020, 5), minor_breaks = NULL) +
  labs(title = "Abundance and 95% CI", y = "Millions") +
  geom_point(data = d2, aes(year, est), color = cb[4]) + 
  geom_errorbar(data = d2, aes(x = year, ymin = LCI, ymax = UCI), color = cb[4]) +
  annotate(geom = "text", label = "Eastern Bering", x = 1985, y = 20, color = cb[6], size = 4) +
  annotate(geom = "text", label = "Northern Bering", x = 1985, y = 12, color = cb[4], size = 4)

ggsave("./figs/mature female SC abundance TS.png", width = 6, height = 4, units = 'in')

# difference in # of individuals
d1$est[d1$year == 2018] - d1$est[d1$year == 2021] # 2672 - 2.7 billion!

# combine for immature male
d1 <- sc %>%
  select(year, NUM_MALE_LE94, CI_NUM_MALE_LE94) %>%
  rename(est = NUM_MALE_LE94, CI = CI_NUM_MALE_LE94) %>%
  mutate(area = "EBS", LCI = est - CI, UCI = est + CI)

d2 <- nbs %>%
  select(year, NUM_MALE_LE67, CI_NUM_MALE_LE67) %>%
  rename(est = NUM_MALE_LE67, CI = CI_NUM_MALE_LE67) %>%
  mutate(area = "NBS", LCI = est - CI, UCI = est + CI)



# immature male SC
ggplot(d1, aes(year, est)) +
  geom_line(color = cb[6]) +
  geom_point(color = cb[6]) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), color = cb[6]) +
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(breaks=c(5, 20, 50, 100, 200, 500, 1000, 3000, 5000, 1000), minor_breaks = NULL) +
  coord_trans(y = "pseudo_log") +
  scale_x_continuous(breaks = seq(1980, 2020, 5), minor_breaks = NULL) +
  labs(title = "Abundance and 95% CI", y = "Millions") +
  geom_point(data = d2, aes(year, est), color = cb[4]) + 
  geom_errorbar(data = d2, aes(x = year, ymin = LCI, ymax = UCI), color = cb[4]) +
  annotate(geom = "text", label = "Eastern Bering (< 95 mm carapace width)", x = 1992, y = 300, color = cb[6], size = 4) +
  annotate(geom = "text", label = "Northern Bering (< 68 mm carapace width)", x = 1992, y = 220, color = cb[4], size = 4)

ggsave("./figs/immature male SC abundance TS.png", width = 6, height = 4, units = 'in')


# difference in # of individuals
d1$est[d1$year == 2018] - d1$est[d1$year == 2021] # 5519 - 5.5 billion!


# combine for immature female
d1 <- sc %>%
  select(year, NUM_IMMATURE_FEMALE, CI_NUM_IMMATURE_FEMALE) %>%
  rename(est = NUM_IMMATURE_FEMALE, CI = CI_NUM_IMMATURE_FEMALE) %>%
  mutate(area = "EBS", LCI = est - CI, UCI = est + CI)

d2 <- nbs %>%
  select(year, NUM_FEMALE_IMMATURE, CI_NUM_FEMALE_IMMATURE) %>%
  rename(est = NUM_FEMALE_IMMATURE, CI = CI_NUM_FEMALE_IMMATURE) %>%
  mutate(area = "NBS", LCI = est - CI, UCI = est + CI)

d1$LCI <- if_else(d1$LCI < 5, 5, d1$LCI) # note that I'm limiting LCI minimum to 5!!

# immature female SC
ggplot(d1, aes(year, est)) +
  geom_line(color = cb[6]) +
  geom_point(color = cb[6]) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), color = cb[6]) +
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(breaks=c(10, 50, 100, 200, 500, 1000, 3000), minor_breaks = NULL) +
  coord_trans(y = "pseudo_log") +
  scale_x_continuous(breaks = seq(1980, 2020, 5), minor_breaks = NULL) +
  labs(title = "Abundance and 95% CI", y = "Millions") +
  geom_point(data = d2, aes(year, est), color = cb[4]) + 
  geom_errorbar(data = d2, aes(x = year, ymin = LCI, ymax = UCI), color = cb[4]) +
  annotate(geom = "text", label = "Eastern Bering", x = 1985, y = 7000, color = cb[6], size = 4) +
  annotate(geom = "text", label = "Northern Bering", x = 1985, y = 5000, color = cb[4], size = 4)

ggsave("./figs/immature female SC abundance TS.png", width = 6, height = 4, units = 'in')


# difference in # of individuals
d1$est[d1$year == 2018] - d1$est[d1$year == 2021] # 2566 - 2.6 billion!

# switch to BBRKC
dat <- read.csv("./data/BBRKC_abundance.csv")

head(dat)

dat <- dat %>%
  select(SURVEY_YEAR, IMMATMALE_MILLIONS, IMMATFEM_MILLIONS)

names(dat) <- c("year", "Male", "Female")

dat <- dat %>%
  pivot_longer(cols = -year)

xtra <- data.frame(year = c(2020, 2020),
                   name = c("Male", "Female"),
                   value = c(NA, NA))

dat <- rbind(dat, xtra)

dat <- dat %>%
  arrange(name) %>%
  arrange(year)

ggplot(dat, aes(year, value, color = name)) +
  geom_point() +
  geom_path() +
  scale_y_continuous(breaks=c(1, 5, 10, 50, 100, 150, 200), minor_breaks = NULL) +
  coord_trans(y = "pseudo_log") +
  scale_color_manual(values = cb[c(1,2)]) +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), legend.position = c(0.8, 0.9)) +
  ylab("Millions")

ggsave("./figs/immature BBRKC abundance TS.png", width = 6, height = 4, units = 'in')

## overfished stocks ------------------------------------

of <- read.csv("./data/legal_KC_abundance_Pribs_St.Matt.csv")

head(of)


of <- of %>%
  pivot_longer(cols = -year)

ggplot(of, aes(year, value)) +
  geom_point() +
  geom_path() +
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  coord_trans(y = "pseudo_log") +
  labs(y = "Millions of crab") +
  theme(axis.title.x = element_blank())

ggsave("./figs/overfished KC abundance TS.png", width = 5, height = 5, units = 'in')
