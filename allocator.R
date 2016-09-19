
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)

dat1 <- read.csv ("Study 1_Data.csv")
dat2 <- read.csv ("Study 2_Data.csv")
dat3 <- read.csv ("Study 3_Data.csv")
dat4 <- read.csv ("Study 4_Data.csv")
dat5 <- read.csv ("Study 5_Data.csv")
dat6 <- read.csv ("Study 6_Data.csv")
dat7 <- read.csv ("Study 7_Data.csv")

# Exp 1
dat1 <- dat1 %>% 
  mutate(exp = 1,
         pick_flip = factor(pick_flip, labels = c("pick", "flip")),
         win_lose = factor(win_lose, labels = c("bonus", "no bonus")),
         role = factor(role, labels = c("receiver", "allocator")),
         condition = interaction(pick_flip, win_lose))
# ggplot(dat1, aes(x = condition, y = feel)) +
#   geom_violin() +
#   facet_wrap(~role) +
#   theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5))
ggplot(dat1, aes(x = feel)) +
  geom_bar(stat = "count") +
  facet_grid(condition~role) +
  scale_x_continuous(breaks = 1:7)

# Exp 2
dat2 <- dat2 %>% 
  mutate(exp = 2,
         pick_flip = factor(pick_flip, labels = c("pick", "flip")),
         win_lose = factor(win_lose, labels = c("bonus", "no bonus")),
         role = factor(role, labels = c("receiver", "allocator")),
         condition = interaction(pick_flip, win_lose))
# ggplot(dat2, aes(x = condition, y = feel)) +
#   geom_violin() +
#   facet_wrap(~role) +
#   theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5))
ggplot(dat2, aes(x = feel)) +
  geom_bar(stat = "count") +
  facet_grid(condition~role) +
  scale_x_continuous(breaks = 1:7)

# Exp 3
dat3 <- dat3 %>% 
  mutate(exp = 3,
         condition = interaction(DecRec, fairunfair))
ggplot(dat3, aes(x = feel)) +
  geom_bar(stat = "count") +
  facet_grid(condition ~ fiftytwentyfive) +
  scale_x_continuous(breaks = 1:7)
# What's up with the zero?

# Exp 4
dat4 <- dat4 %>% 
  mutate(exp = 4,
         pick_flip = factor(pick_flip, labels = c("pick", "flip")),
         win_lose = factor(win_lose, labels = c("bonus", "no bonus")),
         #role = factor(role, labels = c("receiver", "allocator")),
         condition = interaction(pick_flip, win_lose))
ggplot(dat4, aes(x = feel)) +
  geom_bar(stat = "count") +
  facet_grid(win_lose ~ pick_flip) +
  scale_x_continuous(breaks = 1:7)

# Exp 5
dat5 <- dat5 %>% 
  mutate(exp = 5,
         pick_flip = factor(pick_flip, labels = c("pick", "flip")),
         win_lose = factor(win_lose, labels = c("bonus", "no bonus")),
         #role = factor(role, labels = c("receiver", "allocator")),
         condition = interaction(pick_flip, win_lose))
ggplot(dat5, aes(x = feel)) +
  geom_bar(stat = "count") +
  facet_grid(win_lose ~ pick_flip) +
  scale_x_continuous(breaks = 1:7)

# Exp 6
dat6 <- dat6 %>% 
  mutate(exp = 6,
         pick_flip = factor(pick_flip, labels = c("pick", "flip")),
         win_lose = factor(win_lose, labels = c("bonus", "no bonus")),
         #role = factor(role, labels = c("receiver", "allocator")),
         condition = interaction(pick_flip, win_lose),
         id = as.factor(participant))
ggplot(dat6, aes(x = feel)) +
  geom_bar(stat = "count") +
  facet_grid(win_lose ~ pick_flip) +
  scale_x_continuous(breaks = 1:7)
# HLM (b/c fully nested w/in participants)
mod6 <- lmer(feel ~ pick_flip * win_lose + (1|id), data = dat6)
summary(mod6)

# Bad model failing to account for nestedness
mod6.bad <- aov(feel ~ pick_flip * win_lose, data = dat6)
summary(mod6.bad)

# repeated-measures model -- this is the one they'd used
mod6.rm <- aov(feel ~ pick_flip * win_lose + Error(id / (pick_flip * win_lose)), 
               data = dat6)
summary(mod6.rm)

# Exp7
dat7 <- dat7 %>% 
  mutate(exp = 7,
         pick_flip = factor(pick_flip, labels = c("pick", "flip")),
         win_lose = factor(win_lose, labels = c("bonus", "no bonus")),
         role = factor(role, labels = c("receiver", "allocator", "?", "???")),
         condition = interaction(pick_flip, win_lose))
ggplot(dat7, aes(x = feel, fill = role)) +
  geom_bar(stat = "count") +
  facet_grid(win_lose ~ pick_flip) +
  scale_x_continuous(breaks = 1:7)
# What is role = 3? role = 4?
# Felix's plot237
dat7 %>% 
  filter(pick_flip == "pick") %>% 
  ggplot(aes(x = feel, fill = win_lose)) +
  geom_bar(stat = "count") +
  facet_grid(win_lose~.)


# Aggregate
