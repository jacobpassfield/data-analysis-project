load(file = "biomedical-data/data/biomedical-data.RData")

library(tidyverse)
library(ggplot2)

age_mean_m1 <- data %>%
  group_by(age) %>%
  summarise(m1 = mean(m1))

age_mean_m2 <- data %>%
  group_by(age) %>%
  summarise(m2 = mean(m2))

age_mean_m3 <- data %>%
  group_by(age) %>%
  summarise(m3 = mean(m3))

age_mean_m4 <- data %>%
  group_by(age) %>%
  summarise(m4 = mean(m4))

library(patchwork)

age_m1 <- ggplot(data, aes(x = age, y = m1)) +
  geom_point(alpha = 0.3) +
  geom_point(data = age_mean_m1, size = 3, colour = "red") +
  labs(x = "", y = "Measurements for m1") +
  theme_classic()

age_m2 <- ggplot(data, aes(x = age, y = m2)) +
  geom_point(alpha = 0.3) +
  geom_point(data = age_mean_m2, size = 3, colour = "red") +
  labs(x = "", y = "Measurements for m2") +
  theme_classic()

age_m3 <- ggplot(data, aes(x = age, y = m3)) +
  geom_point(alpha = 0.3) +
  geom_point(data = age_mean_m3, size = 3, colour = "red") +
  labs(x = "Age of patient in years", y = "Measurements for m3") +
  theme_classic()

age_m4 <- ggplot(data, aes(x = age, y = m4)) +
  geom_point(alpha = 0.3) +
  geom_point(data = age_mean_m4, size = 3, colour = "red") +
  labs(x = "Age of patient in years", y = "Measurements for m4") +
  theme_classic()

(age_m1 + age_m2) / (age_m3 + age_m4)
