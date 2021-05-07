load(file = "biomedical-data/data/biomedical-data.RData")

library(tidyverse)
library(ggplot2)

date_mean_m1 <- data %>%
  group_by(date) %>%
  summarise(m1 = mean(m1))

date_mean_m2 <- data %>%
  group_by(date) %>%
  summarise(m2 = mean(m2))

date_mean_m3 <- data %>%
  group_by(date) %>%
  summarise(m3 = mean(m3))

date_mean_m4 <- data %>%
  group_by(date) %>%
  summarise(m4 = mean(m4))

library(patchwork)

date_m1 <- ggplot(data, aes(x = date, y = m1)) +
  geom_point(alpha = 0.3) +
  geom_point(data = date_mean_m1, size = 3, colour = "red") +
  labs(x = "", y = "Measurements for m1") +
  theme_classic()

date_m2 <- ggplot(data, aes(x = date, y = m2)) +
  geom_point(alpha = 0.3) +
  geom_point(data = date_mean_m2, size = 3, colour = "red") +
  labs(x = "", y = "Measurements for m2") +
  theme_classic()

date_m3 <- ggplot(data, aes(x = date, y = m3)) +
  geom_point(alpha = 0.3) +
  geom_point(data = date_mean_m3, size = 3, colour = "red") +
  labs(x = "Date (yyyy-mm)", y = "Measurements for m3") +
  theme_classic()

date_m4 <- ggplot(data, aes(x = date, y = m4)) +
  geom_point(alpha = 0.3) +
  geom_point(data = date_mean_m4, size = 3, colour = "red") +
  labs(x = "Date (yyyy-mm)", y = "Measurements for m4") +
  theme_classic()

(date_m1 + date_m2) / (date_m3 + date_m4)
