carriers <- read.table("biomedical-data/data/carriers.txt", header = T)
normals <- read.table("biomedical-data/data/normals.txt", header = T)

library(tidyverse)
data <- bind_rows("0" = normals, "1" = carriers, .id = "class")
data$class <- as.factor(data$class)

library(stringr)
data$date <- sprintf("%06d", data$date)
data$date <- str_replace_all(data$date, "000", "015")
data$date <- str_replace_all(data$date, "00", "15")
data$date <- as.Date(data$date, "%m%d%y")

data <- arrange(data, class, date)

# SAVE
save(data, file = "biomedical-data/data/biomedical-data.RData")