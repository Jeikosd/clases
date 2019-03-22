library(readr) ### importar
library(lubridate) ## fechas
library(anytime)  ## fechas
library(dplyr)    ## manejo de datos

path <- "data/"

x <- read.csv(paste(path, 'weather.csv', sep = ""))
head(x)
x <- read_csv(paste(path, 'weather.csv', sep = ""))
x <- mutate(x, date_1 =
              ymd(paste(year, month, day, sep = "-")))

select(x, year, month, day, date_1)

y <- group_by(x, origin)
summarise(y, temp_mean = mean(temp, na.rm = T))

x %>%
  group_by(origin, month) %>%
  summarise(temp_mean = mean(temp, na.rm = T),
            temp_sd = sd(temp, na.rm = T))


library(ggplot2)
ggplot() +
  geom_line(data = x, aes(x = date_1, y = temp))+
  theme_bw()


ggplot() +
  geom_line(data = x, aes(x = date_1, y = temp, color = origin))+
  theme_bw()

ggplot() +
  geom_line(data = x, aes(x = date_1, y = temp))+
  theme_bw() +
  facet_wrap(~origin) +
  labs(x = "Date", y = "Temperature")



y <- x %>%
  group_by(origin, month) %>%
  summarise(temp_mean = mean(temp, na.rm = T),
            temp_sd = sd(temp, na.rm = T))

ggplot() +
  geom_col(data = y, aes(x = month, y = temp_mean))

library(forcats)
y %>%
  mutate(y = as_factor(month))

y <- y %>%
  mutate(month = month(month, label = T))

ggplot() +
  geom_col(data = y, aes(x = month, y = temp_mean)) +
  theme_bw()