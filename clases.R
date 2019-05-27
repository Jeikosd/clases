library(readr) ### importar
library(lubridate) ## fechas
library(anytime)  ## fechas
library(dplyr)    ## manejo de datos

path <- "data/"

x <- read_csv(paste(path, 'weather.csv', sep = ""))  ## cargar informacion


x <- mutate(x, date_1 =
              ymd(paste(year, month, day, sep = "-"))) ## creando una columna date_1

select(x, year, month, day, date_1)

y <- group_by(x, origin)
summarise(y, temp_mean = mean(temp, na.rm = T))

x %>%
  group_by(origin, month) %>%
  summarise(temp_mean = mean(temp, na.rm = T),
            temp_sd = sd(temp, na.rm = T))


library(ggplot2)  ## motor de graficos hadley
ggplot() +
  geom_line(data = x, aes(x = date_1, y = temp))+
  theme_bw() ## tema para graficos cientificos


ggplot() +
  geom_line(data = x, aes(x = date_1, y = temp, color = origin))+
  theme_bw()

ggplot() +
  geom_line(data = x, aes(x = date_1, y = temp))+
  theme_bw() +
  facet_wrap(~origin) +
  labs(x = "Date", y = "Temperature")+
  ggtitle(label = "Prueba", subtitle = "2013")




y <- x %>%
  group_by(origin, month) %>%
  summarise(temp_mean = mean(temp, na.rm = T),
            temp_sd = sd(temp, na.rm = T))

ggplot() +
  geom_col(data = y, aes(x = month, y = temp_mean))

library(forcats)

y %>%
  mutate(month = as_factor(month))

y <- y %>%
  mutate(month = month(month, label = T, abbr = F))

ggplot() +
  geom_col(data = y, aes(x = month, y = temp_mean, fill = origin)) +
  theme_bw()

ggplot() +
  geom_col(data = y, aes(x = month, y = temp_mean, fill = origin),
           position = "dodge2") +
  theme_bw()

## grafico para precipitacion

ggplot() + ## inicio el grafico
  geom_line(data = x, aes(x = date_1, y = precip)) + ## grafico de linea
  facet_wrap(~origin) + ## panel por origin (estaciones)
  theme_bw() + ## cambio el tema del grafico
  labs(x = "Date", y = "Precipitation")  + ## nombre de eje x e y
  ggtitle(label = "Precipitation", subtitle = "2013") ## titulo y subtitulo

## hacer el grafico por season

y <- x %>%
  group_by(origin, month) %>%
  summarise(sum_prec = sum(precip, na.rm = T)) %>%
  mutate(month = month(month, label = T, abbr = F))

ggplot()+
  geom_col(data = y, aes(x = month, y = sum_prec, fill = origin),
           position = "dodge2") +
  theme_bw()


ggplot() +
  geom_boxplot(data = x, aes(x = month, y = temp))



x <- x %>%
  mutate(month = month(month, label = T, abbr = F))

ggplot() +
  geom_boxplot(data = x, aes(x = month, y = temp))

ggplot() +
  geom_boxplot(data = x, aes(x = month, y = temp, color = origin))

ggplot() +
  geom_boxplot(data = x, aes(x = month, y = temp, fill = origin)) + 
  theme_bw() +
  scale_fill_grey()







library(readr)
data = read_csv("data/weather.csv")
library(dplyr)
select(data, origin, temp)

mutate(data, new = substr(origin, start = 1, stop = 1)) %>%
  select(new, everything()) %>%
  filter(new == "E")

files <- list.files(path =  "data/", full.names = T) 

files <- list.files(path = "data/", full.names = T, pattern = "W")


data <- lapply(files, read_csv)
class(data)
str(data)

library(stringr) ## para manejo de strings cadenas de texto
read_csv_mod <- function(x){
  
  data <- read_csv(x)
  id <- str_remove(basename(x), ".csv")
  data <- mutate(data, id = id)
  return(data)
  
}

data <- lapply(files, read_csv_mod)

library(purrr)
data <- map(.x = files, .f = read_csv_mod) %>%
  bind_rows()

data %>%
  group_by(id, year, month) %>%
  summarise(t_max_avg = mean(t_max))



#####


library(readr) ## para cargar archivos de texto plano
library(dplyr) ## para manipular bases de datos

x <- read_csv(file = "D:/CIAT/Github/clases/data/data.csv")
#
avg <-  x %>% 
 group_by(Nationality) %>% 
  summarise(avg_overall = mean(Overall, na.rm = T), avg_age = mean(Age, na.rm = T), 
            number_player = n(), sd_overall = sd(Overall, na.rm = T))


avg %>% 
  top_n(15, avg_overall)


avg %>% 
  top_n(15, -avg_overall)

x %>% 
  top_n(100, )












