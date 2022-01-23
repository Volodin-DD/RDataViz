library(readr) # Чтение данных из различных файлов
library(ggplot2) # Визуализация данных
library(dplyr) # Трансформация данных
library(tidyr)

# Читаем данные
sales <- read_csv("data/data.csv")

# Первый взгляд на данные
str(sales)
head(sales)

# Задача: сразу два отдела (маркетинг и реклама), хотят узнать,
# в какое время суток чаще покупают.
# Маркетингу необходимо поянть, когда можно экспериментировать с ценой,
# а рекламе: как сделать цену на рекламу на сайте более гибкой.
# Бонус: улодить всё в один "пайп" 
sales %>% 
  # оставим только продажи, удалив возвраты
  filter(Quantity > 0) %>% 
  # добудем дату и время. Дата нужна, чтобы не потерять наблюдения
  mutate(
    dttm = lubridate::mdy_hm(InvoiceDate),
    time = factor(lubridate::hour(dttm), levels = 0:23, ordered = TRUE),
    date = lubridate::floor_date(dttm, unit = "day")
  ) %>%
  # группируем по дате и времени
  group_by(date, time) %>% 
  # вычисляем количество заказов
  summarise(Orders = length(unique(InvoiceNo))) %>% 
  ggplot(data = ., mapping = aes(x = time, y = Orders, fill = time, col = time))+
  # Тип визуализации - ящик с усами (boxplot)
  geom_boxplot(alpha = 0.8)+
  # Выбор предустановленной темы
  theme_minimal()+
  # Удаление легенды (она излишняя)
  theme(legend.position = "none")
