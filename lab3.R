library(readr) # Чтение данных из различных файлов
library(ggplot2) # Визуализация данных
library(dplyr) # Трансформация данных
library(tidyr)

# Читаем данные
sales <- read_csv("data/data.csv")

# Первый взгляд на данные
str(sales)
head(sales)

# Задача: отдел логистики хочет узнать, в какой день месяца больше покупают
# для того, чтобы спланировать график выхода персонала
# Дополнительно посмотреть топ 5 стран для зарубежных логичистических центров

dens_sales <- sales %>% 
  # извлекаем даты и время
  mutate(
    dttm = lubridate::mdy_hm(InvoiceDate),
    dom = lubridate::day(dttm),
    date = lubridate::floor_date(dttm, unit = "day")
  ) %>% 
  # оставляем только три столбца: номер заказа (нам надо подсчитать их количество)
  # день месяца, страну (для дальнейшего анализа)
  select(InvoiceNo, dom, Country) %>% 
  # удаляем дублирующиеся строки, чтобы количество позиций в заказе
  # не влияло на результаты
  distinct()

ggplot(data = dens_sales, mapping = aes(x = dom))+
  # Используем график плотности распределения
  # для визуализации сезонности*
  geom_density()

# Формируем список топ-5 стран по выручке (UK не удаляем)
top_countries <- sales %>% 
  # Исключаем страну, в которой располагается наш ИМ
  filter(Quantity > 0) %>% 
  # Группируем по странам для дальнейшей агрегации
  group_by(Country) %>% 
  # Вычисляем суммарную выручку по группе
  # как произведение количества на цену
  summarise(revenue = sum(Quantity * UnitPrice)) %>%
  # Сортируем по выручке по убыванию
  arrange(desc(revenue)) %>% 
  # Оставляем только 10 первых строк
  head(5)

# Фильтруем предыдущий датасет
dens_sales_filtered <- dens_sales %>% 
  filter(Country %in% top_countries$Country)

ggplot(data = dens_sales_filtered, mapping = aes(x = dom, fill = Country))+
  # для красоты делаем попрозрачнее заливку и стираем границу заливки
  geom_density(alpha = 0.8, col = "transparent")+
  # разбиваем на панели-строки по странам
  facet_grid(rows = vars(Country))+
  # удаляем легенду - она излишняя
  theme(legend.position = "none")
