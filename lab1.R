library(readr) # Чтение данных из различных файлов
library(ggplot2) # Визуализация данных
library(dplyr) # Трансформация данных
library(tidyr)

# Читаем данные
sales <- read_csv("data/data.csv")

# Первый взгляд на данные
str(sales)
head(sales)

# Задача - выбрать графически из топ 10 стран по выручке те страны,
# где среднее количество позиций в заказе ниже общего среднего
top_countries <- sales %>% 
  # Исключаем страну, в которой располагается наш ИМ
  filter(Country != "United Kingdom", Quantity > 0) %>% 
  # Группируем по странам для дальнейшей агрегации
  group_by(Country) %>% 
  # Вычисляем суммарную выручку по группе
  # как произведение количества на цену
  summarise(revenue = sum(Quantity * UnitPrice)) %>%
  # Сортируем по выручке по убыванию
  arrange(desc(revenue)) %>% 
  # Оставляем только 10 первых строк
  head(10)

# Подготовка данных
sales_new <- sales %>% 
  # Исключаем страну, в которой располагается наш ИМ
  filter(Country != "United Kingdom", Quantity > 0) %>% 
  # Группируем по стране и заказу
  group_by(Country, InvoiceNo) %>%
  # Считаем количество позиций в каждом заказе
  summarise(OrderLength = length(unique(StockCode))) %>%
  # Убираем группировку для дальнейших вычислений
  ungroup() %>% 
  # Считаем среднюю длину заказа для всего ИМ 
  mutate(AverageOrderLength = mean(OrderLength)) %>% 
  # Группируем по стране
  group_by(Country) %>% 
  # Вычисляем среднюю длину заказа для каждой страны
  # и сотавляем общую среднюю длину
  summarise(
    OrderLength = mean(OrderLength),
    AverageOrderLength = mean(AverageOrderLength)
    ) %>% 
  # Оставляем только топ 10 стран
  filter(Country %in% top_countries$Country) %>% 
  # Добавляем категориальную переменную-индикатор
  # Она указывает на отношение средней длины в стране
  # к общей средней длине
  mutate(AvOLCompare = if_else(OrderLength > AverageOrderLength, "Higher", "Lower"))

# Изучим получившийся датасет
str(sales_new)
head(sales_new)

# Слой 0 - данные
g <- ggplot(data = sales_new)
g

# Слой 1 - данные для осей
g <- ggplot(data = sales_new, mapping = aes(x = Country, y = OrderLength))
g

# Слои 2,3 - выбор типа визуализации и допольнительные измерения
g <- g + geom_col(mapping = aes(fill = AvOLCompare))
g

# Слой 4 - дополнительная визуализация
g <- g + geom_hline(mapping = aes(yintercept = AverageOrderLength))
g

# Косметический слой - система координат
g <- g + coord_flip()
g

# Единый код графика
ggplot(data = sales_new, mapping = aes(x = Country, y = OrderLength))+
  geom_col(mapping = aes(fill = AvOLCompare))+
  geom_hline(mapping = aes(yintercept = AverageOrderLength))+
  coord_flip()
