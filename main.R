install.packages("ggplot2")
library (ggplot2)

players <- read.csv("D:\\r\\1\\hltv_playerStats-complete.csv", 
                    header = TRUE, 
                    stringsAsFactors = FALSE)

country <- players[1:200, ]$country
headshot_percentage <- players[1:200, ]$headshot_percentage
rounds_played <- players[1:200, ]$rounds_played

#print(headshot_percentage)
#hist(headshot_percentage, breaks = 20, col = "pink", 
#     xlab = "Процент Headshots", 
#     ylab = "Частота", 
#     main = "Гистограмма Headshot percentage")

ggplot(data = players[1:200, ], mapping = aes(country)) +
  geom_bar(colour = "black", fill = "pink") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Страны") +
  ylab("Количество игроков")

ggplot(data = players[1:200, ], mapping = aes(headshot_percentage)) +
  geom_freqpoly(bins = 10, colour = "red") +
  xlab("Процент headshots") +
  ylab("Количество игроков")

cat("Среднее по проценту headshot: ", mean(headshot_percentage))
cat("Среднее по количеству раундов:", mean(rounds_played))

cat("Медиана по проценту headshot:", median(headshot_percentage))
cat("Медиана по количеству раундов:", median(rounds_played))

find_mode <- function(x) {names(which.max(table(x)))}
cat("Мода по проценту headshot: ", find_mode(headshot_percentage))
cat("Мода по количеству раундов: ", find_mode(rounds_played))

myvar <- function(x) {mean((x - mean(x)) ^ 2)} #встроенная функция делит не на n, а на n-1
cat("Дисперсия по проценту headshot: ", var(headshot_percentage))
cat("Дисперсия по количеству раундов: ", var(rounds_played))

cat("Среднеквадратичное отклонение по проценту headshot: ", sd(headshot_percentage))
cat("Среднеквадратичное отклонение по количеству раундов: ", sd(rounds_played))
