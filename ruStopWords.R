library(dplyr)
##Дополнительная очистка от русских стоп сло из списка

stopWords <- c("это", "эта", "этот", "эти", "ко", "нам", "тебе", "снова",
               "мог", "ка", "стало", "лишь", "который","которые", "которая", "которое", "пока",  "всё", "ещё", "ред", "также",
               "издании",
               "ибо", "оно")

removeRuStopWords <- function(text) {
  stopWords_df <- data.frame(word = stopWords)
  # Удаление стоп-слов из заданного списка
  cleanedText <- text %>%
    anti_join(stopWords_df)
  return(cleanedText)
}