library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tm)


source("ruStopWords.R")


tokenizationUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "Инструкция", status = "info", solidHeader = TRUE, 
          HTML("<p>1. Нажмите &quotТокенизировать текст&quot'.</p>
                    <p>2. Изучите график распределения слов по главам.</p>
                    <p>3. Обратите внимание на частоту встречаемости стоп-слов.</p>
                    <p>4. Очистите текст от стоп слов и повторите анализ</p>")
      )),
    
    fluidRow(column(12, actionButton(ns("start_analysis"), "Токенизировать текст", style = "background-color: purple; color: white;"), style="float:left")), br(),
    
    fluidRow(
      box(title = "Распределение слов в исходном тексте", status = "primary", solidHeader = TRUE, width = 10, 
          DTOutput(ns("word_counts")),
          selectInput(ns("document"), "Выберите документ:", choices = ""),
          sliderInput(ns("num_words"), "Выберите количество популярных слов:", min = 1, max = 20, value = 5),
          plotOutput(ns("word_freq_plot")),
          HTML("<b>Часто встречаются предлоги, союзы и другие слова, не позволяющие понять, о чём данный текст</b>")
      ),
      box(title = "Токенизация", status = "primary", solidHeader = FALSE, width = 2, 
          HTML("<p>Токенизация текста - это процесс разделения текста на отдельные слова или токены. 
                        Это необходимый шаг в подготовке данных для тематического моделирования текста. 
                        Каждое слово становится отдельным токеном, что позволяет устранить лишние символы, знаки препинания и пробелы, 
                        иногда также приводит слова к нормализованному виду (например, приводит все слова к нижнему регистру, убирает окончания). 
                        </p>
                             ")
      )
    ),
    fluidRow(
      box(title = "Удаление стоп-слов из текста", status = "primary", solidHeader = FALSE, width = 6, 
          HTML("Под стоп-словами понимаются частицы, предлоги, числительные. Они считаются бесполезными 
                             с точки зрения тематического моделирования и всегда отбрасываются. 
                             Также уникальным для данной области анализа текста отбрасывание 
                             некоторых прилагательных и наречий, глаголов, которые не помогут 
                             при тематической классификации текста (“нет”, “очень”, “мало” и т.д.). 
                             Этот процесс позволяет значительно уменьшить размер текста и размер словаря, 
                             что ускоряет процесс вычислений, снижает требуемые вычислительные ресурсы и 
                             даёт более релевантные результаты в процессе анализа.")
      ),
      box(title = "Мера TF-IDF", status = "primary", solidHeader = FALSE, width = 6, 
          HTML("<strong>TF-IDF (Term Frequency-Inverse Document Frequency)</strong> - это статистическая мера, 
               используемая в информационном поиске и анализе текста для оценки важности слова в контексте документа.
                 <p><strong>TF</strong> - рассчитывается как отношение числа раз, когда термин встречается в документе, 
                к общему числу слов в документе.</p>
                <p><strong>IDF </strong> - 
                это мера того, насколько уникален термин по всей коллекции документов. 
                Она рассчитывается как логарифм отношения общего числа документов к числу документов, 
                содержащих данный термин.</p>
                <p><strong>TF-IDF </strong> - это произведение TF и IDF. 
               Оно используется для оценки важности термина в контексте конкретного документа 
               по сравнению с его важностью в коллекции документов. Чем
               больше TF-IDF термина для данного документа, тем более важен этот термин для этого документа.</p>")
      )
          
      
    ), 
    fluidRow(column(12, div(actionButton(ns("clean_stop_words"), "Очистить стоп слова", style = "background-color: purple; color: white;"), style="float:left"))), br(),
    fluidRow(
      box(title = "Распределение слов в очищенном тексте", status = "primary", solidHeader = TRUE, width = 10, 
          DTOutput(ns("clean_word_counts")),
          selectInput(ns("clean_document"), "Выберите документ:", choices = ""),
          sliderInput(ns("clean_num_words"), "Выберите количество популярных слов:", min = 1, max = 20, value = 5),
          plotOutput(ns("clean_word_freq_plot"))
      )
    )
  )
}

tokenizationServer <- function(id, textByChapters) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Анализ текста и график частот слов до очистки
      wordsByChapters <- eventReactive(input$start_analysis, {
        req(textByChapters)
        tokenized_text <- textByChapters() %>%
          unnest_tokens(word, text)
        #подсчёт числа вхождений слов
        word_counts <- tokenized_text %>%
          count(document, word, sort = TRUE) %>%
          arrange(document, desc(n)) %>%
          ungroup()
        return(word_counts)
      })
      
      #Анализ после очистки от стоп слов
      observeEvent(input$start_analysis, {
        req(wordsByChapters())
        #Создание списка уникальных названий документов
        unique_documents <- unique(wordsByChapters()$document)
        #Устанавливаем список уникальных документов в selectInput
        updateSelectInput(session, "document", choices = unique_documents)
        #Вывод в таблицу
        output$word_counts <- renderDT({
          datatable(wordsByChapters(), options = list(pageLength = 5))
        })
      })
      
      #Отображение частотной гистограммы
      output$word_freq_plot <- renderPlot({
        req(wordsByChapters)
        #Фильтрация данных по выбранному документу
        filtered_data <- wordsByChapters() %>%
          filter(document == input$document)
        #Получение выбранного количества популярных слов
        top_words <- head(filtered_data$word, input$num_words)
        #Фильтрация данных для выбранных слов
        selected_data <- filtered_data %>%
          filter(word %in% top_words)
        #Построение графика
        ggplot(selected_data, aes(x = reorder(word, -n), y = n, fill = word)) +
          geom_col() +
          theme_minimal() +
          labs(x = "Слово", y = "Частота", title = paste("Топ", input$num_words, "популярных слов для", input$document))
      })
      
      #Очистка текста от стоп-слов
      cleanWordCount <- eventReactive(input$clean_stop_words, {
        req(wordsByChapters())
        id1 <- showNotification("Очистка стоп-слов (Этап 1 из 3)", type = "message", duration = NULL)
        #очистка английских слов
        data <- wordsByChapters() %>%
          anti_join(stop_words)
        id2 <- showNotification("Очистка стоп-слов (Этап 2 из 3)", type = "message", duration = NULL)
        
        #Очистка от русских стоп слов с помощью пакета tm
        cleaned_text <- data %>%
          mutate(word = ifelse(is.na(word), NA, tm::removeWords(word, stopwords("russian")))) %>%
          filter(word != "")
        
        id3 <- showNotification("Очистка стоп-слов (Этап 3 из 3)", type = "message", duration = NULL)
        
        #Дополнительная очистка от русских стоп слов с помощью разработанной функции
        cleaned_text <- removeRuStopWords(cleaned_text)
        
        removeNotification(id1)
        removeNotification(id2)
        removeNotification(id3)
        id4 <- showNotification("Готово! Очистка стоп-слов завершена", type = "message", duration = 10)
        #Подсчёт меры TF-IDF
        counts <- cleaned_text %>%
          bind_tf_idf(word, document, n)
        counts <- counts %>% 
          mutate(tf = round(tf, 3),
                 idf = round(idf, 3),
                 tf_idf = round(tf_idf, 3))
        return(counts)
      })
      
      
      #Анализ после очистки от стоп слов
      observeEvent(input$clean_stop_words, {
        req(cleanWordCount())
        #Создание списка уникальных названий документов
        unique_documents <- unique(cleanWordCount()$document)
        #Устанавливаем список уникальных документов в selectInput
        updateSelectInput(session, "clean_document", choices = unique_documents)
        #Вывод в таблицу
        output$clean_word_counts <- renderDT({
          req(cleanWordCount)
          datatable(cleanWordCount(), options = list(pageLength = 5))
        })
      })
      
      #Отображение частотной гистограммы
      output$clean_word_freq_plot <- renderPlot({
        req(cleanWordCount(), input$clean_stop_words, input$clean_document)
        #Фильтрация данных по выбранному документу
        filtered_data <- cleanWordCount() %>%
          filter(document == input$clean_document)
        #Получение выбранного количества популярных слов
        top_words <- head(filtered_data$word, input$clean_num_words)
        #Фильтрация данных для выбранных слов
        selected_data <- filtered_data %>%
          filter(word %in% top_words)
        #Построение графика
        ggplot(selected_data, aes(x = reorder(word, -n), y = n, fill = word)) +
          geom_col() +
          theme_minimal() +
          labs(x = "Слово", y = "Частота", title = paste("Топ ", input$clean_num_words, 
                                                         " популярных слов для", input$clean_document))
      })
      return(cleanWordCount)
    }
  )
}
