library(shiny)
library(shinydashboard)

library(gutenbergr)
library(tidytext) 
library(tidyr) 
library(dplyr)
library(stringr)
library(DT)
library(ggplot2)


ui <- dashboardPage(
  dashboardHeader(title = "TopicModeling", titleWidth = 270),
  dashboardSidebar(
    width = 270,
    sidebarMenu(id = "sidebar",
                menuItem("Введение", tabName = "theory", icon = icon("book")),
                
                menuItem("Занятие 1", tabName = "practice1", icon = icon("pencil"), 
                         menuSubItem("Загрузка и осмотр данных", tabName = "data_loading"),
                         menuSubItem("Токенизация текста", tabName = "text_tokenization"),
                         menuSubItem("Матрица терминов", tabName = "dtm"),
                         menuSubItem("Проверка знаний 1", tabName = "quiz1")
                ),
                menuItem("Занятие 2", tabName = "practice2", icon = icon("pencil"), 
                         menuSubItem("Модель LDA", tabName = "lda_model"),
                         menuSubItem("Результаты моделирования", tabName = "lda_result"),
                         menuSubItem("Ошибки классификации", tabName = "classification_errors"),
                         menuSubItem("Проверка знаний 2", tabName = "quiz2")
                ),
                menuItem("Экзамен", tabName = "quiz", icon = icon("question")),
                menuItem("Результаты тестов", tabName = "stats", icon = icon("chart-bar")),
                menuItem("Помощь", tabName = "help", icon = icon("question-circle"))
                
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "theory", sidebarLayout(sidebarPanel(actionButton("button1", "Нажми для приветствия Мира"), actionButton("button2", "Нажми для приветствия Привета")), mainPanel(textOutput("text")))),
      tabItem(tabName = "practice1", h1("Содержимое практики")),
      tabItem(tabName = "lesson1", h1("Первичная обработка данных")),
      tabItem(tabName = "data_loading", h1("Загрузка и осмотр данных"),
              fluidRow(
                box(width = 10, solidHeader = TRUE, status = "primary",
                    actionButton("viewLibrary", "Просмотреть библиотеку Gutenberg"),
                    DTOutput("libraryTable", height = "300px")),
                box(title = "Проект Gutenberg", width = 2, solidHeader = TRUE, status = "primary", collapsible = TRUE, 
                    HTML("<p>Проект \"Гутенберг\"— это общественная некоммерческая инициатива, которая занимается созданием и распространением цифровой коллекции произведений, находящихся в общественном достоянии.</p>
                  <p>Проект считается старейшей в мире электронной библиотекой. Большинство работ было оцифровано волонтёрами и доступно для свободного скачивания.</p>
                  <p>На 2021 год в коллекции проекта более 60 000 книг.</p>")
                ), br(), br()
              ),
              fluidRow(
                box(width = 12, solidHeader = TRUE, status = "primary",
                    textInput("bookIDs", "Введите ID книг для загрузки (например, 1268, 33111, 1074):"),
                    actionButton("downloadBooks", "Скачать книги"),
                    DTOutput("booksTable", height = "300px"),
                    textInput("chapterSplitter", "Разделить на главы:", value = "^Chapter "),
                    checkboxInput("ignoreCase", "Игнорировать регистр", value = TRUE),
                    actionButton("splitChapters", "Разделить на главы"),
                    DTOutput("chapterTable")  
                )
                
              ),
              fluidRow(column(12, div(actionButton(inputId="next1", label="Далее"), style="float:left")))
              
      ),
      tabItem(tabName = "text_tokenization", h1("Токенизация текста"), 
              fluidRow(
                box(title = "Инструкция", status = "primary", solidHeader = TRUE, 
                    HTML("<p>1. Нажмите 'Разделить текст на слова' для разделения текста на слова и анализа.</p>
                    <p>2. Изучите график распределения слов по главам.</p>
                    <p>3. Обратите внимание на частоту стоп-слов.</p>")
                )),
              fluidRow(column(12, div(actionButton("start_analysis", "Разделить на слова")), style="float:left")), br(), 
              fluidRow(
                box(DTOutput("word_counts"),
                    selectInput("document", "Выберите документ:", choices = ""),
                    sliderInput("num_words", "Выберите количество популярных слов:", min = 1, max = 20, value = 5),
                    plotOutput("word_freq_plot"), width = 12)
              ),
              
              fluidRow(
                box(title = "Чистка от стоп-слов", status = "warning", solidHeader = TRUE, 
                    "Нажмите кнопку ниже, чтобы очистить текст от стоп-слов и повторить анализ."
                ), br(), br()),
              fluidRow(column(12, div(actionButton("clean_stop_words", "Очистить стоп слова"), style="float:left"))), br(),
              fluidRow(
                box(title = "Результат удаления стоп слов", status = "primary",
                    DTOutput("clean_word_counts"),
                    selectInput("clean_document", "Выберите документ:", choices = ""),
                    sliderInput("clean_num_words", "Выберите количество популярных слов:", min = 1, max = 20, value = 5),
                    plotOutput("clean_word_freq_plot"), width = 12),
                
              ),
              fluidRow(column(12, div(actionButton(inputId="next2", label="Далее"), style="float:left")))
              
      ),
      tabItem(tabName = "dtm", h1("Матрица терминов документа")
              
              
              
              
              
              
      ),
      tabItem(tabName = "quiz1", h1("Проверка знаний 1")),
      tabItem(tabName = "quiz", fluidRow(column(12, h3("Тест по языку R:"), uiOutput("questions"), hr(), actionButton("submit", "Отправить ответы")))),
      tabItem(tabName = "stats", fluidRow(column(12, h3("Статистика верных ответов:"), tableOutput("stats")))),
      tabItem(tabName = "help", h1("Помощь"), p("Здесь будет страница помощи.")),
      tabItem(tabName = "feedback", h1("Отзыв"), p("Здесь будет страница отзыва."))
    )
  ),
  
)


server <- function(input, output, session) {
  
  libraryTable <- eventReactive(input$viewLibrary, {
    tryCatch({
      id <- showNotification("Загрузка библиотеки", type = "message", duration = 5)
      gutenberg_metadata
    }, error = function(e) {
      NULL
    })
  })
  
  output$libraryTable <- renderDT({
    libraryData <- libraryTable()
    if (is.null(libraryData)) {
      showNotification("Ошибка: Не удалось загрузить библиотеку", type = "error")
      return(NULL)
    }
    libraryData
  }, server = FALSE, options = list(scrollX = TRUE, scrollY = "300px", pageLength = 5))
  
  
  book <- eventReactive(input$downloadBooks, {
    req(input$bookIDs)
    book_ids <- as.numeric(unlist(strsplit(input$bookIDs, ",")))
    id <- showNotification("Загрузка книг...", type = "message", duration = NULL)
    # Загружаем книги
    books <<- tryCatch({
      gutenberg_download(book_ids, meta_fields = "title", mirror="http://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/")
    }, error = function(e) {
      NULL  # В случае ошибки возвращаем NULL
    })
    removeNotification(id)
    
    # Проверяем, успешно ли были загружены данные
    if (is.null(books)) {
      showNotification("Ошибка при загрузке книг.", type = "error")
    } else {
      output$booksTable <- renderDT({
        books
      }, options = list(pageLength = 5))
    }
  })
  
  output$booksTable <- renderDT({
    req(book())
    datatable(books()[, c("title", "text")], options = list(scrollX = TRUE, scrollY = "300px", pageLength = 5))
  }, server = FALSE)
  
  splitTextIntoChapters <- eventReactive(input$splitChapters, {
    req(input$chapterSplitter)
    books %>% group_by(title) %>%
      mutate(chapter = cumsum(str_detect(
        text, regex("^chapter ", ignore_case = TRUE)
      ))) %>%
      ungroup() %>%
      filter(chapter > 0) %>%
      unite(document, title, chapter)
  })
  
  output$chapterTable <- renderDT({
    req(splitTextIntoChapters())
    datatable(splitTextIntoChapters(), options = list(scrollX = TRUE, scrollY = "200px", pageLength = 5), rownames = FALSE)
  }, server = FALSE)
  
  observeEvent(input$next1, {
    updateTabItems(session, "sidebar", "text_tokenization") 
  })
  
  #################################################################################################################
  
  # функция word_counts_unique
  word_counts_unique <- function(data) {
    word_counts <- data %>%
      count(document, word, sort = TRUE) %>%
      ungroup()
    
    return(word_counts)
  }
  
  
  # Анализ текста и график частот слов до очистки
  observeEvent(input$start_analysis, {
    
    by_chapter_word <<- splitTextIntoChapters() %>%
      unnest_tokens(word, text)
    
    word_counts <- by_chapter_word %>%
      count(document, word, sort = TRUE) %>%
      arrange(document, desc(n)) %>%
      ungroup()
    
    output$word_counts <- renderDT({
      datatable(word_counts)
    }, options = list(pageLength = 5))
    
    # Создание списка уникальных названий документов
    unique_documents <- unique(by_chapter_word$document)
    
    # Устанавливаем список уникальных документов в selectInput
    updateSelectInput(session, "document", choices = unique_documents)
  })
  
  
  # Отображение графика
  output$word_freq_plot <- renderPlot({
    
    word_counts_data <- reactive({
      # Фильтрация данных по выбранному документу
      filtered_data <- by_chapter_word %>%
        filter(document == input$document)
      # Получение списка уникальных слов
      word_counts_unique(filtered_data)
    })
    
    # Получение выбранного количества популярных слов
    top_words <- head(word_counts_data()$word, input$num_words)
    
    # Фильтрация данных для выбранных слов
    selected_data <- word_counts_data() %>%
      filter(word %in% top_words)
    
    # Построение графика
    ggplot(selected_data, aes(x = reorder(word, n), y = n)) +
      geom_col() +
      theme_minimal() +
      labs(x = "Слово", y = "Частота", title = paste("Топ", input$num_words, "слов для", input$document))
  })
  
  
  # Очистка от стоп-слов и анализ после очистки
  observeEvent(input$clean_stop_words, {
    
    cleaned_data <<- by_chapter_word %>%
      anti_join(stop_words)
    
    clean_word_count <- cleaned_data %>%
      count(document, word, sort = TRUE) %>%
      arrange(document, desc(n)) %>%
      ungroup()
    
    
    output$clean_word_counts <- renderDT({
      datatable(clean_word_count)
    }, options = list(pageLength = 5))
    
    # Создание списка уникальных названий документов
    unique_clean_documents <- unique(cleaned_data$document)
    
    # Устанавливаем список уникальных документов в selectInput
    updateSelectInput(session, "clean_document", choices = unique_clean_documents)
  })
  
  # Отображение графика
  output$clean_word_freq_plot <- renderPlot({
    
    clean_word_counts_data <- reactive({
      # Фильтрация данных по выбранному документу
      filtered_data <- cleaned_data %>%
        filter(document == input$clean_document)
      # Получение списка уникальных слов
      word_counts_unique(filtered_data)
    })
    
    # Получение выбранного количества популярных слов
    top_words <- head(clean_word_counts_data()$word, input$clean_num_words)
    
    # Фильтрация данных для выбранных слов
    selected_data <- clean_word_counts_data() %>%
      filter(word %in% top_words)
    
    # Построение графика
    ggplot(selected_data, aes(x = reorder(word, n), y = n)) +
      geom_col() +
      theme_minimal() +
      labs(x = "Слово", y = "Частота", title = paste("Топ", input$clean_num_words, "слов для", input$clean_document))
  })
  
  
  observeEvent(input$next2, {
    updateTabItems(session, "sidebar", "dtm") 
  })
  
  
  ###############################################################################################################
  
  
  
  
  
}

shinyApp(ui, server)
