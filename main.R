if (interactive()) {
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(gutenbergr)
  library(tidytext) 
  library(tidyr) 
  library(dplyr)
  library(stringr)
  library(DT)
  library(ggplot2)
  library(plotly)
  
  shinyApp(
    ui = dashboardPage(
      header = dashboardHeader(title = "TopicModeling", titleWidth = 270),
      sidebar = dashboardSidebar(id = "sidebar",
                                 width = 270,
                                 sidebarMenu(id = "sidebarMenu",
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
      body = dashboardBody(
        tabItems(
          tabItem(tabName = "theory", sidebarLayout(sidebarPanel(actionButton("button1", "Нажми для приветствия Мира"), actionButton("button2", "Нажми для приветствия Привета")), mainPanel(textOutput("text")))),
          tabItem(tabName = "practice1", h1("Содержимое практики")),
          tabItem(tabName = "lesson1", h1("Первичная обработка данных")),
          tabItem(tabName = "data_loading", h1("Загрузка и осмотр данных"),
                  fluidRow(
                    box(width = 6, solidHeader = TRUE, status = "primary",
                        p("Для работы вам предлагается любые 3 книги из следующего списка: "),
                        tags$ul(
                          tags$li("Sea Wolf"),
                          tags$li("The Mysterious Island"),
                          tags$li("")
                        ),
                        p("Загрузите библиотеку книг и попробуйте найти id этих книг с помощью кнопки поиска")
                        
                    ), br(), br()
                  ),
                  fluidRow(
                    box(width = 10, solidHeader = TRUE, status = "primary",
                        actionButton("viewLibrary", "Просмотреть библиотеку Gutenberg"),
                        DTOutput("libraryTable", height = "300px")),
                    box(title = "Загрузка текста", width = 2, solidHeader = TRUE, status = "primary",
                        uiOutput("displayText"),
                        actionButton("data_loading_info1", "1"),
                        actionButton("data_loading_info2", "2")), 
                    br(), br()
                  ),
                  fluidRow(
                    box(width = 12, solidHeader = TRUE, status = "primary",
                        textInput("bookIDs", "Введите ID книг для загрузки (например, 1268, 33111, 1074):"),
                        p("Если возникла ошибка, попробуйте сменить зеркало"),
                        selectInput("changeMirror", "Выберите зеркало:", choices = c("http://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/",
                                                                                     "http://eremita.di.uminho.pt/gutenberg/",
                                                                                     "http://mirror.csclub.uwaterloo.ca/gutenberg/",
                                                                                     "https://gutenberg.nabasny.com/",
                                                                                     "https://www.gutenberg.org/dirs/",
                                                                                     "https://mirror2.sandyriver.net/pub/gutenberg"
                        ), 
                        selected = "http://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/"),
                        actionButton("downloadBooks", "Скачать книги"),
                        selectInput("selectedTitle", "Выберите книгу", choices = ""), br(),
                        DTOutput("booksTable"), br(),
                        htmlOutput("bookText")
                    ),
                    box(
                      textInput("chapterSplitter", "Разделить на главы:", value = "^Chapter "),
                      checkboxInput("ignoreCase", "Игнорировать регистр", value = TRUE),
                      actionButton("splitChapters", "Разделить на главы"), br(),
                      selectInput("selectedChapter", "Выберите главу", choices = ""), br(),
                      DTOutput("chapterTable"),
                      htmlOutput("chapterText")
                    ), br(),
                    
                    
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
          tabItem(tabName = "dtm", h1("Матрица терминов документа"),
                  fluidRow(
                    box(
                      title = "Что такое матрица терминов документа (DTM)?",
                      status = "info",
                      solidHeader = TRUE,
                      width = 4, height = "300px",
                      tags$ul(
                        tags$li("Это матрица, в которой строки представляют термины (слова), а столбцы - документы."),
                        tags$li("Элементы матрицы показывают частоту появления каждого термина в каждом документе."),
                        tags$li("DTM широко используется в анализе текста для моделирования документов и извлечения признаков.")
                      )
                    ),
                    box(
                      title = "Чем удобна DTM?",
                      status = "success",
                      solidHeader = TRUE,
                      width = 4, height = "300px",
                      tags$ul(
                        tags$li("Показывает частоту слов в каждом документе"),
                        tags$li("Позволяет использовать методы машинного обучения для анализа текста и классификации документов."),
                        tags$li("Эффективна для обработки компьютером, т.к. операции с матрицами производятся быстро.")
                      )
                    ),
                    box(
                      title = "Чем неудобна DTM?",
                      status = "danger",
                      solidHeader = TRUE,
                      width = 4, height = "300px",
                      tags$ul(
                        tags$li("Низкая наглядность - матрица может состоять из тысяч строк и столбцов, вывести такую матрицу на экран и сохранить информативность практически невозможно"),
                        tags$li("Неудобна для человека - матрица является разряженной и содержит свыше 90% нулей, что делает её громоздкой для обработки человеком"),
                      )
                    )
                  ),
                  fluidRow(column(12, div(actionButton("to_dtm", "Показать Матрицу Документ - Термин")), style="float:left")), br(),
                  fluidRow(
                    box(
                      title = "Обычная таблица",
                      width = 6,
                      status = "primary",
                      solidHeader = TRUE,
                      DTOutput("normal_table")
                    ),
                    box(
                      title = "Document-Term Matrix",
                      width = 6,
                      status = "warning",
                      solidHeader = TRUE,
                      DTOutput("dtm_table")
                    )),
                  fluidRow(
                    box(
                      title = "Информация о матрице",
                      width = 6,
                      status = "primary",
                      solidHeader = TRUE,
                      verbatimTextOutput("dtm_content")
                    ),
                    box(
                      width = 6,
                      status = "primary",
                      solidHeader = TRUE),
                  ),
                  DTOutput("dtm_table2"),
                  fluidRow(column(12, div(actionButton(inputId="next3", label="Перейти к проверке знаний"), style="float:left")))
          ),
          
          
          
          tabItem(tabName = "quiz1", h1("Проверка знаний 1")),
          tabItem(tabName = "quiz", fluidRow(column(12, h3("Тест по языку R:"), uiOutput("questions"), hr(), actionButton("submit", "Отправить ответы")))),
          tabItem(tabName = "stats", fluidRow(column(12, h3("Статистика верных ответов:"), tableOutput("stats")))),
          tabItem(tabName = "help", h1("Помощь"), p("Здесь будет страница помощи.")),
          tabItem(tabName = "feedback", h1("Отзыв"), p("Здесь будет страница отзыва."))
        )
        
      ),
      footer = dashboardFooter(
        left = tags$a(href="https://github.com/Xhurma/rshiny", "Проект на GitHub"),
        right = "НГТУ НЭТИ"
      )
    ),
    
    
    server <- function(input, output, session) {
      #################### ЗАГРУЗКА ДАННЫХ ####################
      ## ТЕКСТОВЫЕ БЛОКИ
      #блок 1
      text_data <- c(
        "<p>Для работы предлагается загрузить несколько книг из свободной библиотеки Gutenberg.</p>
    <p>Все тексты доступны для загрузки на английском языке</p>",
        "<p>Проект \"Гутенберг\"— это общественная некоммерческая инициатива, которая занимается созданием и распространением цифровой коллекции произведений, находящихся в общественном достоянии.</p>
                  <p>Проект считается старейшей в мире электронной библиотекой. Большинство работ было оцифровано волонтёрами и доступно для свободного скачивания.</p>
                  <p>На 2021 год в коллекции проекта более 60 000 книг.</p>"
      )
      
      current_text <- reactiveValues(text = text_data[1])
      
      observeEvent(input$data_loading_info1, {
        current_text$text <- text_data[1]
      })
      
      observeEvent(input$data_loading_info2, {
        current_text$text <- text_data[2]
      })
      
      output$displayText <- renderUI({
        HTML(current_text$text)
      })
      
      ##ЛОГИКА ДАННЫХ
      #Загрузка библиотеки
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
      
      
      #Закачка книг
      books <- eventReactive(input$downloadBooks, {
        req(input$bookIDs, input$changeMirror)
        book_ids <- as.numeric(unlist(strsplit(input$bookIDs, ",")))
        id <- showNotification("Загрузка книг...", type = "message", duration = NULL)
        booksData <- tryCatch({
          gutenberg_download(book_ids, meta_fields = "title", mirror = input$changeMirror)
        }, error = function(e) {
          NULL
        })
        removeNotification(id)
        # Проверяем, успешно ли были загружены данные
        if (is.null(booksData)) {
          showNotification("Ошибка при загрузке книг.", type = "error")
        }
        return(booksData)
      })
      
      observe({
        req(books())
        titles <- unique(books()$title)
        updateSelectInput(session, "selectedTitle", choices = titles)
      })
      
      
      output$booksTable <- renderDT({
        req(books(), input$selectedTitle)
        bookSelected <- books()[books()$title == input$selectedTitle, ]
        datatable(bookSelected[, c("title", "text")], options = list(scrollX = TRUE, scrollY = "300px", pageLength = 5))
      }, server = FALSE)
      
      
      output$bookText <- renderUI({
        req(books(), input$selectedTitle)
        bookSelected <- books()[books()$title == input$selectedTitle, ]
        book_text <- paste(bookSelected$text, collapse = "<br>")
        div(style = "overflow-y: scroll; max-height: 300px; font-size: 12px; font-family: 'Times New Roman', Times, serif;", 
            HTML(book_text)
        )
      })
      
      
      
      
      splitTextIntoChapters <- eventReactive(input$splitChapters, {
        req(input$chapterSplitter)
        books() %>% group_by(title) %>%
          mutate(chapter = cumsum(str_detect(
            text, regex("^chapter ", ignore_case = TRUE)
          ))) %>%
          ungroup() %>%
          filter(chapter > 0) %>%
          unite(document, title, chapter)
      })
      
      observe({
        req(splitTextIntoChapters())
        chapters <- unique(splitTextIntoChapters()$document)
        updateSelectInput(session, "selectedChapter", choices = chapters)
      })
      
      output$chapterTable <- renderDT({
        req(splitTextIntoChapters())
        datatable(splitTextIntoChapters()[, "text"], options = list(scrollX = TRUE, scrollY = "200px", pageLength = 5), rownames = FALSE)
      }, server = FALSE)
      
      output$chapterText <- renderUI({
        req(splitTextIntoChapters(), input$selectedTitle)
        chapterSelected <- splitTextIntoChapters()[splitTextIntoChapters()$document == input$selectedChapter, ]
        chapter_text <- paste(chapterSelected$text, collapse = "<br>")
        div(style = "overflow-y: scroll; max-height: 300px; font-size: 12px; font-family: 'Times New Roman', Times, serif;", 
            HTML(chapter_text)
        )
      })
      
      observeEvent(input$next1, {
        updateTabItems(session, "sidebarMenu", selected = "text_tokenization") 
      })
      
      #################### ТОКЕНИЗАЦИЯ ДАННЫХ ####################
      
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
        
        clean_word_count <<- cleaned_data %>%
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
        updateTabItems(session, "sidebarMenu", "dtm") 
      })
      
      
      #################### МАТРИЦА ДОКУМЕНТ-ТЕРМИН ####################
      observeEvent(input$to_dtm, {
        
        output$normal_table <- renderDT({
          datatable(clean_word_count)
        }, options = list(pageLength = 5))
        
        chapters_dtm <- clean_word_count %>%
          cast_dtm(document, word, n)
        
        chapters_df <- as.data.frame(as.matrix(chapters_dtm))
        
        output$dtm_table <- renderDT({
          subset_matrix <- chapters_df[1:10, 1:10]
          datatable(subset_matrix, options = list(pageLength = 10, scrollX = TRUE))
        })
        
        chapters_df2 <- tidy(chapters_dtm)
        
        output$dtm_table2 <- renderDT({
          subset_matrix <- chapters_df[1:10, 1:10]
          datatable(chapters_df2, options = list(pageLength = 10, scrollX = TRUE))
        })
        
        
        
        
        output$dtm_content <- renderPrint({
          print(chapters_dtm)
        })
        
        
        
        
        
      })
      
      observeEvent(input$next3, {
        updateTabItems(session, "sidebarMenu", "quiz1") 
      })
      
      
      
      
      
    }
  )
}
