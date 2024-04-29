if (interactive()) {
  library(shiny)
  library(shinyWidgets)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(gutenbergr)
  library(topicmodels)
  library(tidytext) 
  library(tidyr) 
  library(dplyr)
  library(stringr)
  library(DT)
  library(ggplot2)
  library(plotly)
  library(lubridate)
  library(telegram.bot)
  
  source("quizFunc.R")
  
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
                  radioGroupButtons(
                    inputId = "dataDownloadType",
                    label = "Выберите способ загрузки данных:", 
                    choices = c("Загрузить свой файл", "Загрузить из сервиса Gutenberg"),
                    status = "primary"
                  ),
                  uiOutput("dataUploadUi"),
                  
                  
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
                  fluidRow(column(12, div(actionButton("toDtm", "Показать Матрицу Документ - Термин")), style="float:left")), br(),
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
                  
                  fluidRow(column(12, div(actionButton(inputId="next3", label="Перейти к проверке знаний"), style="float:left")))
          ),
          tabItem(tabName = "quiz1", h1("Проверка знаний 1")),
          tabItem(tabName = "lda_model",
                  fluidRow(
                    box(
                      title = "Настройки LDA Анализа",
                      width = 6,
                      solidHeader = TRUE,
                      numericInput("ldaTopicsCount", "Количество тем:", value = 3),
                      numericInput("ldaSeed", "Seed:", value = 123),
                      actionButton("makeLda", "Создать модель LDA")
                    )
                  ),
                  fluidRow(
                    box(
                      title = "Результаты LDA Анализа",
                      width = 6,
                      solidHeader = TRUE,
                      DTOutput("chaptersProb"),
                      DTOutput("topTermsDf"),
                      plotOutput("topTermsPlot")
                    )
                  ),
                  fluidRow(column(12, div(actionButton(inputId="next5", label="Перейти к результатам LDA"), style="float:left")))
          ),
          tabItem(tabName = "lda_result",
                  fluidRow(
                    box(title = "Гамма - вероятности", status = "primary", solidHeader = TRUE, collapsible = TRUE, DTOutput("chaptersGammaDf")),
                    box(title = "График Бокса-Вискера", status = "warning", solidHeader = TRUE, collapsible = TRUE, plotOutput("boxViskerPlot")),
                    box(title = "Классификация глав", status = "info", solidHeader = TRUE, collapsible = TRUE, DTOutput("chapterClassificationDf"))
                  )
          ),
          tabItem(tabName = "classification_errors", 
                  fluidRow(
                    box(title = "Неверно классифицированные главы", status = "danger", solidHeader = TRUE, collapsible = TRUE, DTOutput("chapterErrorClassificationDf")),
                    box(title = "Назначения слов", status = "warning", solidHeader = TRUE, collapsible = TRUE, DTOutput("assignmentsDf")),
                    box(title = "Матрица ошибок", status = "info", solidHeader = TRUE, collapsible = TRUE, plotOutput("errorMatrix")),
                    box(title = "Неверно классифицированные слова", status = "primary", solidHeader = TRUE, collapsible = TRUE, DTOutput("wrongWordsDf"))
                  )
                  
          ),
          tabItem(tabName = "quiz", 
                  fluidRow(column(12, 
                                  h3("Тест по тематическому моделированию текстов:"), 
                                  uiOutput("quizList"), hr(),
                                  
                  ))
          ),
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
      output$dataUploadUi <- renderUI({
        req(input$dataDownloadType)
        if (input$dataDownloadType == "Загрузить свой файл") {
          tagList(
            fileInput("fileInput", "Выберите текстовые файлы", multiple = TRUE),
            fluidRow(
              box(width = 12, solidHeader = TRUE, status = "primary",
                  selectInput("selectedTitle", "Выберите книгу", choices = ""), br(),
                  DTOutput("booksTable"), br(),
                  htmlOutput("bookText")
              ),
              box(
                uiOutput("splitButtons"),
                actionButton("splitChapters", "Разделить на главы"),
                selectInput("selectedChapter", "Выберите главу", choices = ""),
                htmlOutput("chapterText")
              ), br(),
            )
            
          )
        } else {
          tagList(
            fluidRow(
              box(width = 6, solidHeader = TRUE, status = "primary",
                  p("Для работы вам предлагается любые 3 книги из следующего списка: "),
                  tags$ul(
                    tags$li("Sea Wolf"),
                    tags$li("The Mysterious Island"),
                    tags$li("The Origin of the Family, Private Property and the State")
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
                uiOutput("splitButtons"),
                actionButton("splitChapters", "Разделить на главы"),
                selectInput("selectedChapter", "Выберите главу", choices = ""),
                htmlOutput("chapterText")
              ), br(),
            )
          )
        }
      })
      
      
      
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
      books <- reactive({
        if (input$dataDownloadType == "Загрузить свой файл") {
          req(input$fileInput)
          file_paths <- input$fileInput$datapath
          filenames <- input$fileInput$name
          lines_list <- lapply(file_paths, readLines)
          lines_flat <- unlist(lines_list)
          filenames_rep <- rep(filenames, lengths(lines_list))  # Создаем вектор с названиями файлов для каждой строки
          
          booksData <- data.frame(
            title = filenames_rep,
            text = lines_flat,
            stringsAsFactors = FALSE
          )
          return(booksData)
          
        } else {
          req(input$downloadBooks, input$bookIDs, input$changeMirror)
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
        }
        
      })
      
      #Обновление списка названий загруженных книг
      observe({
        req(books())
        titles <- unique(books()$title)
        updateSelectInput(session, "selectedTitle", choices = titles)
      })
      
      #Вывод текста книги в таблицу
      output$booksTable <- renderDT({
        req(books(), input$selectedTitle)
        bookSelected <- books()[books()$title == input$selectedTitle, ]
        datatable(bookSelected[, c("title", "text")], options = list(scrollX = TRUE, scrollY = "300px", pageLength = 5))
      }, server = FALSE)
      
      #Вывод текста книги
      output$bookText <- renderUI({
        req(books(), input$selectedTitle)
        bookSelected <- books()[books()$title == input$selectedTitle, ]
        book_text <- paste(bookSelected$text, collapse = "<br>")
        div(style = "overflow-y: scroll; max-height: 300px; font-size: 12px; font-family: 'Times New Roman', Times, serif;", 
            HTML(book_text)
        )
      })
      
      #Создание полей ввода для резделителя глав
      output$splitButtons <- renderUI({
        req(input$selectedTitle)
        titles <- unique(books()$title)
        buttons <- lapply(titles, function(title) {
          tagList(
            textInput(paste0("splitChapters_", title), paste("", title)),
            HTML("<br>"),
          )
        })
        do.call(tagList, buttons)
      })
      
      #Разделение текста книг на главы
      splitTextIntoChapters <- eventReactive(input$splitChapters, {
        req(input$selectedTitle)
        titles <- unique(books()$title)
        
        if (length(titles) == 0) {
          showNotification("Нет доступных книг", type = "error")
          return(NULL)
        }
        
        # Проверка наличия введенных значений в каждом textInput
        input_complete <- all(sapply(titles, function(title) {
          input_val <- input[[paste0("splitChapters_", title)]]
          !is.null(input_val) && input_val != ""
        }))
        
        if (!input_complete) {
          # Если не все значения введены, выдаем сообщение об ошибке
          showNotification("Заполните все поля", type = "error")
          return(NULL)
        }
        
        # Разделение каждой книги на главы
        chapters <- lapply(titles, function(temp) {
          book <- books() %>%
            filter(title == temp)
          
          book_chapters <- book %>%
            mutate(chapter = cumsum(str_detect(
              text, regex(input[[paste0("splitChapters_", temp)]], ignore_case = TRUE)
            ))) %>%
            filter(chapter > 0) %>%
            unite(document, title, chapter)
          
          return(book_chapters)
        })
        
        # Объединение глав из всех книг в один объект
        combined_chapters <- do.call(rbind, chapters)
        
        chaptersList <- unique(combined_chapters$document)
        updateSelectInput(session, "selectedChapter", choices = chaptersList)
        
        return(combined_chapters)
      }) 
      
      #Вывод текста выбранной главы
      output$chapterText <- renderUI({
        req(splitTextIntoChapters(), input$selectedChapter)
        chapterSelected <- splitTextIntoChapters()[splitTextIntoChapters()$document == input$selectedChapter, ]
        chapter_text <- paste(chapterSelected$text, collapse = "<br>")
        div(style = "overflow-y: scroll; max-height: 300px; font-size: 12px; font-family: 'Times New Roman', Times, serif;", 
            HTML(chapter_text)
        )
      })
      
      #Обработка кнопки "Далее"
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
        ggplot(selected_data, aes(x = reorder(word, -n), y = n)) +
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
        ggplot(selected_data, aes(x = reorder(word, -n), y = n)) +
          geom_col() +
          theme_minimal() +
          labs(x = "Слово", y = "Частота", title = paste("Топ", input$clean_num_words, "слов для", input$clean_document))
      })
      
      
      observeEvent(input$next2, {
        updateTabItems(session, "sidebarMenu", "dtm") 
      })
      
      
      #################### МАТРИЦА ДОКУМЕНТ-ТЕРМИН ####################
      chaptersDtm <- eventReactive(input$toDtm, {
        dtm <- clean_word_count %>%
          cast_dtm(document, word, n)
        return(dtm)
      })
      
      output$normal_table <- renderDT({
        req(chaptersDtm())
        datatable(clean_word_count, options = list(pageLength = 5))
      })
      
      output$dtm_table <- renderDT({
        req(chaptersDtm())
        chapters_df <- as.data.frame(as.matrix(chaptersDtm()))
        subset_matrix <- chapters_df[1:10, 1:10]
        datatable(subset_matrix, options = list(pageLength = 10, scrollX = TRUE))
      })
      
      output$dtm_content <- renderPrint({
        req(chaptersDtm())
        print(chaptersDtm())
      })
      
      
      #################### МОДЕЛЬ LDA ####################
      
      ldaChapters <- eventReactive(input$makeLda, {
        req(chaptersDtm(), input$ldaTopicsCount, input$ldaSeed)
        chapters_lda <- topicmodels::LDA(chaptersDtm(), k = input$ldaTopicsCount, control = list(seed = input$ldaSeed))
        return(chapters_lda)
      })
      
      chapterTopics <- eventReactive(input$makeLda, {
        req(ldaChapters())
        tidy(ldaChapters(), matrix = "beta")
      })
      
      output$chaptersProb <- renderDT({
        req(chapterTopics())
        datatable(chapterTopics())
      }, options = list(pageLength = 5))
      
      
      topTerms <- reactive({
        req(chapterTopics())
        top_terms <- chapterTopics() %>%
          group_by(topic) %>%
          top_n(7, beta) %>%
          ungroup() %>%
          arrange(topic, -beta)
        return(top_terms)
      })
      
      output$topTermsDf <- renderDT({
        req(topTerms())
        datatable(topTerms())
      }, options = list(pageLength = 5))
      
      
      output$topTermsPlot <- renderPlot({
        req(topTerms())
        topTerms() %>%
          mutate(term = reorder_within(term, beta, topic)) %>%
          ggplot(aes(beta, term, fill = factor(topic))) +
          geom_col(show.legend = FALSE) +
          facet_wrap(~ topic, scales = "free") +
          scale_y_reordered()
      })
      
      
      #Вероятность главы принадлежать теме (процент слов, принадлежащих теме) 
      chaptersGamma <- reactive({
        req(ldaChapters())
        chapters_gamma <- tidy(ldaChapters(), matrix = "gamma") %>%
          separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
      })
      
      output$chaptersGammaDf <- renderDT({
        req(chaptersGamma())
        datatable(chaptersGamma())
      }, options = list(pageLength = 5))
      
      #график коробка с усами
      output$boxViskerPlot <- renderPlot({
        req(chaptersGamma())
        chaptersGamma() %>%
          mutate(title = reorder(title, gamma * topic)) %>%
          ggplot(aes(factor(topic), gamma)) +
          geom_boxplot() +
          facet_wrap(~ title) +
          labs(x = "topic", y = expression(gamma))
      })
      
      
      #Найдём тему каждой главы
      chapterClassification <- reactive({
        chapter_classifications <- chaptersGamma() %>%
          group_by(title, chapter) %>%
          slice_max(gamma) %>%
          ungroup()
      })
      
      output$chapterClassificationDf <- renderDT({
        req(chapterClassification())
        datatable(chapterClassification())
      }, options = list(pageLength = 5))
      
      
      
      
      
      #################### ОШИБКИ ####################
      
      # Найдём неврено-классифицированные главы
      bookTopics <- reactive({
        req(chapterClassification())
        book_topics <- chapterClassification() %>%
          count(title, topic) %>%
          group_by(title) %>%
          top_n(1, n) %>%
          ungroup() %>%
          transmute(consensus = title, topic)
        return(book_topics)
      })
      
      
      chapterErrorClassification <- reactive({
        req(bookTopics())
        chapter_error_classification %>%
          inner_join(bookTopics(), by = "topic") %>%
          filter(title != consensus)
        return(chapter_error_classification)
      })
      
      output$chapterErrorClassificationDf <- renderDT({
        req(chapterErrorClassification())
        datatable(chapterErrorClassification())
      }, options = list(pageLength = 5))
      
      
      # Узнаем, какие слова в главах относятся к темам
      assignments <- reactive({
        req(ldaChapters(), chaptersDtm(), bookTopics())
        assignments_temp <- augment(ldaChapters(), data = chaptersDtm()) %>%
          separate(document, c("title", "chapter"), 
                   sep = "_", convert = TRUE) %>%
          inner_join(bookTopics(), by = c(".topic" = "topic"))
        return(assignments_temp)
      })
      
      
      output$assignmentsDf <- renderDT({
        req(assignments())
        datatable(assignments())
      }, options = list(pageLength = 5))
      
      
      #Построим матрицу ошибок
      output$errorMatrix <- renderPlot({
        req(assignments())
        assignments() %>%
          count(title, consensus, wt = count) %>%
          mutate(across(c(title, consensus), ~str_wrap(., 20))) %>%
          group_by(title) %>%
          mutate(percent = n / sum(n)) %>%
          ggplot(aes(consensus, title, fill = percent)) +
          geom_tile() +
          scale_fill_gradient2(high = "darkred", label = scales::percent_format()) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                panel.grid = element_blank()) +
          labs(x = "Book words were assigned to",
               y = "Book words came from",
               fill = "% of assignments") 
        
      })
      
      # неверно классифицированные слова 
      wrongWords <- reactive({
        req(assignments())
        wrong_words <- assignments() %>%
          filter(title != consensus) %>%
          count(title, consensus, term, wt = count) %>%
          ungroup() %>%
          arrange(desc(n))
        return(wrong_words)
      })
      
      output$wrongWordsDf <- renderDT({
        req(wrongWords())
        datatable(wrongWords())
      }, options = list(pageLength = 5))
      
      #################### Экзамен ####################
      # Список вопросов с ответами
      questions <- list(
        list(question = "Какая функция используется для создания вектора в R?",
             answers = c("vector()", "c()", "list()", "array()"),
             correctAnswer = "vector()"),
        list(question = "Как добавить элемент в вектор в R?",
             answers = c("append()", "push()", "concat()", "combine()"),
             correctAnswer = "push()")
      )
      
      #Токен бота
      botToken <- "6164749454:AAHbX0AlQYi4f6iHIclnUrlSJV3KSL1lLJg"
      #Список ответов пользователя
      selectedAnswers <- rep(NA, length(questions))
      #Число верных ответов пользователя
      userScore <- 0
      #Флаг завершённости тестирования
      testDone <- reactiveVal(FALSE)
      
      
      #Логика работы таймера 
      timer <- reactiveVal(1800)
      timerIsOn <- reactiveVal(FALSE)
      
      observe({
        invalidateLater(1000, session)
        isolate({
          if (timerIsOn()) {
            timer(timer() - 1)
            
            if (timer() < 1) {
              timerIsOn(FALSE)
              selectedAnswers <<- saveUserAnswers(questions, input)
              userScore <<- calculateUserScore(selectedAnswers, questions, userScore) 
              showModal(modalDialog(   
                title = "Время вышло!",
                "Ваши ответы были сохранены."
              ))
              testDone(TRUE)
              sendEndMessage(botToken, input$teacherId, input$studentName, userScore, userResults())
            }
          }
        })
      })
      
      #Вывод таймера на экран
      output$timeleft <- renderText({
        paste("Оставшееся время ", seconds_to_period(timer()))
      })
      
      
      #Обработка кнопки "Начать тест"
      observeEvent(input$start, {
        
        # Получаем имя студента и логин преподавателя из введенного текста
        teacherId <- input$teacherId
        studentName <- input$studentName
        
        # Проверяем, что пользователь ввел своё имя
        if (studentName == "") {
          showModal(modalDialog(
            title = "Ошибка",
            "Пожалуйста, введите своё имя"
          ))
          return(NULL)
        }
        
        # Проверяем, что пользователь ввел логин преподавателя
        if (teacherId == "") {
          showModal(modalDialog(
            title = "Ошибка",
            "Пожалуйста, введите логин преподавателя в Телеграме."
          ))
          return(NULL)
        }
        
        #Отправляем сообщение преподователю о начале теста
        sendStartMessage(botToken, teacherId, studentName)
        
        #Включаем таймер
        timerIsOn(TRUE)
        
      })
      
      
      
      # Обработка кнопки "Отправить ответы"
      observeEvent(input$sendAnswers, {
        selectedAnswers <<- saveUserAnswers(questions, input)
        userScore <<- calculateUserScore(selectedAnswers, questions, userScore)
        timerIsOn(FALSE)
        testDone(TRUE)
        sendEndMessage(botToken, input$teacherId, input$studentName, userScore, userResults())
      })
      
      
      #Обработка условной отрисовки интерфеса тестирования
      observeEvent(timerIsOn(), {
        if (!timerIsOn()  && testDone()) {
          output$quizList <- renderUI({
            renderQuizResult(userScore)
          })
        } else if (timerIsOn() && !testDone()) {
          output$quizList <- renderUI({
            renderQuiz(questions)
          })
        } else {
          output$quizList <- renderUI({
            renderQuizStart()
          })
        }
      })
      
      #Обработка кнопки скачивания ответов пользователя
      output$download_btn <- downloadHandler(
        filename = function() {
          paste("stats", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(userResults(), file, fileEncoding = "UTF-8")
        }
      )
      
      #Отрисовка таблицы с ответами пользователя
      output$userResult <- renderDT({
        datatable(userResults())
      })
      
      #Создание таблицы с ответами пользователя
      userResults <- reactive({
        req(testDone())
        # Создание списка из ответов пользователя
        userResponses <- lapply(seq_along(questions), function(i) {
          data.frame(
            Вопрос = questions[[i]]$question,
            ОтветПользователя = selectedAnswers[[i]],
            ПравильныйОтвет = questions[[i]]$correctAnswer,
            stringsAsFactors = FALSE
          )
        })
        # Преобразование списка в датафрейм
        userResponses <- do.call(rbind, userResponses)
        return(userResponses)
      })
      
      
    }
  )
}
