library(shiny)
library(shinydashboard)
library(gutenbergr)
library(tidytext) 
library(tidyr) 
library(dplyr)
library(stringr)
library(DT)


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
      tabItem(tabName = "text_tokenization", h1("Токенизация текста")),
      tabItem(tabName = "dtm", h1("Матрица терминов документа")),
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
  
  
  books <- eventReactive(input$downloadBooks, {
    req(input$bookIDs)
    book_ids <- as.numeric(unlist(strsplit(input$bookIDs, ",")))
    id <- showNotification("Загрузка книг...", type = "message", duration = NULL)
    # Загружаем книги
    books <- tryCatch({
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
    req(books())
    datatable(books()[, c("title", "text")], options = list(scrollX = TRUE, scrollY = "300px", pageLength = 5))
  }, server = FALSE)
  
  splitTextIntoChapters <- eventReactive(input$splitChapters, {
    req(input$chapterSplitter)
    books() %>%
      mutate(chapter = cumsum(str_detect(text, regex(input$chapterSplitter, ignore_case = input$ignoreCase)))) %>%
      filter(chapter > 0) %>%
      group_by(title) %>%
      mutate(chapter = row_number()) %>%
      ungroup() %>%
      select(title, chapter, text)
  })
  
  output$chapterTable <- renderDT({
    req(splitTextIntoChapters())
    datatable(splitTextIntoChapters(), options = list(scrollX = TRUE, scrollY = "200px", pageLength = 5), rownames = FALSE)
  }, server = FALSE)
  
  observeEvent(input$next1, {
    updateTabItems(session, "sidebar", "text_tokenization") 
  })
  
  
  
}

shinyApp(ui, server)
