library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(gutenbergr)
library(tidytext) 
library(tidyr) 
library(dplyr)
library(stringr)
library(DT)
library(readr)
library(data.table)

source("dataDownloadFunc.R")

dataDownloadAndMarkupUI <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Загрузка и разметка данных"),
    fluidRow(
      box(title = "Инструкция", status = "info", solidHeader = TRUE, width = 8, 
          HTML("<p>1. Выберете способ загрузки исходных данных и следуйте дальнейшим указаниям.</p>
                        <p>2. Укажите разделитель для каждого документа</p>
                    <p>3. Перейдите к следующему шагу</p>
                    <p><b>Для прохождения обучающего сценария выберете загрузку через Gutenberg</b></p>")
      ) 
    ),
    fluidRow(
      column(width = 6,
             radioGroupButtons(
               inputId = ns("dataDownloadType"),
               label = "Выберите способ загрузки данных:", 
               choices = c("Загрузить свой файл", "Загрузить из сервиса Gutenberg"),
               status = "primary"
             )
      )
    ),
    uiOutput(ns("dataUploadUi")),
    fluidRow(
      box(width = 6, solidHeader = TRUE, status = "primary", title = "Список загруженных книг",
          selectInput(ns("selectedTitle"), "Выберите книгу", choices = ""), br(),
          HTML("<p>Вы можете <strong>отредактировать</strong> текст книги.</p>
                       <p>Для этого сделайте двойной клик по строке</p>
                       <p>Для завершения редактирования кликните вне таблицы</p>"),
          DTOutput(ns("booksTable")), br()
      ),
      box(width = 6, solidHeader = TRUE, status = "primary", title = "Текст выбранной книги",
          htmlOutput(ns("bookText"))
      )
    ),
    fluidRow(
      box(width = 8, solidHeader = TRUE, status = "primary", title = "Разделите книги на главы",
          uiOutput(ns("splitButtons")), br(),
          actionButton(ns("splitChapters"), "Разделить на главы", style = "background-color: purple; color: white;"), br(),
          selectInput(ns("selectedChapter"), "Выберите главу", choices = ""), 
          htmlOutput(ns("chapterText"))
      ),
      box(width = 4, solidHeader = FALSE, status = "info", title = "Инструкция по разделителю",
          HTML("<p>Разделитель - это уникальная последовательность символов, позволяющая разделить
                  текст на несколько частей. Для книги это обычно слово &quotГлава&quot или &quotChapter&quot.</p>
                  <p>Осмотрите текст книг на наличие такого разделителя и введите его в соответствующее поле.</p>
                  <p>Если такого разделителя в книге нет, отредактивруйте её текст в блоке выше</p>
                  
                  <p>Вы можете изменять разделитель и смотреть получившиеся результаты.
                  Как только текст будет разделен на нужные Вам части, переходите к следующему уроку</p>
                       ")
      )
    ),
    br()
  )

}

dataDownloadAndMarkupServer <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      output$dataUploadUi <- renderUI({
        ns <- session$ns
        req(input$dataDownloadType)
        if (input$dataDownloadType == "Загрузить свой файл") {
          tagList(
            fluidRow(
              box(width = 6, solidHeader = TRUE, status = "primary", title = "Выберите текстовые файлы",
                  fileInput(ns("fileInput"), "", multiple = TRUE)
              ),
              box(width = 6, solidHeader = FALSE, status = "primary", title = "Перед загрузкой файлов",
                  HTML("<p>Вы можете выбрать один или несколько файлов в формате .txt, рекомендуется кодировка UTF-8.
                  Для выбора нескольких файлов кликайте при нажатой клавише ctrl</p>
                  
                     <p>Для работы предлагается разбить текст для анализа на части (например главы),
                     позаботьтесь о том, чтобы каждая часть начиналась с одного и того же
                     разделителя (например &quotГЛАВА&quot), если его нет, отредактируйте
                     текст исходных файлов заранее и добавьте его в ручную. Можете
                     использовать уникальную последовательность букв или цифр для разделителя (такую как &quot4321&quot)
                       Также вы можете отредактировать текст и после загрузки файлов</p>")
              )
            )
          )
        } else {
          tagList(
            fluidRow(
              box(width = 6, solidHeader = TRUE, status = "info", title = "Загрузка книг",
                  p("Для работы вам предлагается загрузить любые книги из следующего списка: "),
                  tags$ul(
                    tags$li("Sea Wolf"),
                    tags$li("The Mysterious Island"),
                    tags$li("Pride and Prejudice"),
                    tags$li("The Origin of the Family, Private Property and the State")
                  ),
                  p("Загрузите библиотеку книг и попробуйте найти id этих книг с помощью поля поиска")
                  
              ), br(), br()
            ),
            fluidRow(
              box(width = 10, solidHeader = TRUE, status = "primary", title = "Осмотрите библиотеку Getenberg",
                  actionButton(ns("viewrequire"), "Просмотреть библиотеку Gutenberg", style = "background-color: purple; color: white;"),
                  br(),
                  p(strong("Для навигации используйте поле поиска")),
                  DTOutput(ns("requireTable"), height = "300px")),
              tabBox(
                side = "right",
                title = "Загрузка текста", width = 2,
                tabPanel("1", 
                         HTML("<p>Для работы предлагается загрузить несколько книг из свободной библиотеки Gutenberg.</p>
    <p>Все тексты доступны для загрузки на английском языке</p>")
                ),
                tabPanel("2", HTML("<p>Проект \"Гутенберг\"— это общественная некоммерческая инициатива, которая занимается созданием и распространением цифровой коллекции произведений, находящихся в общественном достоянии.</p>
                  <p>Проект считается старейшей в мире электронной библиотекой. Большинство работ было оцифровано волонтёрами и доступно для свободного скачивания.</p>
                  <p>На 2021 год в коллекции проекта более 60 000 книг.</p>")
                )
              ),
              br(), br()
            ),
            fluidRow(
              box(width = 10, solidHeader = TRUE, status = "primary", title = "Загрузите выбранные книги",
                  textInput(ns("bookIDs"), "Введите ID книг для загрузки (например, 1268, 42671, 1074, 33111):"),
                  p("Если возникла ошибка, попробуйте сменить зеркало"),
                  selectInput(ns("changeMirror"), "Выберите зеркало:", choices = c("http://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/",
                                                                               "http://eremita.di.uminho.pt/gutenberg/",
                                                                               "http://mirror.csclub.uwaterloo.ca/gutenberg/",
                                                                               "https://gutenberg.nabasny.com/",
                                                                               "https://www.gutenberg.org/dirs/",
                                                                               "https://mirror2.sandyriver.net/pub/gutenberg"), 
                  selected = "http://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/"),
                  actionButton(ns("downloadBooks"), "Скачать книги", style = "background-color: purple; color: white;")
                  )
              )
          )
        }
      })
      
      
      #Загрузка библиотеки
      requireTable <- eventReactive(input$viewrequire, {
        tryCatch({
          gutenberg_metadata
        }, error = function(e) {
          NULL
        })
      })
      
      #Вывод библиотеки 
      output$requireTable <- renderDT({
        requireData <- requireTable()
        if (is.null(requireData)) {
          showNotification("Ошибка: Не удалось загрузить библиотеку, проверьте интернет соединение
                           или загрузите книги в ручном режиме", type = "error")
          return(NULL)
        }
        datatable(requireData, options = list(scrollX = TRUE, scrollY = "300px", pageLength = 10), class = 'cell-border stripe')
      })
      
      #Хранение книг
      books <- reactiveVal()
      
      #Закачка книг
      observe({
        if (input$dataDownloadType == "Загрузить свой файл") {
          req(input$fileInput)
          booksData <- loadTxt(input$fileInput$datapath, input$fileInput$name)
          books(booksData)
        } else {
          req(input$downloadBooks, input$bookIDs, input$changeMirror)
          booksData <- loadGutenberg(input$bookIDs, input$changeMirror)
          books(booksData)
        }
      })
      
      #Обновление списка названий загруженных книг
      observe({
        req(books())
        titles <- unique(books()$title)
        updateSelectInput(session, "selectedTitle", choices = titles)
      })
      
      #Обработка изменения таблицы с книгой
      observeEvent(input$booksTable_cell_edit, {
        req(books())
        temp <- books()
        info <- input$booksTable_cell_edit
        
        # Если ячейка была отредактирована
        if (!is.null(info)) {
          # Получаем индекс строки, название столбца и данные, которые был отредактированы
          row <- info$row
          col <- info$col
          value <- info$value
          temp[row, col] <- value
          # Обновляем books() с учетом внесенных изменений
          try(
            books(temp)
          )
        }
      })
      
      #Вывод текста книги в таблицу
      output$booksTable <- renderDT({
        req(books(), input$selectedTitle)
        bookSelected <- books()[books()$title == input$selectedTitle, ]
        datatable(bookSelected[, c("title", "text")], 
                  editable = TRUE, options = list(scrollX = TRUE, scrollY = "300px", 
                                                  pageLength = 5), 
                  class = 'cell-border stripe'
                  )
      })
      
      #Вывод текста книги
      output$bookText <- renderUI({
        req(books(), input$selectedTitle)
        bookSelected <- books()[books()$title == input$selectedTitle, ]
        book_text <- paste(bookSelected$text, collapse = "<br>")
        div(style = "overflow-y: scroll; max-height: 500px; font-size: 14px; font-family: 'Times New Roman', Times, serif;", 
            HTML(book_text)
        )
      })
      
      #Создание полей ввода для резделителя глав
      output$splitButtons <- renderUI({
        ns <- session$ns
        req(input$selectedTitle)
        titles <- unique(books()$title)
        buttons <- lapply(titles, function(title) {
          tagList(
            textInput(ns(paste0("splitChapters_", title)), paste("", title)),
            HTML("<br>"),
          )
        })
        do.call(tagList, buttons)
      })
      
      #Разделение текста книг на главы
      textByChapters <- eventReactive(input$splitChapters, {
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
          
        })
        # Объединение глав из всех книг в один объект
        combine_chapters <- do.call(rbind, chapters)
        
        chaptersList <- unique(combine_chapters$document)
        updateSelectInput(session, "selectedChapter", choices = chaptersList)
        return(combine_chapters)
      }) 
      
      #Вывод текста выбранной главы
      output$chapterText <- renderUI({
        req(textByChapters(), input$selectedChapter)
        chapterSelected <- textByChapters()[textByChapters()$document == input$selectedChapter, ]
        chapter_text <- paste(chapterSelected$text, collapse = "<br>")
        div(style = "overflow-y: scroll; max-height: 500px; font-size: 14px; font-family: 'Times New Roman', Times, serif;", 
            HTML(chapter_text)
        )
      })
      
      
      return(textByChapters)
    }
  )
}