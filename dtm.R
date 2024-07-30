library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)


dtmUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Инструкция",
        status = "info",
        solidHeader = TRUE,
        width = 6, height = "300px",
        HTML("<p>Большинство моделей тематического меоделирования текстов принимают данные в формате
                           Матрицы Терминов Документа (Document-Term Matrix)</p><p>На этом шаге Вы увидете преобразование датафрейма в матрицу
                           и узнаете о преимуществах такого способа представления данных</p><br><p><b>Нажмите кнопку 
                           и прочитайте информацию, затем можете переходить к следующему шагу</b></p>")
      )
    ),
    fluidRow(
      box(
        title = "Что такое Матрица Терминов Документа (DTM)?",
        status = "primary",
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
    fluidRow(column(12,
                    radioButtons(ns("method"), "Выберите метод взвешивания:", c("TF" = "tf", "TF-IDF" = "tfIdf")),
                    div(actionButton(ns("toDtm"), "Показать Матрицу Терминов Документа", 
                                         style = "background-color: purple; color: white;")), 
                    style="float:left")), 
    br(),
    fluidRow(
      box(
        title = "Матрица Терминов Документа",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        DTOutput(ns("dtm_table"))
      ),
      box(
        title = "Датафрейм",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        DTOutput(ns("normal_table"))
      )),
    fluidRow(
      column(width = 6,
             box(
               title = "Информация о матрице",
               width = NULL,
               status = "primary",
               solidHeader = FALSE,
               verbatimTextOutput(ns("dtm_content"))
             ),
             box(title = "Сохранение результатов", width = NULL, status = "warning", solidHeader = FALSE,
                 HTML("<b>Внимание!</b> Если вы планируете пройти второе занятие при следующем открытии приложения,
                           пожалуйста, скачайте полученные результаты к себе на ПК.
                           Вы сможете загрузить их при начале второго занятия"),
                 downloadButton(ns("downloadDTM"), "Скачать результаты")
             )
      ),
      column(width = 6,
             box(
               title = "Пояснение",
               width = NULL,
               status = "primary",
               solidHeader = FALSE,
               HTML("<p>
                      <b>documents:</b> количество документов в корпусе. Показывает, сколько текстовых документов было обработано для создания матрицы.</p>
                      <p><b>terms:</b> количество уникальных терминов (слов), которые были найдены во всех документах. Это уникальные слова, которые встречаются хотя бы в одном документе, используются для формирования столбцов матрицы.</p>
                      <p><b>Non-sparse entries:</b> показывает количество ненулевых (non-sparse) и нулевых (sparse) записей в матрице.Н улевые записи указывают на отсутствие термина в документе.</p>
                      <p><b>Sparse entries:</b> количество нулевых записей, показывающих, что термин не встречается в документе. Большое значение типично для данных текстовых анализов, так как множество слов встречаются не во всех текстах.</p>
                      <p><b>Sparsity:</b> процент нулевых записей от общего количества элементов в матрице. Показывает, насколько разреженной является матрица.  Высокий процент показывает, что большинство терминов не появляется в большинстве документов, это типично для больших текстовых наборов данных.</p>
                      <p><b>Maximal term length:</b> это максимальная длина термина (в символах) среди всех терминов, используемых в матрице.</p>
                      <p><b>Weighting:</b> описывает метод взвешивания, использованный в матрице. Term frequency (tf) — это самый базовый метод, при котором вес каждого термина в документе равен количеству его упоминаний в этом документе, без учета его важности или редкости в контексте всего корпуса.
                      </p>"
                    )
             )
      )
    ),
  )
}

dtmServer <- function(id, cleanWordCount) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      #Хранение DTM
      chaptersDtm <- reactiveVal()
      
      #Создание DTM
      observeEvent(input$toDtm, {
        req(cleanWordCount, input$toDtm)
        if (input$method == "tf") {
          dtm <- cleanWordCount() %>%
            cast_dtm(document, word, n)
        }
        else {
          dtm <- cleanWordCount() %>%
            cast_dtm(document, word, tf_idf)
        }
        chaptersDtm(dtm)
      })
      
      #Вывов таблицы токенизирвоанных данных
      output$normal_table <- renderDT({
        datatable(cleanWordCount(), options = list(pageLength = 20))
      })
      
      #Вывод DTM
      output$dtm_table <- renderDT({
        req(chaptersDtm())
        chapters_df <- as.data.frame(as.matrix(chaptersDtm()))
        subset_matrix <- chapters_df[1:10, 1:10]
        datatable(subset_matrix, options = list(pageLength = 10, scrollX = TRUE))
      })
      
      #Параметры DTM
      output$dtm_content <- renderPrint({
        req(chaptersDtm(), input$toDtm)
        print(chaptersDtm())
      })
      
      # Загрузка результата на ПК пользователся
      output$downloadDTM <- downloadHandler(
        filename = function() {
          "DTM_matrix.csv" # Название файла для скачивания
        },
        content = function(file) {
          write.csv(cleanWordCount()[c(1, 2 , 3, 6)], file, row.names = FALSE)
        }
      )
      
      return(chaptersDtm)
      
      
      
      
      
      
    }
  )
}