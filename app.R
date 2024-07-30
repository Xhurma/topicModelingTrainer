#
#Установка требуемых бибилотек
#Запустите приложение и установка недостающих бибилотек начнётся автоматически
#
#Список пакетов для работы приложения
requiredPackages <- c("shiny", "shinydashboard", "shinydashboardPlus", "tidyr", "tidytext", "dplyr", "stringr", 
                      "ggplot2", "DT", "gutenbergr", "readr", "data.table",
                      "topicmodels", "shinyWidgets", "lubridate", "telegram.bot", "topicdoc", "tm", "purrr")
for (package in requiredPackages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    tryCatch(
      install.packages(package),
      error = function(e) {
        print(paste("Ошибка установки пакета", package, ": ", e$message))
      }
    )
  }
}

# Подключение библиотек
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

#Подключение модулей 
source("introduction.R")
source("dataDownloadAndMarkup.R")
source("tokenization.R")
source("dtm.R")
source("lda.R")
source("classification.R")
source("errors.R")
source("quiz.R")
source("help.R")


  ui = dashboardPage(
    header = dashboardHeader(title = "TopicModeling", titleWidth = 270),
    sidebar = dashboardSidebar(id = "sidebar",
                               width = 270,
                               sidebarMenu(id = "sidebarMenu",
                                           menuItem("Введение", tabName = "introduction", icon = icon("book")),
                                           
                                           menuItem("Занятие 1", tabName = "practice1", icon = icon("pencil"), 
                                                    menuSubItem("Загрузка и разметка данных", tabName = "data_loading"),
                                                    menuSubItem("Токенизация текста", tabName = "text_tokenization"),
                                                    menuSubItem("Матрица терминов документа", tabName = "dtm")
                                                    
                                           ),
                                           menuItem("Занятие 2", tabName = "practice2", icon = icon("pencil"), 
                                                    menuSubItem("Модель LDA", tabName = "lda_model"),
                                                    menuSubItem("Результаты классификации", tabName = "lda_result"),
                                                    menuSubItem("Ошибки классификации", tabName = "classification_errors")
                                                    
                                           ),
                                           menuItem("Экзамен", tabName = "quiz", icon = icon("question")),
                                           
                                           menuItem("Помощь", tabName = "help", icon = icon("question-circle"))
                                           
                               )
    ),
    body = dashboardBody(
      tabItems(
        tabItem(tabName = "introduction",
                introductionUI("introduction1"),
                fluidRow(
                  column(
                    6,
                    div(
                      actionButton(inputId="toLeson1", 
                                   label="Приступить к первому занятию", 
                                   style = "background-color: green; color: white;"), 
                      style="margin-right: 10px;"
                    )
                  ),
                  column(
                    6,
                    div(
                      actionButton(inputId="toHelp", 
                                   label="Перейти на страницу помощи", 
                                   style = "background-color: blue; color: white;"), 
                      style="margin-left: 10px;"
                    )
                  )
                )
        ),
        tabItem(tabName = "practice1", h1("")),
        tabItem(tabName = "lesson1", h1("Первичная обработка данных")),
        tabItem(tabName = "data_loading",
                dataDownloadAndMarkupUI("dataDownloadAndMarkup1"),
                fluidRow(column(12, div(actionButton(inputId = "next1", 
                                                     label="Далее", 
                                                     style = "background-color: green; color: white;"), 
                                        style="float:right")))
              
                
        ),
        tabItem(tabName = "text_tokenization", h1("Токенизация текста"), 
                tokenizationUI("tokenization1"),
                fluidRow(column(12, div(actionButton(inputId="next2", label="Далее", style = "background-color: green; color: white;"), style="float:right")))
                
        ),
        tabItem(tabName = "dtm", h1("Матрица терминов документа"),
                dtmUI("dtm1"),
                
                fluidRow(column(12, div(actionButton(inputId="next3", label="Перейти ко второму занятию", style = "background-color: green; color: white;"), style="float:right")))
        ),
        tabItem(tabName = "lda_model", h1("Модель LDA"),
                ldaUI("lda1"),
                fluidRow(column(12, div(actionButton(inputId="next4", label="Перейти к результатам классификации глав", style = "background-color: green; color: white;"), style="float:right")))
        ),
        tabItem(tabName = "lda_result", h1("Результат классификации глав"),
                classificationUI("classification1"),
                fluidRow(column(12, div(actionButton(inputId="next5", label="Перейти к ошибкам классификации", style = "background-color: green; color: white;"), style="float:right")))
        ),
        tabItem(tabName = "classification_errors", h1("Ошибки классификации глав и слов"),
              errorsUI("errors1"),
              fluidRow(column(12, div(actionButton(inputId="toQuiz", label="Перейти к экзамену", style = "background-color: green; color: white;"), style="float:right")))
                
        ),
        tabItem(tabName = "quiz", 
                fluidRow(column(12, h3("Тест по тематическому моделированию текстов:"), 
                                quizUI("quiz1")
                                
                ))
        ),
        tabItem(tabName = "help", h1("Руководство пользователя"),
                helpUI("help1")
                
        )
      )
      
    ),
    footer = dashboardFooter(
      left = tags$a(href="https://github.com/Xhurma/rshiny", "Проект на GitHub"),
      right = "НГТУ НЭТИ"
    )
  )
  
  server <- function(input, output, session) {
    #################### Введение ####################
    introductionServer("introduction1")
    #Обработка кнопки "К первому занятию"
    observeEvent(input$toLeson1, {
      updateTabItems(session, "sidebarMenu", selected = "data_loading") 
    })
    #Обработка кнопки "На сттраницу помощи"
    observeEvent(input$toHelp, {
      updateTabItems(session, "sidebarMenu", selected = "help") 
    })
    
    #################### ЗАГРУЗКА ДАННЫХ ####################
    textByChapters <- dataDownloadAndMarkupServer("dataDownloadAndMarkup1")
    
    #Обработка кнопки "Далее"
    observeEvent(input$next1, {
      updateTabItems(session, "sidebarMenu", selected = "text_tokenization") 
    })
    
    #################### ТОКЕНИЗАЦИЯ ДАННЫХ ####################
    
    cleanWordCount <- tokenizationServer("tokenization1", textByChapters)
    
    
    observeEvent(input$next2, {
      updateTabItems(session, "sidebarMenu", "dtm") 
    })
    
    
    #################### МАТРИЦА ДОКУМЕНТ-ТЕРМИН ####################
    chaptersDtm <- dtmServer("dtm1", cleanWordCount)
    
    observeEvent(input$next3, {
      updateTabItems(session, "sidebarMenu", "lda_model") 
    })
    
    
    #################### МОДЕЛЬ LDA ####################
    ldaChapters <- ldaServer("lda1", chaptersDtm)
    
    
    observeEvent(input$next4, {
      updateTabItems(session, "sidebarMenu", "lda_result") 
    })
    

    #################### РЕЗУЛЬТАТ КЛАССИФИКАЦИИ ГЛАВ####################
    chapterClassification <- classificationServer("classification1", ldaChapters)
    
    observeEvent(input$next5, {
      updateTabItems(session, "sidebarMenu", "classification_errors") 
    })
    
    
    #################### ОШИБКИ ####################
    errorsServer("errors1", chapterClassification, ldaChapters, chaptersDtm)
    
    observeEvent(input$toQuiz, {
      updateTabItems(session, "sidebarMenu", "quiz") 
    })
    
    
    #################### Экзамен ####################
    # Список вопросов с ответами
    questions1 <- list(
      list(
        question = "Какая цель предварительной обработки данных при работе с тематическим моделированием?",
        answers = c("Уменьшить размерность данных", "Очистить данные от шума", "Преобразовать данные в числовой формат", "Все вышеперечисленное"),
        correctAnswer = "Все вышеперечисленное"
      ),
      list(
        question = "Какой метод обычно используется для преобразования текстовых данных в числовой формат перед тематическим моделированием?",
        answers = c("Метод главных компонент", "Метод TF-IDF", "Метод k-means", "Метод логистической регрессии"),
        correctAnswer = "Метод TF-IDF"
      ),
      list(
        question = "Как называется метод, используемый для выделения тематик из текстовых данных?",
        answers = c("Кластеризация", "Классификация", "Тематическое моделирование", "Регрессионный анализ"),
        correctAnswer = "Тематическое моделирование"
      ),
      list(
        question = "Какой алгоритм является наиболее популярным для тематического моделирования?",
        answers = c("K-means", "SVM (Support Vector Machine)", "LDA (Latent Dirichlet Allocation)", "PCA (Principal Component Analysis)"),
        correctAnswer = "LDA (Latent Dirichlet Allocation)"
      ),
      list(
        question = "Какая метрика часто используется для оценки качества тематической модели?",
        answers = c("Accuracy", "Precision", "Perplexity", "ROC-AUC (Receiver Operating Characteristic - Area Under Curve)"),
        correctAnswer = "Perplexity"
      ),
      list(
        question = "Какие методы обычно используются для очистки текстовых данных перед тематическим моделированием?",
        answers = c("Токенизация", "Удаление стоп-слов", "Лемматизация", "Все вышеперечисленное"),
        correctAnswer = "Все вышеперечисленное"
      ),
      list(
        question = "Что такое TF-IDF?",
        answers = c("Метод оценки важности слова в документе относительно корпуса текстов", "Метод машинного обучения для классификации текста", "Метод для поиска аномалий в текстовых данных", "Метод сжатия текстовых данных"),
        correctAnswer = "Метод оценки важности слова в документе относительно корпуса текстов"
      ),
      list(
        question = "Что представляют собой темы в тематическом моделировании?",
        answers = c("Группы слов, связанных с одной темой", "Категории документов", "Оценки важности слов", "Атрибуты документов"),
        correctAnswer = "Группы слов, связанных с одной темой"
      ),
      list(
        question = "Какой метод используется для извлечения ключевых слов из текста?",
        answers = c("TF-IDF", "LDA", "N-граммы", "Word2Vec"),
        correctAnswer = "TF-IDF"
      ),
      list(
        question = "Какие метрики обычно используются для оценки качества тематических моделей?",
        answers = c("Coherence Score", "Silhouette Score", "Intracluster Variance", "Euclidean Distance"),
        correctAnswer = "Coherence Score"
      )
    )
    
    
    quizServer("quiz1", questions = questions1)
    
    
    
    #################Руководство пользователя#############
    helpServer("help1")


  }


shinyApp(ui = ui, server = server)
