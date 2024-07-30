options(shiny.maxRequestSize=30*1024^2)

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(topicmodels)
library(tidyr) 
library(dplyr)
library(ggplot2)
library(topicdoc)
library(DT)
library(tm)


ldaUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Инструкция",
        status = "info",
        solidHeader = TRUE,
        width = 6, height = "300px",
        HTML("<p>1. Ознакомьтесь с информацией</p>
                      <p>2. Выберете настройки модели LDA</p>
                           <p>3. Нажмите кнопку и дождитесь завершения построения модели</p>
                           <p>4. Переходите к следующему пункту</p>
                           <p><b>Внимание, если вы уже выполнили первое занятие в прошлой сессии, загрузите
                           DTM матрицу в блоке ниже</b></p>")
      ),
    ),
    fluidRow(
      box(
        title = "Загрузите файл с матрицей документов-терминов, если вы сохраняли его ранее",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        fileInput(ns("uploadDTM"), "Загрузить DTM")
      )
    ),
    
    fluidRow(
      box(
        title = "Что такое LDA",
        width = 6,
        status = "primary",
        solidHeader = FALSE,
        HTML("LDA (Latent Dirichlet Allocation) - это вероятностная модель тематического моделирования. 
                           Она использует два распределения Дирихле: распределение θ (тэта) и распределение β (бета).
                           Для каждого документа в коллекции LDA генерирует вектор θ, где каждая компонента указывает на вероятность принадлежности документа к определенной теме.
                          Для каждой темы модель LDA генерирует вектор β, где каждая компонента указывает на вероятность принадлежности слова к данной теме.")
      ),
      box(
        title = "Результаты LDA",
        width = 6,
        status = "primary",
        solidHeader = FALSE,
        HTML("<p>Результатом модели LDA (Latent Dirichlet Allocation) является описание тем, выявленных в текстовой коллекции, и их представление в каждом документе этой коллекции. Конкретно, результаты LDA включают:</p>
                            <ul>
                              <li>
                                <p>Матрица θ, в которой каждая строка представляет документ, а каждый столбец - вероятности принадлежности документа к каждой из тем.</p>
                                Позволяет увидеть, какие темы присутствуют в каждом документе и насколько они значимы для него.
                              </li>
                              <li>
                                <p>Матрица φ, в которой каждая строка представляет тему, а каждый столбец - вероятности вхождения слова в данную тему.</p>
                                Позволяет увидеть, какие слова характеризуют каждую тему и насколько они связаны с ней.
                              </li>
                              <li>
                                <p>Список тем, выявленных в текстовой коллекции.</p>
                                Представляет собой интерпретируемые тематические области, выявленные моделью.
                              </li>
                            </ul>
                            ")
      )
    ),
    
    fluidRow(
      box(
        title = "Настройки LDA Анализа",
        width = 6,
        status = "primary",
        solidHeader = TRUE, 
        uiOutput(ns("ldaSettings")),
        actionButton(ns("makeLda"), "Создать модель LDA", style = "background-color: purple; color: white;")
      ),
      box(
        title = "Настройки LDA Анализа",
        width = 6,
        status = "primary",
        solidHeader = FALSE,
        HTML("<p>Выбор оптимального количества тем зависит от решаемой задачи.
                           Если вы следуете предложенному сценарию работы, выберете число тем равное
                           количеству книг, которые вы загрузили в первом занятии.</p>
                           <p>Параметр &quotSeed&quot позволяет обеспечить повторяемость эксперимента и 
                          обеспечивает выпадение одних и тех же чисел при генерации случайных велчиин. Введите
                           любое число в это поле, оно вам понадобится при необходимости повторить эксперимент </p>
             <p>Параметр &quotЧисло итераций&quot. </p>
             <p>Увеличение числа итераций может улучшить сходимость модели, но требует больше времени для выполнения.</p>
             <p>Параметр &quotТип альфа&quot обеспечивает выбор гиперпараметра альфа, 
             определяющего распределение локументов по темам. По умолчанию он задается как 1/К, где K - число тем.
             Значение < 1 - документ содержит меньше тем, > 1 - документ содержит больше тем</p>
             <p>Параметр &quotТип бэта&quot аналогично альфа, но для распределения слов внутри темы</p>")
      )
    ),
    fluidRow(
      column(6,
             box(
               title = "Вероятность появления слова в теме",
               width = NULL,
               status = "primary",
               solidHeader = TRUE,
               selectInput(ns("selectedTopic"), "Выберите тему", choices = ""),
               br(),
               textOutput(ns("alphaText")), br(),
               DTOutput(ns("chaptersProb"))
             )  
      ),
      column(6,
             box(
               title = "Наиболее вероятные слова в теме",
               width = NULL,
               status = "primary",
               solidHeader = TRUE,
               uiOutput(ns("themePlot"))
               )
      )
    ),
    fluidRow(
      column(6,
             h2("Оценка модели")
      )
    ),
    fluidRow(
      column(6,
             box(
               title = "Перплексия",
               width = NULL,
               status = "primary",
               solidHeader = TRUE,
               textOutput(ns("perplexityText")), br(),
               HTML("<b>Перплексия</b> — это мера оценки качества модели. 
                             Она используется для оценки того, насколько хорошо модель предсказывает слова, 
                             встречающиеся в наборе документов.
                             Чем ниже значение перплексии, тем лучше модель считается подходящей для описания данных.
                                  ")
             )
             
      ),
      column(6,
             box(
               title = "Когерентность тем",
               width = NULL,
               status = "primary",
               solidHeader = TRUE,
               uiOutput(ns("coherenceText")), br(),
               HTML("<b>Когерентность</b> - это мера оценки качества тем.
                             Она оценивает, насколько часто слова, представляющие каждую тему, встречаются вместе в текстах. 
                             Когерентность темы определяется как средняя оценка когерентности пар слов в теме.
                             Оценка когерентности пары слов вычисляется как взаимная информация между словами, 
                             нормализованная на основе распределения слов в корпусе.
                             <p>Чем выше значения, тем более семантически связаны слова в одной теме</p>
                            <p>Низкие (отрицательные) значения - тема содержит слова, которые редко встречаются 
                            вместе в корпусе, и эти слова не обязательно семантически связаны.
                            Чем ниже отрицательное значение, тем менее семантически связаны слова в теме.</p>
                                  ")
             )
             
      )
    )
    
  )
  
}

ldaServer <- function(id, chaptersDtm) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Загрузка DTM матрицы из файла
      observeEvent(input$uploadDTM, {
        req(input$uploadDTM)
        inFile <- input$uploadDTM
        # Чтение  из загруженного файла
        temp <- read.csv(inFile$datapath, header = TRUE)
        dtm <- cast_dtm(temp, document, word, n)
        chaptersDtm(dtm)
      })
      
      #UI настроек LDA
      output$ldaSettings <- renderUI({
        ns <- session$ns
        tagList(
          numericInput(ns("ldaTopicsCount"), "Количество тем:", value = 3),
          numericInput(ns("ldaSeed"), "Seed:", value = 123),
          numericInput(ns("var"), "Итераций: ", value = 50),
          radioButtons(ns("alpha"), "Тип гиперпараметров:", c("Оценка" = "estimate", "Задать" = "fix")),
          uiOutput(ns("altSetting"))
        )
      })
      
      #UI для ввода alpha
      output$altSetting <- renderUI({
        ns <- session$ns
        tagList(
          if (input$alpha=="fix") {
            tagList(
              numericInput(ns("alphaFix"), "Значение альфа ", value = 0.5),
              numericInput(ns("betaFix"), "Значение бэта ", value = 1.5)
            )
          }
        )
      })

      #построение модели LDA
      ldaChapters <- eventReactive(input$makeLda, {
        req(chaptersDtm(), input$ldaTopicsCount, input$ldaSeed)
        id <- showNotification("Построение модели...", type = "message", duration = NULL)
          lda <- switch(input$alpha,
                        estimate = topicmodels::LDA(chaptersDtm(), k = input$ldaTopicsCount,
                                                    control = list(seed = input$ldaSeed, 
                                                    var = list(iter.max = input$var, tol = 10^-6),
                                                    em = list(iter.max = input$var, tol = 10^-4))
                        ),
                        fix = topicmodels::LDA(chaptersDtm(), k = input$ldaTopicsCount, 
                                               control = list(alpha = input$alphaFix, seed = input$ldaSeed,
                                                              var = list(iter.max = input$var, tol = 10^-6),
                                                              em = list(iter.max = input$var, tol = 10^-4)
                                                              ))
          )

        removeNotification(id)
        return(lda)
      })
      
      
      #Вероятность слов для каждой темы
      wordsBeta <- eventReactive(input$makeLda, {
        req(ldaChapters())
        beta <- tidy(ldaChapters(), matrix = "beta")
        unique(beta$topic)
        updateSelectInput(session, "selectedTopic", choices = unique(beta$topic))
        return(beta)
      })
      
      #Вывод вероятностей слов в теме
      output$chaptersProb <- renderDT({
        req(wordsBeta(), input$selectedTopic)
        filteredData <- wordsBeta()[wordsBeta()$topic == input$selectedTopic, ]
        datatable(filteredData, options = list(pageLength = 5))
      })
      
      #Выбор 10 самых вероятных слов в каждой теме
      topTerms <- reactive({
        req(wordsBeta())
        top_terms <- wordsBeta() %>%
          group_by(topic) %>%
          top_n(10, beta) %>%
          ungroup() %>%
          arrange(topic, -beta)
        return(top_terms)
      })
      
      #Вывод состава тем
      output$themePlot <- renderUI({
        ns <- session$ns
        req(topTerms())
        #округляем до большего цело числа, чтобы поолучить число строк с диаграммами
        plotHeight <- 400 * ceiling(input$ldaTopicsCount/3)
        plotOutput(ns("topTermsPlot"), height = plotHeight)
      })
      
      # Состав тем документа
      output$topTermsPlot <- renderPlot({
        req(topTerms())
        topTerms() %>%
          mutate(term = reorder_within(term, beta, topic)) %>%
          ggplot(
            aes(x = beta, y = term, fill = factor(topic))
          ) +
          geom_col(show.legend = TRUE) +
          facet_wrap(~ topic, ncol = 3, scales = "free") +
          scale_x_continuous(name = "Фи-вероятность") +
          scale_y_reordered(name = "Термин") +
          theme_minimal() +
          labs(title = "Номер темы") +
          guides(fill = guide_legend(title = "Тема"))
      })
      
      # Вычисление перплексии
      perplexityVal <- reactive({
        req(ldaChapters())
          topicmodels::perplexity(ldaChapters())
      })
      
      #Вывод перплексии
      output$perplexityText <- renderText({
        req(perplexityVal())
        paste("Значение перплексии для модели:", round(abs(perplexityVal()), digits = 2))
      })
      
      #Вычисление когерентности для каждой темы
      coherenceLda <- eventReactive(input$makeLda, {
        req(ldaChapters())
        dtm = chaptersDtm()
        coherence <- round(topic_coherence(topic_model = ldaChapters(), dtm_data = dtm, top_n_tokens = 10), digits = 2)
        return(coherence)
      })
      
      #Вывод когерентности
      output$coherenceText <- renderUI({
        req(coherenceLda())
        HTML(paste("Значение когерентности для темы:", coherenceLda(), "<br>"))
      })
      
      
      
      return(ldaChapters)
      
      
    }
  )
}