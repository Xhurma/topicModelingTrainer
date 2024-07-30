library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)


classificationUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Инструкция",
        status = "info",
        solidHeader = TRUE,
        width = 6, height = "300px",
        HTML("<p>1. Нажмите на кнопку</p>
        <p>2. Осмотрите таблицу и график</p>
                      <p>2. Ознакомьтесь с информацией</p>")
      )
    ),
    fluidRow(
      column(6, actionButton(ns("makeClass"), "Показать классификацию глав", style = "background-color: purple; color: white;"))
    ),
    fluidRow(
      box(
        title = "Принадлежность документа теме",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        selectInput(ns("document"), "Выберите документ:", choices = ""),
        DTOutput(ns("thetaDt")),
        HTML("<p><b>Это матрица с гамма-вероятностями отнесения главы chapter из выбранного документа  
                             к каждой из тем</b></p>")
        )
      ),
    fluidRow(
      box(
        title = "Наиболее вероятная тема документа",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        DTOutput(ns("chapterDt")),
        HTML("<p><b>Для каждой главы документа определена наиболее вероятная тема</b></p>")
      ),
      box(
        title = "Число глав по темам внутри документа",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        DTOutput(ns("chapterNDt")),
        HTML("<p><b>Для каждого документа подсичтано число глав n, принадлежащих теме topic</b></p>")
      )
    ),
    fluidRow(
      box(title = "Радиальная диаграмма", width = 6, status = "primary", solidHeader = TRUE,
          plotOutput(ns("radialPlot")),
          HTML("<p><b>На радиальной диаграмме представлены средние вероятности отнесения выбранной книги к каждой из тем</b></p>")
      ),
      box(title = "Диаграмма размаха ", width = 6, status = "primary", solidHeader = TRUE,
          plotOutput(ns("boxWhiskerPlot")),
          HTML("<p><b>Диаграмма размаха</b> показывает распределение вероятности документу принадлежать к каждой из тем, 
                        выделенных в результате тематического моделирования LDA. 
                        Для каждой темы строится ящик с усами, где высота ящика показывает медианное значение вероятности, 
                        а распределение значений показывает, насколько эта вероятность различается в пределах данной темы. 
                        Такая визуализация помогает исследовать, какие темы преобладают в различных главах книги и 
                        как сильно они в них выражены.</p>
                             <p>При использовании обучающего сценария вы увидите почти полное отсутствие ящика, что показывает 
                             почти полное соответствие книги своей теме</p>")
      )
    )
    
  )
  
}

classificationServer <- function(id, ldaChapters) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      observe({
        req(chaptersTheta)
        unique_documents <- unique(chaptersTheta()$title)
        updateSelectInput(session, "document", choices = unique_documents)
        
      })
      
      #Матрица вероятностей для глав 
      theta <- eventReactive(input$makeClass, {
        req(ldaChapters)
        theta_matrix <- posterior(ldaChapters())$topics
        theta_df <- as.data.frame(theta_matrix)
        theta_df <- cbind(document = rownames(theta_df), theta_df)
        rownames(theta_df) <- NULL
        theta_df <- theta_df %>%
          separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
          mutate(chapter = as.numeric(chapter))
        return(theta_df)
      })
      
      #Вывод матрицы 
      output$thetaDt <- renderDT({
        req(theta)
        theta_doc <- theta() %>%
          filter(title == input$document) %>%
          arrange(chapter)
        datatable(theta_doc, options = list(pageLength = 5))
      })
      
      #Вероятность главы принадлежать теме
      chaptersTheta <- reactive({
        req(input$makeClass)
        req(ldaChapters)
        #преобразуем в таблицу матрицу гамма вероятностей
        theta_prob <- tidy(ldaChapters(), matrix = "gamma") %>%
          separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
        return(theta_prob)
      })
      
      #Радиальная диаграмма
      output$radialPlot <- renderPlot({
        req(theta)
        theta <- theta() %>%
          filter(title == input$document)
        #Названия тем
        topics <- names(theta)[3:ncol(theta)]
        #Среднее значение по темам
        mean <- theta %>%
          select(all_of(topics)) %>%
          summarise(across(everything(), mean, na.rm = TRUE))
        #Значение в процентах
        percentage <- as.numeric(mean[1, ]) / sum(as.numeric(mean[1, ])) * 100
        data <- data.frame(
          topic = topics,
          mean_probability = as.numeric(mean[1, ]),
          label = paste0("Тема ", topics, " (", round(percentage, 1), "%)")
          )
  
        ggplot(data, aes(x = "", y = mean_probability, fill = label)) +
          geom_bar(width = 1, stat = "identity") +
          coord_polar("y", start = 0) +
          theme_minimal() +
          labs(title = "Средние значения вероятностей тем",
               x = NULL,
               y = NULL) +
          theme(axis.text.x = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank())
      })
    
      
      #Диаграмма размаха
      output$boxWhiskerPlot <- renderPlot({
        req(input$makeClass)
        req(chaptersTheta)
        chaptersTheta() %>%
          mutate(title = reorder(title, gamma * topic)) %>%
          ggplot(
            aes(x = factor(topic), y = gamma), ##на карте отображение
            facet.wrap(~ title) ##отдельный график для каждого titlr
          ) +
          geom_boxplot() + ##диаграмма размаха
          facet_wrap(~ title) +
          theme_light() +
          labs(x = "Тема", y = "Гамма-вероятность", title = "Документ")
      })
      
      
      #Тема каждой главы
      chapterClassification <- reactive({
        req(input$makeClass)
        req(chaptersTheta)
        classifications <- chaptersTheta() %>%
          group_by(title, chapter) %>%
          #Сортирует данные 
          arrange(desc(gamma)) %>%
          #Оставляем только первый самый вероятный элемент
          slice(1) %>%
          ungroup()
        return(classifications)
      })
      
      #Вывод наиболее вероятной темы
      output$chapterDt <- renderDT({
        req(chapterClassification)
        data <- chapterClassification() %>%
          filter(title == input$document) %>%
          arrange(chapter)
        datatable(data, options = list(pageLength = 10))
      })
      
      #Вывод состава документа по темам
      output$chapterNDt <- renderDT({
        req(chapterClassification)
        data <- chapterClassification() %>%
          group_by(title, topic) %>%
          summarise(n = n_distinct(chapter)) %>%
          ungroup()
        datatable(data, options = list(pageLength = 10))
      })
      
      return(chapterClassification)
 
    }
  )
}