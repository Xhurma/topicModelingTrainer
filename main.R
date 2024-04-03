library(shiny)

# Определение пользовательского интерфейса
ui <- navbarPage(
  title = "ТренеR",
  tabPanel("Теория",
           sidebarLayout(
             sidebarPanel(
               actionButton("button1", "Нажми для приветствия Мира"),
               actionButton("button2", "Нажми для приветствия Привета")
             ),
             mainPanel(
               textOutput("text")
             )
           )
  ),
  tabPanel("Практика", 
           h1("Содержимое")
  ),
  tabPanel("Проверка знаний", 
           fluidRow(
             column(12, 
                    h3("Тест по языку R:"),
                    uiOutput("questions"),
                    hr(),
                    actionButton("submit", "Отправить ответы")
             )
           )
  ),
  tabPanel("Статистика", 
           fluidRow(
             column(12, 
                    h3("Статистика верных ответов:"),
                    tableOutput("stats")
             )
           )
  )
)

# Определение серверной части
server <- function(input, output, session) {
  # Вопросы и ответы
  questions <- c(
    "Какая функция используется для создания вектора в R?",
    "Что делает функция `head()`?",
    "Чем отличается `==` от `=` в R?",
    "Какой функцией можно просмотреть структуру объекта в R?"
  )
  
  answers <- list(
    choice1 = c("c()", "rbind()", "vector()", "seq()"),
    choice2 = c("Возвращает первые 6 строк датафрейма", "Возвращает первые 5 строк датафрейма", "Возвращает последние 6 строк датафрейма", "Возвращает последние 5 строк датафрейма"),
    choice3 = c("Оператор сравнения", "Оператор присваивания", "Оба оператора идентичны", "Оба оператора разные"),
    choice4 = c("view()", "str()", "summary()", "help()")
  )
  
  correct_answers <- c(1, 2, 2, 2)
  
  # Генерация вопросов и ответов
  output$questions <- renderUI({
    question_list <- lapply(seq_along(questions), function(i) {
      list(
        h4(paste("Вопрос", i, ":")),
        p(questions[i]),
        radioButtons(inputId = paste0("answer", i), 
                     label = NULL, 
                     choices = answers[[paste0("choice", i)]])
      )
    })
    question_list
  })
  
  # Подсчет статистики верных ответов
  output$stats <- renderTable({
    user_score <- sum(sapply(seq_along(questions), function(i) {
      input_id <- paste0("answer", i)
      if (is.null(input[[input_id]])) {
        return(FALSE)
      } else {
        input[[input_id]] == correct_answers[i]
      }
    }))
    data.frame("Верные ответы" = user_score, "Всего вопросов" = length(questions))
  })
  
  # Обработчик кнопки отправки ответов
  observeEvent(input$submit, {
    user_answers <- reactiveValuesToList(input)[grepl("^answer", names(input))]
    correct_answers <<- unlist(answers)
    updateTabsetPanel(session, "tabs", "Статистика")
  })
  
  # Обработчик для первой кнопки
  observeEvent(input$button1, {
    output$text <- renderText({
      "Привет, мир!"
    })
  })
  
  # Обработчик для второй кнопки
  observeEvent(input$button2, {
    output$text <- renderText({
      "Мир, привет!"
    })
  })
}

# Запуск приложения
shinyApp(ui = ui, server = server)
