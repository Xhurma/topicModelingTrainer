library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(stringr)
library(lubridate)
library(telegram.bot)
library(DT)

source("quizFunc.R")


quizUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("quizList")), 
    hr()
    
  )
  
}

quizServer <- function(id, questions) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      #Токен бота
      botToken <- Sys.getenv("TELEGRAM_BOT_TOKEN")
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
              sendEndMessage(botToken, input$teacherId, input$studentName, input$groupName, userScore, userResults())
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
        groupName <- input$groupName
        
        # Проверяем, что пользователь ввел своё имя
        if (studentName == "") {
          showModal(modalDialog(
            title = "Ошибка",
            "Пожалуйста, введите своё имя"
          ))
          return(NULL)
        }
        
        # Проверяем, что пользователь ввел логин преподавателя
        if (groupName == "") {
          showModal(modalDialog(
            title = "Ошибка",
            "Пожалуйста, введите название группы."
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
        sendEndMessage(botToken, input$teacherId, input$studentName, input$groupName, userScore, userResults())
      })
      
      
      #Обработка условной отрисовки интерфеса тестирования
      observeEvent(timerIsOn(), {
        ns <- session$ns
        if (!timerIsOn()  && testDone()) {
          output$quizList <- renderUI({
            renderQuizResult(userScore, ns)
          })
        } else if (timerIsOn() && !testDone()) {
          output$quizList <- renderUI({
            renderQuiz(questions, ns)
          })
        } else {
          output$quizList <- renderUI({
            renderQuizStart(ns)
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
            Question = questions[[i]]$question,
            UserAnswer = selectedAnswers[[i]],
            CorrectAnswer = questions[[i]]$correctAnswer,
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