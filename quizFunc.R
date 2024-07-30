library(shiny)
library(DT)
library(telegram.bot)


#Функция отправки сообщения преподователю об окончании теста
sendEndMessage <- function(bot_token, teacher_id, student_name, group_name, user_score, resultDF) {
  #Создаем временный CSV-файл
  userCsv <- tempfile(fileext = ".csv")
  write.csv(resultDF, userCsv, row.names = FALSE)  # Сохраняем данные в CSV
  
  bot <- Bot(token = bot_token)
  message <- paste(group_name, " студент ", "*", student_name, "*", " завершил тест ", 
                     "результат", user_score)    

  
  #Отправляем сообщение с прикрепленным CSV-файлом
  try(
    bot$sendDocument(chat_id = teacher_id,
                     document = userCsv,
                     caption = message,
                     parse_mode = "Markdown") 
  )
  
  #Удаляем временный файл после отправки
  unlink(userCsv)
}


#Функция отрисовывающая вопросы теста
renderQuiz <- function(questions, ns) {
  question_list <- lapply(questions, function(i) {
    temp <- radioButtons(
      input = ns(paste0("answer_", i$question)),
      label = i$question,
      choices = i$answers,
      selected = i$answers[1]
    )
    
    div(
      tagList(temp)
    )
  })
  
  tags$div(
    textOutput(ns('timeleft')),
    actionButton(ns("sendAnswers"), "Отправить ответы", style = "background-color: purple; color: white;"),
    tagList(question_list)
  )
}

#Функция, отрисовывающая интерфейс перед началом теста
renderQuizStart <- function(ns) {
  tags$div(
    textInput(ns("studentName"), "Введите ваше имя:", value = "Дмитрий Гудков"),
    textInput(ns("groupName"), "Введите вашу группу:", value = "АВТ-010"),
    textInput(ns("teacherId"), "Введите ID преподавателя в Telegram (см руководство пользователя):", value = "1332401991"),
    hr(),
    actionButton(ns("start"),"Начать тест", style = "background-color: purple; color: white;")
  )
}

#Функция, отрисовывающая интерфейс после окончания теста
renderQuizResult <- function(user_score, ns) {
  tags$div(
    HTML("Вы уже проходили это тест"),
    tags$span(HTML("Ваш счет: "), user_score),
    DTOutput(ns("userResult")),
    downloadButton(ns("download_btn"), "Скачать таблицу")
  )
}


# Функция для сохранения ответов пользователя
saveUserAnswers <- function(questions, input) {
  selected_answers <- lapply(questions, function(question) {
    input[[paste0("answer_", question$question)]]
  })
  return(selected_answers)
}

# Функция для подсчета числа правильных ответов
calculateUserScore <- function(selected_answers, questions, user_score) {
  for (i in 1:length(questions)) {
    if (selected_answers[[i]] == questions[[i]]$correctAnswer) {
      user_score <- user_score + 1
    }
  }
  return(user_score)
}