library(shiny)
library(shinyWidgets)
library(shinydashboard)

source("helpFunc.R")



helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    radioGroupButtons(
      inputId = ns("helpType"),
      label = "Открыть страницу:", 
      choices = c("Руководство пользователя", "Теория"),
      status = "primary"
    ),
    uiOutput(ns("helpPage")) 
  )
  
}

helpServer <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      observeEvent(input$helpType, {
        ns <- session$ns
        if (input$helpType == "Руководство пользователя") {
          output$helpPage <- renderUI({
            manualPageUi(ns)
          })
        } else {
          output$helpPage <- renderUI({
            theoryPageUi(ns)
          })
        }
      })
      


      
      output$downloadManual <- downloadHandler(
        filename = function() {
          "UserManual.pdf"  # Имя файла, которое будет видеть пользователь
        },
        content = function(file) {
          file.copy("www/UserManual.pdf", file)  # Путь к файлу в папке www
        }
      )
      
      output$downloadTheory <- downloadHandler(
        filename = function() {
          "TopicMidelingTheory.pdf"  # Имя файла, которое будет видеть пользователь
        },
        content = function(file) {
          file.copy("www/TopicMidelingTheory.pdf", file)  # Путь к файлу в папке www
        }
      )
      

    }
  )
}