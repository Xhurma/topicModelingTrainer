library(shiny)
library(shinydashboard)
library(gutenbergr)
library(tidytext) 
library(tidyr) 
library(dplyr)
library(stringr)
library(DT)
library(readr)
library(data.table)

#считывание одного файла
readFile <- function(path, encoding) {
  tryCatch({
    # Попытка прочитать файл с указанной кодировкой
    file_content <- iconv(readLines(path, warn = FALSE), from = encoding, to = "UTF-8")
    return(file_content)
  }, error = function(e) {
    # Возвращаем NULL, если возникла ошибка при чтении файла
    return(NULL)
  })
}

#Загрузка txt
loadTxt <- function(datapaths, names) {
  #Считывание путей к файлам
  file_paths <- datapaths
  #Считывание имён файлов
  file_names <- names
  #Построчное считывание файлов
  lines_list <- lapply(file_paths, function(path) {
    encoding <- guess_encoding(path)
    file_content <- readFile(path, encoding$encoding[1])
    if (is.null(file_content)) {
      showModal(modalDialog(   
        title = paste("Ошибка загрузки текста!", toString(path)),
        "Загрузите .txt  файл в кодировке UTF-8"
      ))
      return(NULL)
    } else {
        return(file_content)
      }
  })
  
  lines_flat <- unlist(lines_list)
  filenames_rep <- rep(file_names, lengths(lines_list))
  
  books_data <- data.frame(
    title = filenames_rep,
    text = lines_flat,
    stringsAsFactors = FALSE
  )
  return(books_data)
}

#загрузка книг через gutenberg
loadGutenberg <- function(book_ids_str, mirror) {
  book_ids <- as.numeric(unlist(strsplit(book_ids_str, ",")))
  id <- showNotification("Загрузка книг...", type = "message", duration = NULL)
  books_data <- tryCatch({
    gutenberg_download(book_ids, meta_fields = "title", mirror = mirror)
  }, error = function(e) {
    return(NULL)
  })
  removeNotification(id)
  isAllBookDownload <- book_ids %in% books_data$gutenberg_id
  # Проверяем, успешно ли были загружены данные
  if (any(!isAllBookDownload)) {
    showModal(modalDialog(
      title = "Ошибка при загрузке книг",
      "Проверьте интернет соединение, 
            попробуйте сменить зеркало 
            или загрузите книги в ручном режиме"
      )
    )
    return(NULL)
  } else {
    return(books_data)
  }
}