#' Get sentiment from a text
#'
#' @aliases get_emotion
#'
#' @description
#' For a text written in a given language, this function extracts the sentiment 
#' it conveys. Returned labels are "anger", "joy", "love", "sadness" and 
#' unrecognized".
#'
#' @usage
#' get_emotion(text, language, all = FALSE, encoding = "UTF-8", token)
#'
#' @param text String with the text.
#' @param language String with the language. Supported languages are 
#' "en" (English), "de" (German) and "es" (Spanish). 
#' @param all Boolean. If TRUE, all the sentiments probabilities
#' are displayed. If FALSE, only the largest probability is displayed.
#' @param encoding Character encoding. Default "UTF-8".
#' @param token The access token.
#'
#' @return
#' A data frame with columns:
#' \enumerate{
#'   \item text Text analyzed.
#'   \item emotion Emotion that the text conveys.
#'   \item probab Probability of the emotion returned.
#' }
#'
#' @author
#' Symanto
#'
#' @examples
#' \dontrun{
#' token <- "Request your API Key: https://developer.symanto.net/"
#' # English:
#' text <- "I am very happy"
#' language <- "en"
#' get_emotion(text, language, TRUE, "UTF-8", token)
#' get_emotion(text, language, FALSE, "UTF-8", token)
#' 
#' # German:
#' text <- "Ich bin sehr zufrieden"
#' language <- "de"
#' get_emotion(text, language, TRUE, "UTF-8", token)
#' 
#' # Spanish:
#' text <- "Estoy muy contento"
#' language <- "es"
#' gs <- get_emotion(text, language, TRUE, "UTF-8", token)
#' gs
#' # Probabilities sum indeed one:
#' sum(gs[grepl("prob", names(gs))])
#' }
#'
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite fromJSON
#' 
#' @export

get_emotion <- function(text, language, all = FALSE, encoding = "UTF-8", token) {
  url <- paste("https://api.symanto.net/api/v1/emotion?all=", tolower(all),
               "&api_key=", token, sep = "")
  
  headers <- c(`Content-Type` = 'application/json')
  
  data <- paste("[{\"id\":\"", text, "\",\"language\":\"", language,  
                "\",\"text\":\"", text, "\"}]", sep = "")
  
  response <- POST(url = url, add_headers(.headers = headers), body = data)
  text_info <- content(response, "text", encoding = encoding)
  json_info <- fromJSON(text_info, flatten = TRUE)
  
  info <- as.data.frame(t(unlist(json_info)), stringsAsFactors = FALSE)
  
  if (all) {
   colnames(info) <- c("text", 
                       paste("emotion", 1:6, sep = ""), 
                       paste("probab", 1:6, sep = "")) 
   info[grep("probab", names(info))] <- round(as.numeric(info[grep("probab", 
                                                                 names(info))]), 3)
  }else{
    colnames(info) <- c("text", "emotion", "probab")
    info$probab <- round(as.numeric(info$probab), 3)
  }
  
  return(info)
}
